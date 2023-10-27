;;; eldev-build.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019-2023 Paul Pogonyshev

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses.

(require 'eldev)


;; To silence byte-compilation warnings on Emacs 24-25.
(defvar byte-compile-log-warning-function)


(defvar eldev--build-targets (make-hash-table :test #'equal))
(defvar eldev--targets-prepared-for nil)

(defvar eldev--build-plan nil)
(defvar eldev--build-results nil)


(defun eldev-build-find-targets (&rest standard-filesets)
  "Return a hash table of all build targets in given filesets.
STANDARD-FILESETS must be a list of names from `eldev-filesets'
variable or `all'.  The hash table might contain targets from
other filesets too.  Caller must not modify contents of the
returned hash table.

Hash table keys are strings, i.e. names of targets.  Entries,
each describing a builder invocations, are alists with at least
the following keys, in no particular order:

  - builder: name (symbol) of the builder that can generate this
    target, see macro `eldev-defbuilder';

  - sources: list of files or other targets that serve as sources
    for this one; each entry must be a file or a real target, not
    virtual;

  - targets: all targets that will be build by this invocation;
    will include at least the target used as the key, but may
    include more (depends on the builder).

Returned hash table doesn't contain cross-target dependency
information past the list of sources.  For this, use function
`eldev-get-target-dependencies'."
  (when (or (null standard-filesets) (memq 'all standard-filesets))
    (setf standard-filesets (mapcar #'car eldev-filesets)))
  (when (memq 'test standard-filesets)
    (eldev--inject-loading-roots 'test))
  (let ((new-filesets (eldev-filter (not (memq it eldev--targets-prepared-for)) standard-filesets)))
    (when new-filesets
      (setf new-filesets (nreverse new-filesets))
      (eldev-trace "Generating build target list for %s" (eldev-message-enumerate "standard fileset" new-filesets))
      ;; FIXME: The point of special-casing `package' builder is to let it know all the
      ;;        sources at once, because its target depends on the number of sources
      ;;        (`.el' if one file, `.tar' otherwise).  This is of course a hack, but it
      ;;        is not clear how to generalize that.  Postponed until some real-world
      ;;        needs come up.
      (let* ((special-cased-builder (assq 'package eldev--builders))
             (reordered-builders    (if special-cased-builder
                                        (append (remq special-cased-builder eldev--builders) (list special-cased-builder))
                                      eldev--builders))
             (potential-new-sources (eldev-find-files (apply #'eldev-standard-filesets new-filesets)))
             (visited-sources       (make-hash-table :test #'equal))
             special-cased-builder-sources
             new-sources)
        (while (setf new-sources (eldev-filter-files (eldev-filter (not (gethash it visited-sources)) potential-new-sources)
                                                     `(:not ,eldev-standard-excludes)))
          (dolist (source new-sources)
            (puthash source t visited-sources))
          (setf potential-new-sources nil)
          (dolist (entry reordered-builders)
            (let* ((builder-name (car entry))
                   (builder      (cdr entry))
                   (source-files (eldev-get builder :source-files)))
              (when source-files
                (setf source-files (eldev-filter-files new-sources source-files)))
              (when (eq entry special-cased-builder)
                (setf source-files (append special-cased-builder-sources source-files)))
              (when source-files
                (if (eq entry special-cased-builder)
                    (setf special-cased-builder-sources source-files)
                  (dolist (invocation (eldev--build-find-builder-invocations source-files builder builder-name))
                    (let ((sources (car invocation))
                          (targets (cdr invocation)))
                      (eldev--build-target-entries targets builder builder-name sources)
                      (setf potential-new-sources (append targets potential-new-sources)))))))))
        (when special-cased-builder-sources
          (let ((builder-name (car special-cased-builder))
                (builder      (cdr special-cased-builder)))
            (dolist (invocation (eldev--build-find-builder-invocations special-cased-builder-sources builder builder-name))
              (eldev--build-target-entries (cdr invocation) builder builder-name (car invocation))))))))
  (unless (gethash ":default" eldev--build-targets)
    (eldev--build-target-entries '(":default") nil nil nil))
  eldev--build-targets)

(defun eldev--build-find-builder-invocations (sources builder builder-name)
  (let ((target-rule (eldev-get builder :targets))
        invocations
        ignored-targets)
    (when (functionp target-rule)
      (setf target-rule (funcall target-rule sources)))
    (dolist (invocation (pcase target-rule
                          ((pred stringp)
                           `((,sources . (,target-rule))))
                          ((pred eldev-string-list-p)
                           `((,sources . ,target-rule)))
                          (`(,(and (or (pred stringp) (pred eldev-string-list-p)) source-suffixes) -> ,(and (pred stringp) target-suffix))
                           (setf source-suffixes (eldev-listify source-suffixes))
                           (let (result)
                             (dolist (source sources)
                               (let ((scan source-suffixes)
                                     found)
                                 (while scan
                                   (let ((source-suffix (pop scan)))
                                     (when (string-suffix-p source-suffix source)
                                       (push `((,source) . (,(eldev-replace-suffix source source-suffix target-suffix))) result)
                                       (setf scan  nil
                                             found t))))
                                 (unless found
                                   (error "Builder `%s': name of source file `%s' doesn't end with %s as `:targets' wants"
                                          builder-name source (eldev-message-enumerate nil source-suffixes nil nil "or")))))
                             (nreverse result)))
                          (_ (error "Invalid `:targets' (or its return value) %S in builder `%s'" target-rule builder-name))))
      ;; Discard builder's wishes that match `eldev-build-ignored-target-fileset'.
      (let* ((targets (cdr invocation))
             (ignored (when eldev-build-ignored-target-fileset
                        (eldev-filter-files targets eldev-build-ignored-target-fileset))))
        (when ignored
          (setf ignored-targets (append ignored-targets ignored)))
        (when (> (length targets) (length ignored))
          (push `(,(car invocation) . ,(eldev-filter (not (member it ignored)) targets)) invocations))))
    (when ignored-targets
      (eldev-trace "%s" (eldev-message-enumerate-files "Ignored potential target%s: %s (%d)" ignored-targets)))
    (nreverse invocations)))

(defun eldev--build-target-entries (targets builder builder-name sources)
  (let ((sorted-targets (sort (copy-sequence targets) #'string<))
        shared-entry
        created-new-entry)
    (dolist (target targets)
      (if (eldev-virtual-target-p target)
          (when builder-name
            (error "Virtual targets (e.g. `%s') must not have builders" target))
        (unless builder-name
          (error "Real targets (e.g. `%s') must have associated builders" target)))
      (let ((entry (gethash target eldev--build-targets)))
        (if shared-entry
            (if (or entry (not created-new-entry))
                (unless (eq entry shared-entry)
                  (error "Unexpected entry %S for target `%s': expected %S" entry target shared-entry))
              (setf entry shared-entry))
          (if entry
              (let ((entry-builder (cdr (assq 'builder entry))))
                (unless (eq entry-builder builder-name)
                  (error "Conflicting builders `%s' and `%s' for target `%s'" entry-builder builder-name target))
                ;; Since both target lists are sorted, we can compare just like this.
                (unless (equal (cdr (assq 'sorted-targets entry)) sorted-targets)
                  (error "Unexpected full target list %s for target `%s': expected %s"
                         (eldev-message-enumerate nil (cdr (assq 'targets entry)) nil nil t) target (eldev-message-enumerate nil targets nil nil t))))
            (setf entry             `((builder        . ,builder-name)
                                      (targets        . ,targets)
                                      (sorted-targets . ,sorted-targets))
                  created-new-entry t))
          (let* ((existing-sources (cdr (assq 'sources entry)))
                 (new-sources      (if existing-sources (eldev-filter (not (member it existing-sources)) sources) sources)))
            (eldev--assq-set 'sources (append existing-sources new-sources) entry))
          (setf shared-entry entry))
        (puthash target entry eldev--build-targets))))
  (when builder
    (eldev--build-collect-targets targets builder)))

(defun eldev--build-collect-targets (targets builder)
  (let ((collect (eldev-get builder :collect)))
    (when (functionp collect)
      (setf collect (funcall collect targets)))
    (dolist (entry (cond ((or (stringp collect) (eldev-string-list-p collect))
                          `((,targets ,collect)))
                         ((eldev-all-p (pcase it (`(,(or (pred stringp) (pred eldev-string-list-p))
                                                    ,(or (pred stringp) (pred eldev-string-list-p)))
                                                  t))
                                       collect)
                          collect)))
      (let ((entry-targets (eldev-listify (car entry))))
        (dolist (virtual-target (reverse (eldev-listify (cadr entry))))
          (unless (eldev-virtual-target-p virtual-target)
            (error "Expected a virtual target, `%s' isn't" virtual-target))
          (eldev--build-target-entries (list virtual-target) nil nil entry-targets)
          (setf entry-targets (list virtual-target)))))))


(defun eldev-get-target-dependencies (target &optional finder)
  "Get TARGET's dependencies.
Usually, this function should be of no interest: custom builders
should normally use only `eldev-set-target-dependencies'.
However, it is allowed to use this function as described below.

By default, list of all dependencies, i.e. across all possible
dependency finders, is returned.  However, if argument FINDER is
specified, returned list includes dependencies only for that
finder.

Returned list's elements look like (TYPE DEPENDENCY [...]).
Following types (symbols) are currently defined:

  - depends-on: DEPENDENCY must always be built (or be
    up-to-date) before TARGET is built;

  - inherits: TARGET inherits all sources (which become
    `depends-on' dependencies) and dependencies of DEPENDENCY;
    this is less strict than `depends-on';

For future compatibility, callers should treat any unknown type
as `depends-on'.  It must not assume anything about contents of
the elements past the two described values.

Returned list must not be modified.  Instead, use function
`eldev-set-target-dependencies' when needed.

This function may only be called while inside the body of a
`eldev-with-target-dependencies' macro."
  ;; This function is for public use, so don't "notice" dependency information if it is
  ;; internal.
  (unless (car eldev--target-dependencies)
    (error "May only be called inside `eldev-with-target-dependencies' macro"))
  (eldev--do-get-target-dependencies target finder))

(defun eldev--do-get-target-dependencies (target &optional finder)
  (let ((dependencies (gethash target (cdr eldev--target-dependencies))))
    (if finder
        (cdr (assq finder dependencies))
      (let (all-dependencies)
        (dolist (entry dependencies)
          (dolist (dependency (cdr entry))
            ;; FIXME: O(N*N), but probably doesn't matter, as we don't
            ;;        expect large lists here.
            (unless (member dependency all-dependencies)
              (push dependency all-dependencies))))
        all-dependencies))))

(defun eldev-set-target-dependencies (target finder dependencies)
  "Set the list of TARGET's DEPENDENCIES according to given FINDER.
FINDER should be a unique symbol, e.g. caller function name.  The
purpose of it is that when function is called for the same target
and finder again, it replaces previous dependencies, but only
those found by the same finder.  This way several finders can
cooperate to find exhaustive dependency list without even knowing
of each other.

See documentation of `eldev-get-target-dependencies' for list's
elements description.

Evaluates to non-nil if FINDER's dependencies are changed, to nil
if they are exactly the same as before (possibly in different
order).  In some cases, even if return value is non-nil, final
dependencies can remain the same because of different finders.

This function may only be called while inside the body of a
`eldev-with-target-dependencies' macro."
  (let ((current-dependencies (eldev-get-target-dependencies dependencies finder)))
    ;; FIXME: This seems to be badly wrong.
    (unless (equal (sort (copy-sequence dependencies)         (lambda (a b) (string< (car a) (car b))))
                   (sort (copy-sequence current-dependencies) (lambda (a b) (string< (car a) (car b)))))
      (eldev--assq-set finder (copy-sequence dependencies) (gethash target (cdr eldev--target-dependencies)) #'equal)
      (setf eldev--target-dependencies-need-saving t))))


(defun eldev--do-targets (parameters)
  (let ((all-targets (if parameters
                         (apply #'eldev-build-find-targets (mapcar #'eldev-validate-standard-fileset parameters))
                       (eldev-build-find-targets 'main))))
    (if (> (hash-table-count all-targets) 0)
        (let ((sources (make-hash-table :test #'equal))
              toplevel)
          (maphash (lambda (_target entry)
                     (dolist (source (cdr (assq 'sources entry)))
                       (puthash source t sources)))
                   all-targets)
          (maphash (lambda (target _)
                     (unless (gethash target sources)
                       (push target toplevel)))
                   all-targets)
          (eldev-with-target-dependencies
            ;; This is done only to detect cyclic dependencies.
            (let ((eldev--build-plan (make-hash-table :test #'equal)))
              (maphash (lambda (target _entry) (eldev--build-add-to-plan target all-targets)) all-targets))
            (eldev--print-target-level toplevel 0 all-targets (make-hash-table :test #'eq))))
      (eldev-print "There are no targets for %s" (if (> (length parameters) 1) "these filesets" "this fileset")))))

(defun eldev--print-target-level (level-targets level all-targets printed-target-entries)
  (when level-targets
    (let ((indentation   (make-string (* level 4) ? ))
          (level-targets (copy-sequence level-targets))
          level-sources)
      (setf level-targets (sort level-targets #'string<))
      (dolist (prioritized-target '(":package" ":default"))
        (when (member prioritized-target level-targets)
          (setf level-targets `(,prioritized-target ,@(delete prioritized-target level-targets)))))
      (dolist (target level-targets)
        (let* ((entry       (gethash target all-targets))
               (repeated    (gethash entry printed-target-entries))
               (builder     (cdr (assq 'builder entry)))
               (sources     (cdr (assq 'sources entry)))
               (target-name (eldev-colorize target (cond ((eldev-virtual-target-p target) 'section) (sources 'name)) target)))
          (cond ((and (null sources) (eq eldev-targets-list-sources 'concise))
                 (push target level-sources))
                ((or sources eldev-targets-list-sources (eldev-virtual-target-p target))
                 (cond ((and repeated builder)
                        (eldev-output "%s%s  [%s]" indentation target-name
                                      (eldev-colorize (if (equal repeated target) "repeated, see above" (eldev-format-message "repeated, see `%s' above" repeated)) 'details)))
                       ((and builder (eldev-unless-quiet t))
                        (eldev-output "%s%s  [%s]" indentation target-name (or (eldev-get (cdr (assq builder eldev--builders)) :short-name) builder)))
                       (t
                        (eldev-output "%s%s" indentation target-name)))
                 (unless repeated
                   (puthash entry target printed-target-entries)
                   (eldev--print-target-level sources (1+ level) all-targets printed-target-entries)
                   (when eldev-targets-list-dependencies
                     (dolist (dependency (sort (copy-sequence (eldev-get-target-dependencies target)) (lambda (a b) (string< (car a) (car b)))))
                       (eldev-output "%s    %s %s" indentation
                                     (eldev-colorize (eldev-pcase-exhaustive (car dependency)
                                                       (`depends-on "[dep]")
                                                       (`inherits   "[inh]"))
                                                     'details)
                                     (cadr dependency)))))))))
      (when level-sources
        (setf level-sources (nreverse level-sources))
        (let ((num (length level-sources)))
          (when (> num 3)
            (setf (nthcdr 3 level-sources) nil))
          (eldev-output "%s%s%s" indentation (mapconcat #'identity level-sources (eldev-colorize ", " 'details))
                        (if (> num 3) (eldev-colorize (eldev-format-message " + %d more" (- num 3)) 'details) "")))))))


(defun eldev--do-build (parameters &optional dont-touch-packages)
  ;; When building, project loading mode is ignored.  The reason is that building itself
  ;; can involve compiling or packaging.
  (run-hooks 'eldev-build-system-hook)
  (unless dont-touch-packages
    (let ((eldev-project-loading-mode 'as-is))
      (when (memq 'test eldev-build-sets)
        (eldev--inject-loading-roots 'test))
      (eldev-load-project-dependencies 'build nil t)))
  (let ((all-targets (apply #'eldev-build-find-targets (or eldev-build-sets '(main))))
        target-list
        target-fileset
        virtual-targets)
    (if parameters
        (dolist (parameter parameters)
          (push parameter (if (eldev-virtual-target-p parameter) virtual-targets target-fileset)))
      (setf virtual-targets '(":default")))
    (dolist (target (setf virtual-targets (nreverse virtual-targets)))
      (unless (gethash target all-targets)
        (signal 'eldev-error `("Unknown virtual target `%s'" ,target))))
    (when target-fileset
      (maphash (lambda (target _entry) (push target target-list)) all-targets)
      (setf target-list (eldev-filter-files target-list (nreverse target-fileset))))
    (eldev-with-target-dependencies
      (let ((eldev--build-plan    (make-hash-table :test #'equal))
            (eldev--build-results (make-hash-table :test #'equal))
            build-sequence
            anything-failed)
        ;; We create a plan before even starting to build everything, so that cyclic
        ;; dependencies can be detected earlier.
        (dolist (target (nconc virtual-targets target-list))
          (eldev--build-add-to-plan target all-targets))
        (maphash (lambda (target order) (when (numberp order) (push (cons target order) build-sequence))) eldev--build-plan)
        ;; FIXME: Really could add nicety by sorting this more intelligently so that
        ;;        e.g. targets go alphabetically and the same builder is used sequentially
        ;;        where this doesn't break dependency ordering.
        (when build-sequence
          (setf build-sequence (sort build-sequence (lambda (a b) (< (cdr a) (cdr b)))))
          (eldev-trace "Building plan: %s" (eldev-message-enumerate nil build-sequence #'car nil t))
          (eldev-autoinstalling-implicit-dependencies t
            (dolist (entry build-sequence)
              (if eldev-build-keep-going
                  ;; Ignore errors here: they will have been reported in `eldev-build-target'
                  ;; already.
                  (condition-case nil
                      (eldev-build-target (car entry))
                    (eldev-build-abort-branch))
                (eldev-build-target (car entry))))))
        (when (= (hash-table-count eldev--build-results) 0)
          (eldev-print "Nothing to do"))
        (maphash (lambda (_target status) (unless (eq status 'built) (setf anything-failed t))) eldev--build-results)
        ;; See e.g. test `eldev-loading-modes-3'.  Basically, once we load the main
        ;; project's package (in `as-is' mode), it would never get reloaded later, which
        ;; would be problematic if this build process was not the main invocation,
        ;; e.g. was only invoked from `autoloads' plugin.  For now, simply always "unload"
        ;; the main package: in the main invocation simply nothing would happen afterwards
        ;; anyway.  The only exception is when compiling on-demand.
        (unless dont-touch-packages
          (eldev--unload-package (package-desc-name (eldev-package-descriptor))))
        (when anything-failed
          (signal 'eldev-error `("Build failed")))))))


(defun eldev--need-to-build (target source)
  "Determine if we need to build a non-virtual TARGET because of SOURCE."
  (unless (eldev-virtual-target-p source)
    (or (eq eldev-build-infinitely-new t)
        (member source eldev-build-infinitely-new)
        (file-newer-than-file-p source target))))

(defun eldev--need-to-build-full (target dependency all-targets &optional dependency-stack)
  (let ((cycle (member dependency dependency-stack)))
    (when cycle
      (signal 'eldev-error `("%s form a dependency cycle" ,(eldev-message-enumerate "Target" (reverse dependency-stack))))))
  (let ((entry (gethash dependency all-targets)))
    (when entry
      (push dependency dependency-stack)
      (or (eldev-any-p (eldev--need-to-build target it) (cdr (assq 'sources entry)))
          (eldev-any-p (eldev-pcase-exhaustive (car it)
                         (`depends-on (eldev--need-to-build target (cadr it)))
                         (`inherits   (eldev--need-to-build-full target (cadr it) all-targets dependency-stack)))
                       (eldev-get-target-dependencies dependency))))))

(defun eldev--build-add-to-plan (target all-targets &optional dependency-stack)
  (let ((already-planned (gethash target eldev--build-plan)))
    (when (eq already-planned 'side-effect)
      (puthash target 'planned-side-effect eldev--build-plan))
    (or already-planned
        (let ((cycle (member target dependency-stack)))
          (when cycle
            (signal 'eldev-error `("%s form a dependency cycle" ,(eldev-message-enumerate "Target" (reverse dependency-stack))))))
        (let ((entry         (gethash target all-targets))
              (virtual       (eldev-virtual-target-p target))
              (need-to-build (or (eq eldev-build-force-rebuilding t)
                                 (member target eldev-build-force-rebuilding))))
          (when entry
            (push target dependency-stack)
            (dolist (source (cdr (assq 'sources entry)))
              (when (or (eldev--build-add-to-plan source all-targets dependency-stack)
                        (unless virtual
                          (eldev--need-to-build target source)))
                (setf need-to-build t)))
            (dolist (dependency-entry (eldev-get-target-dependencies target))
              (let ((dependency (cadr dependency-entry)))
                (eldev-pcase-exhaustive (car dependency-entry)
                  (`depends-on (when (or (eldev--build-add-to-plan dependency all-targets dependency-stack)
                                         (unless virtual
                                           (eldev--need-to-build target dependency)))
                                 (setf need-to-build t)))
                  (`inherits   (when (eldev--need-to-build-full target dependency all-targets dependency-stack)
                                 (setf need-to-build t))))))
            ;; For the main target it will be overwritten.
            (dolist (other-target (cdr (assq 'targets entry)))
              (puthash other-target 'side-effect eldev--build-plan))
            (puthash target (when need-to-build (hash-table-count eldev--build-plan)) eldev--build-plan))))))

(defun eldev-build-target-status (target)
  "Returns TARGET building result.
Return value is one of the following symbols:

  - planned
  - building
  - built
  - failed
  - not-planned"
  (or (gethash target eldev--build-results)
      ;; Both numbers and symbol `planned-side-effect' mean that the target is planned for
      ;; building.
      (if (memq (gethash target eldev--build-plan) '(nil side-effect)) 'not-planned 'planned)))

(defun eldev-build-target (target)
  "Build given TARGET.
The TARGET must be previously planned for building: it is not
possible to build arbitrary targets this way."
  (eldev-pcase-exhaustive (eldev-build-target-status target)
    (`building    (error "Trying to build `%s' recursively" target))
    (`built       (eldev-trace "Not trying to build `%s' again" target))
    (`failed      (error "Building `%s' failed earlier" target))
    (`not-planned (error "Building `%s' was never planned" target))
    (`planned
     ;; Reset `default-directory', because it can have been changed e.g. when called from
     ;; inside byte-compilation.
     (let* ((default-directory eldev-project-dir)
            (entry             (gethash target eldev--build-targets))
            (builder-name      (cdr (assq 'builder entry)))
            (targets           (cdr (assq 'targets entry)))
            succeeded)
       (puthash target 'building eldev--build-results)
       (unwind-protect
           (if builder-name
               (let* ((builder       (cdr (assq builder-name eldev--builders)))
                      (builder-type  (or (eldev-get builder :type) 'one-to-one))
                      (sources       (cdr (assq 'sources entry)))
                      ;; Normally such things will not be in the plan to begin with, but
                      ;; also take into account situations where builders don't update
                      ;; their target for whatever reason, e.g. when they can detect that
                      ;; it hasn't changed.
                      (need-to-build (or (eq eldev-build-force-rebuilding t)
                                         (member target eldev-build-force-rebuilding)
                                         (eldev--need-to-build-full target target eldev--build-targets))))
                 (if need-to-build
                     (let* ((eldev-build-current-targets targets)
                            (source-argument             (if (memq builder-type '(one-to-one one-to-many)) (car sources) sources))
                            (target-argument             (if (memq builder-type '(one-to-one many-to-one)) (car targets) targets)))
                       (eldev-unless-quiet
                         (let* ((short-name    (or (eldev-get builder :short-name) builder-name))
                                (message       (or (eldev-get builder :message) 'sources))
                                (source-string (if (cddr sources)
                                                   (if (eldev-when-verbose t)
                                                       (eldev-message-enumerate nil sources (lambda (source) (eldev-colorize source 'name)) t)
                                                     (eldev-format-message "%d files" (length sources)))
                                                 (eldev-colorize (car sources) 'name)))
                                (target-string (if (cddr targets)
                                                   (if (eldev-when-verbose t)
                                                       (eldev-message-enumerate nil targets (lambda (target) (eldev-colorize target 'name)) t)
                                                     (eldev-format-message "%d files" (length targets)))
                                                 (eldev-colorize (car targets) 'name))))
                           (pcase message
                             ((or `source `sources)
                              (eldev-output "%-8s %s" short-name source-string))
                             ((or `target `targets)
                              (eldev-output "%-8s -> %s" short-name target-string))
                             ((or `source-and-target `sources-and-target `source-and-targets `sources-and-targets)
                              (eldev-output "%-8s %s -> %s" short-name source-string target-string))
                             (_
                              (eldev-output "%-8s %s" short-name (funcall message sources targets))))))
                       (unless eldev-build-dry-run-mode
                         (if eldev-build-keep-going
                             (condition-case error
                                 (eldev-backtrace-notch 'eldev
                                   (if (eldev-get builder :profiling-self)
                                       (funcall builder source-argument target-argument)
                                     (eldev-profile-body
                                       (funcall builder source-argument target-argument))))
                               (eldev-build-abort-branch
                                (signal (car error) (cdr error)))
                               (eldev-error
                                (eldev-error "While building `%s': %s" target (apply #'eldev-format-message (cdr error)))
                                (signal 'eldev-build-abort-branch nil))
                               (error
                                (eldev-error "While building `%s': %s" target (error-message-string error))
                                (signal 'eldev-build-abort-branch nil)))
                           (funcall builder source-argument target-argument)))
                       (setf succeeded t))
                   (setf succeeded t)
                   (eldev-verbose "Not building target `%s': it is up-to-date" target)))
             (setf succeeded t)
             (eldev-verbose "Done building “sources” for virtual target `%s'" target))
         (dolist (target targets)
           (puthash target (if succeeded 'built 'failed) eldev--build-results)))))))


(defconst eldev--have-byte-compile-warning-function (boundp 'byte-compile-log-warning-function))

(defvar eldev--recursive-byte-compilation   nil)
(defvar eldev--recursive-elevated-errors-in nil)

(defun eldev--byte-compile-.el (source target)
  (eval-and-compile (require 'bytecomp))
  (let* ((recursive                           eldev--recursive-byte-compilation)
         (eldev--recursive-byte-compilation   t)
         (eldev--recursive-elevated-errors-in (if recursive eldev--recursive-elevated-errors-in (list nil)))
         (load-prefer-newer                   t)
         ;; When called recursively, let `byte-compile-file' determine this.
         (skip-byte-compilation             (unless recursive
                                              (with-temp-buffer
                                                (insert-file-contents source)
                                                ;; Older versions don't understand `no-mode'.
                                                (hack-local-variables (when (>= emacs-major-version 26) 'no-mode))))))
    ;; Don't do anything with `no-byte-compile' files (not even load) unless called
    ;; recursively.  Otherwise we might e.g. attempt loading `define-package' and fail.
    (unless skip-byte-compilation
      (eldev-verbose (if recursive "Byte-compiling file `%s' early as `require'd from another file..." "Byte-compiling file `%s'...")
                     source)
      (eldev-advised ('load :before
                            (unless recursive
                              (lambda (file &rest _ignored)
                                (eldev--trigger-early-byte-compilation file))))
        ;; The advice for `load' is, unfortunately, not enough since `require' calls
        ;; C-level function directly, bypassing advice machinery.
        (eldev-advised ('require :before
                                 (unless recursive
                                   (lambda (feature &optional filename &rest _ignored)
                                     (eldev--trigger-early-byte-compilation (or filename (eldev--find-feature-provider feature))))))
          (let* (result
                 (failed-source
                  (catch 'eldev--byte-compilation-failed
                    ;; Must be within the `catch', because it can trigger byte-compilation
                    ;; of other targets.
                    (when eldev-build-load-before-byte-compiling
                      (eldev-trace "Loading file `%s' before byte-compiling it..." source)
                      (eldev-profile-body
                        (load source nil t t)))
                    (setf result (if skip-byte-compilation
                                     'no-byte-compile
                                   ;; We are not using `byte-compile-error-on-warn' because that "helpfully"
                                   ;; aborts compilation after the very first warning (since forever and at
                                   ;; least till June 2022 Emacs snapshots).  Makes it tedious to hunt them
                                   ;; down if you have twenty.
                                   ;;
                                   ;; Changes will be done several times in case of a recursive compilation,
                                   ;; but this doesn't really matter.
                                   (let* ((original-warning-function         (when eldev--have-byte-compile-warning-function byte-compile-log-warning-function))
                                          (byte-compile-log-warning-function (when eldev--have-byte-compile-warning-function
                                                                               (lambda (string &optional position fill level &rest etc)
                                                                                 (when (and eldev-build-treat-warnings-as-errors (eq level :warning))
                                                                                   (push source (car eldev--recursive-elevated-errors-in))
                                                                                   (setf level :error))
                                                                                 (unless (and eldev-build-suppress-warnings (eq level :warning))
                                                                                   (apply original-warning-function string position fill level etc))))))
                                     (eldev-advised (#'byte-compile-log-warning :around
                                                                                ;; Not available on old Emacs versions.  Basically
                                                                                ;; just duplicating the function above.
                                                                                (unless eldev--have-byte-compile-warning-function
                                                                                  (lambda (original string &optional fill level &rest etc)
                                                                                    (when (and eldev-build-treat-warnings-as-errors (eq level :warning))
                                                                                      (push source (car eldev--recursive-elevated-errors-in))
                                                                                      (setf level :error))
                                                                                    (unless (and eldev-build-suppress-warnings (eq level :warning))
                                                                                      (apply original string fill level etc)))))
                                       (eldev--silence-file-writing-message (expand-file-name target)
                                         (eldev-backtrace-notch 'eldev
                                           (eldev-profile-body
                                             (and (byte-compile-file source)
                                                  (if (memq source (car eldev--recursive-elevated-errors-in))
                                                      (progn (delete-file target) nil)
                                                    t)))))))))
                    (cond ((eq result 'no-byte-compile)
                           (eldev-verbose "Cancelled byte-compilation of `%s': it has `no-byte-compile' local variable" source)
                           nil)
                          (result
                           ;; Keep in sync with `eldev--do-byte-compile-.el-on-demand'.
                           (when eldev-build-load-before-byte-compiling
                             ;; Load ourselves, since `byte-compile-file' calls `load'
                             ;; without `nomessage' parameter.  Byte-compiled file should
                             ;; be loaded to replace its slower source we loaded above.
                             (eldev-trace "Loading the byte-compiled file `%s'..." target)
                             (eldev-profile-body
                               (load target nil t t)))
                           nil)
                          (t
                           source)))))
            (when failed-source
              (if recursive
                  ;; Normal errors would get caught inside Emacs byte-compilation code, so
                  ;; we use tag throwing/catching instead.
                  (throw 'eldev--byte-compilation-failed failed-source)
                (signal 'eldev-build-failed `("Failed to byte-compile `%s'" ,failed-source))))
            (unless (eq result 'no-byte-compile)
              (let ((history-entry (or (assoc (expand-file-name target eldev-project-dir) load-history)
                                       (assoc (expand-file-name source eldev-project-dir) load-history)))
                    inherited-targets
                    provided-feature)
                (unless history-entry
                  ;; This would not be needed if we could determine dependencies in any
                  ;; other way.  At least we load already byte-compiled file.
                  (eldev-trace "Loading file `%s' in order to find dependencies..." target)
                  (load (expand-file-name target eldev-project-dir) nil t t)
                  (setf history-entry (assoc (expand-file-name target eldev-project-dir) load-history)))
                (dolist (entry (cdr history-entry))
                  (pcase entry
                    (`(require . ,feature)
                     (unless provided-feature
                       (let ((provided-by (eldev--find-feature-provider feature)))
                         (when (stringp provided-by)
                           (push `(,feature . ,provided-by) inherited-targets)))))
                    (`(provide . ,feature)
                     ;; See e.g. `eldev-test-compile-circular-requires-1': after `provide' form
                     ;; ignore all remaining `require's for dependency purposes.  Also remove
                     ;; self-dependency entry if it has been added.
                     (puthash feature source eldev--feature-providers)
                     (setf inherited-targets (delete `(,feature . ,source) inherited-targets)
                           provided-feature  t))))
                (eldev-set-target-dependencies target 'eldev-builder-byte-compile-.el
                                               (mapcar (lambda (entry) `(inherits ,(eldev-replace-suffix (cdr entry) ".el" ".elc")))
                                                       inherited-targets))))))))))

(defvar eldev--byte-compile-.el-on-demand-recursing-for nil)

(defun eldev--do-byte-compile-.el-on-demand (source quiet)
  (unless (member source eldev--byte-compile-.el-on-demand-recursing-for)
    (let ((eldev--byte-compile-.el-on-demand-recursing-for eldev--byte-compile-.el-on-demand-recursing-for)
          (target (eldev-replace-suffix source ".el" ".elc")))
      (push source eldev--byte-compile-.el-on-demand-recursing-for)
      (if (and eldev--build-plan (eq (eldev-build-target-status target) 'planned))
          (eldev-build-target target)
        ;; This initializes build system even if it is already up, but hasn't planned to
        ;; build the target previously.  Happens in `compiled-on-demand' loading mode with
        ;; nested `require', i.e. is a pretty common situation.  Maybe something could be
        ;; optimized to avoid this.
        ;;
        ;; To be consistent with how loading in other modes work, make compilation very
        ;; quiet here.  Warnings and errors will still be shown, though, possibly in the
        ;; middle of actual project's output — but that is an unavoidable side-effect of
        ;; on-demand compilation.
        (let ((eldev-verbosity-level (if quiet 'quiet eldev-verbosity-level)))
          (eldev--do-build (list target) t)
          ;; Keep in sync with `eldev--byte-compile-.el'.  Force-load the byte-compiled
          ;; file now, to replace raw Lisp functions with faster byte-compiled versions.
          (unless eldev-build-load-before-byte-compiling
            (load target nil t t)))))))

(defun eldev--trigger-early-byte-compilation (file)
  ;; See `eldev--find-feature-provider': `file' might not really be a file here.
  (when (stringp file)
    ;; Reset `default-directory', because it can have been changed e.g. when called from
    ;; inside byte-compilation.
    (let ((default-directory eldev-project-dir))
      (setf file (file-relative-name file eldev-project-dir))
      (unless (or (eldev-external-or-absolute-filename file)
                  (not (eldev-external-or-absolute-filename (file-relative-name file eldev-cache-dir))))
        (setf file (eldev-replace-suffix file ".el" ".elc"))
        (when (eq (eldev-build-target-status file) 'planned)
          (eldev-build-target file))))))


(defun eldev--do-package (sources targets)
  (unless sources
    (signal 'eldev-build-failed `("No sources for packaging")))
  (let* ((package           (eldev-package-descriptor))
         (pretended-version (or eldev-package-forced-version (package-desc-version package)))
         (package-target    (car targets))
         (entry-target      (cadr targets)))
    ;; Hardly anyone would make an entry without the package, but let's check.
    (when (memq (eldev-build-target-status package-target) '(planned building))
      (if (cdr sources)
          ;; Building a tarball.
          (let* ((name-version     (eldev--package-name-version))
                 (name-version-dir (file-name-as-directory name-version))
                 (working-dir      (make-temp-file "eldev-packaging-" t))
                 (descriptor-file  (eldev-package-descriptor-file-name))
                 temporary-descriptor
                 files-to-tar)
            (condition-case nil
                (make-symbolic-link eldev-project-dir (expand-file-name name-version working-dir))
              (file-error
               (let ((working-dir-pkg (expand-file-name name-version working-dir)))
                 (copy-directory eldev-project-dir working-dir-pkg t t t))))
            (make-directory (file-name-directory package-target) t)
            (unless (file-exists-p descriptor-file)
              (with-temp-file (expand-file-name descriptor-file (expand-file-name name-version-dir working-dir))
                (insert "; -*- no-byte-compile: t -*-\n")
                (pp `(define-package ,(symbol-name (package-desc-name package)) ,(package-version-join pretended-version)
                       ,(package-desc-summary package)
                       ,(eldev-macroexp-quote (mapcar (lambda (requirement)
                                                        `(,(car requirement) ,(package-version-join (cadr requirement))))
                                                      (package-desc-reqs package)))
                       ,@(apply #'nconc (mapcar (lambda (extra)
                                                  `(,(eldev-macroexp-quote (car extra)) ,(eldev-macroexp-quote (cdr extra))))
                                                (package-desc-extras package))))
                    (current-buffer)))
              (setf temporary-descriptor t))
            (when (or temporary-descriptor (not (member descriptor-file sources)))
              (push (concat name-version-dir descriptor-file) files-to-tar))
            (dolist (source sources)
              (push (concat name-version-dir source) files-to-tar))
            ;; Note that `file-name-as-directory' is important on older Emacs versions,
            ;; otherwise `tar' will be executed from `/tmp'.
            (let ((default-directory (file-name-as-directory working-dir)))
              (eldev-verbose "%s" (eldev-message-enumerate-files "Packaging the following file%s: %s (%d)" sources))
              (eldev-call-process (eldev-tar-executable) `("-cf" ,(expand-file-name package-target eldev-project-dir) ,@(nreverse files-to-tar))
                :trace-command-line "Full command line to create package tarball"
                :forward-output t
                (when (/= exit-code 0)
                  (signal 'eldev-build-failed `("Failed to create package tarball `%s'" ,package-target)))))
            ;; Note that if packaging fails, `working-dir' and `descriptor-file' are not
            ;; deleted.  This is intentional.
            (when temporary-descriptor
              (delete-file descriptor-file))
            (delete-directory working-dir t))
        ;; Single-file package.
        ;; FIXME: Validate sanity.
        (make-directory (file-name-directory package-target) t)
        (copy-file (car sources) package-target 'overwrite))
      (setf eldev--package-target-generated t))
    (when (memq (eldev-build-target-status entry-target) '(planned building))
      (eldev-verbose "Generating package archive entry `%s'" entry-target)
      (with-temp-file entry-target
        (prin1 `(,(package-desc-name package)
                 . [,pretended-version ,(package-desc-reqs package) ,(package-desc-summary package)
                    ,(if (cdr sources) 'tar 'single) ,(package-desc-extras package)])
               (current-buffer))
        (insert "\n")))))


(provide 'eldev-build)

;;; eldev-build.el ends here
