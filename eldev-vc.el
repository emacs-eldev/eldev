;;; eldev-vc.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2020-2025 Paul Pogonyshev

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

;;; Code:

(require 'eldev)
(require 'subr-x)
(require 'vc)
(require 'vc-dir)


(defconst eldev-vc-supported-backends '(Git Hg)
  "List of VC backends supported by Eldev.
While Eldev uses Emacs' built-in package `vc', it supports only a
few VCS.  Currently, these are only `Git' and `Hg'.")


(defun eldev-vc-root-dir ()
  "Same as `vc-root-dir', needed for compatibility on Emacs 24."
  (if (fboundp 'vc-root-dir)
      (vc-root-dir)
    (let ((backend (vc-deduce-backend)))
      (if backend
          (condition-case err
              (vc-call-backend backend 'root default-directory)
            (vc-not-supported
             (unless (eq (cadr err) 'root)
               (signal (car err) (cdr err)))
             nil))))))


(defun eldev-vc-executable (backend &optional not-required)
  "Return VC executable for given BACKEND.
Specifying a non-supported backend will either return nil or
signal an error, depending on flag NOT-REQUIRED.

Since 0.8."
  (pcase backend
    (`Git (eldev-git-executable not-required))
    (`Hg  (eldev-hg-executable  not-required))
    (`SVN (eldev-svn-executable not-required))
    (_    (unless not-required
            (error "Unsupported VC backend `%s'" backend)))))

(defun eldev-vc-full-name (backend)
  "Return full VC name for given BACKEND.
Since 0.8."
  (pcase backend
    (`Git "Git")
    (`Hg  "Mercurial")
    (`SVN "Subversion")
    (_    "?")))


(defmacro eldev-with-vc (project-dir &rest body)
  "Execute `vc' code in BODY in given PROJECT-DIR.
If PROJECT-DIR is nil, `eldev-project-dir is used instead.  BODY
has access to locally bound variable `backend'.

Since 0.8."
  (declare (indent 1) (debug (sexp body)))
  `(let* ((default-directory (or ,project-dir eldev-project-dir))
          (backend           (ignore-errors (vc-responsible-backend default-directory))))
     ;; `vc-deduce-backend' insists on specific modes; easier to just fake it.
     (eldev-advised (#'vc-deduce-backend :override (lambda (&rest _ignored) backend))
       ,@body)))

(defmacro eldev-with-vc-buffer (project-dir &rest body)
  "Execute `vc' code in BODY in given PROJECT-DIR.
If PROJECT-DIR is nil, `eldev-project-dir' is used instead.  BODY
has access to locally bound variable `backend'.  Current buffer
is a VC status buffer with the specified directory.

Since 0.8."
  (declare (indent 1) (debug (sexp body)))
  (let ((buffer (make-symbol "$buffer")))
    `(let* ((default-directory (or ,project-dir eldev-project-dir))
            (backend           (ignore-errors (vc-responsible-backend default-directory)))
            (,buffer           (eldev--vc-set-up-buffer backend)))
       (unwind-protect
           (with-current-buffer ,buffer
             ,@body)
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             ;; Else it will ask pointless questions.
             (vc-dir-kill-dir-status-process))
           (kill-buffer ,buffer))))))

(defvar use-vc-backend)
(defun eldev--vc-set-up-buffer (backend)
  (let ((buffer (vc-dir-prepare-status-buffer "*eldev-vc*" default-directory backend t)))
    (with-current-buffer buffer
      ;; Yeah, need to go into sort-of-undocumented-internals, as always.
      (let ((use-vc-backend backend))
        (vc-dir-mode))
      (eldev-vc-synchronize-dir))
    buffer))

(defun eldev-vc-synchronize-dir ()
  ;; Don't you love Elisp?  Joining a process?  Never hard of 'em.  Find a better way.
  (while (vc-dir-busy)
    (sleep-for 0.01)))


(defun eldev-vc-detect (&optional project-dir)
  "Detect VCS used in given PROJECT-DIR.
If `vc' detects a backend not supported by Eldev, return value is
nil.  Return value is also nil if PROJECT-DIR is not the root of
a VCS checkout.

Since 0.8."
  (eldev-with-vc project-dir
    ;; Even if built-in `vc' can detect some VCS, we only support a few selected ones.
    ;; This makes it easier to test and give some guarantees.
    (when (and (memq backend '(Git Hg SVN)) (let ((root (eldev-vc-root-dir))) (and root (file-equal-p root default-directory))))
      backend)))


(defun eldev-vc-commit-id (&optional short project-dir)
  "Determine the identifier of the current commit in given PROJECT-DIR.
Return value is a string.  Unlike `vc-working-revision', this
doesn't accept any `file' argument, but instead works for the
whole checkout.

Since 1.2."
  (eldev-with-vc project-dir
    (eldev-call-process (eldev-vc-executable backend)
        (eldev-pcase-exhaustive backend
          (`Git `("rev-parse" ,@(when short '("--short")) "HEAD"))
          (`Hg  `("id" "--id"))
          (`SVN `("info" "--show-item" "last-changed-revision")))
      :destination  '(t nil)
      :discard-ansi t
      :die-on-error t
      (string-trim (buffer-string)))))

(defun eldev-vc-branch-name (&optional project-dir)
  "Determine the current VCS branch in given PROJECT-DIR.
Return value is a string.  For Git and Mercurial it is
straightforward.  For Subversion it is instead the part of
relative URL without leading \"^/\" and, if possible, beginning
with word \"trunk\", \"branches\" or \"tags\".  If there is no
such component in the relative URL, it is returned full.

Since 1.2."
  (eldev-with-vc project-dir
    (eldev-call-process (eldev-vc-executable backend)
        (eldev-pcase-exhaustive backend
          ;; Cryptic way to get current branch name.  `branch --show-current' is too
          ;; recent.
          (`Git `("rev-parse" "--abbrev-ref" "HEAD"))
          (`Hg  `("branch"))
          (`SVN `("info" "--show-item" "relative-url")))
      :destination  '(t nil)
      :discard-ansi t
      :die-on-error t
      (let ((branch (string-trim (buffer-string))))
        (when (eq backend 'SVN)
          (setf branch (replace-regexp-in-string (rx bol "^/") "" branch))
          ;; Relative URL can be e.g. `^/myfaces/core/branches/1.2.x/api', because
          ;; Subversion has a pretty insane notion of a "branch".
          (setf branch (replace-regexp-in-string (rx bol (0+ any) "/" (group (| "trunk" "branches" "tags") (| "/" eos))) "\\1" branch)))
        branch))))

(defun eldev-vc-create-tag (name &optional project-dir)
  "Create a VCS tag with given NAME for PROJECT-DIR.
Since 1.2."
  ;; `vc-create-tag' sucks and is not even implemented for Mercurial.  So, we roll our own.
  (eldev-with-vc project-dir
    (eldev-call-process (eldev-vc-executable backend)
        (eldev-pcase-exhaustive backend
          ;; Cryptic way to get current branch name.  `branch --show-current' is too
          ;; recent.
          (`Git `("tag" ,name))
          (`Hg  `("tag" ,name))
          (`SVN (error "Tagging with Subversion is not currently implemented")))
      (unless (= exit-code 0)
        (error "%s" (string-trim (buffer-string)))))))



;; Working with VC repositories to fetch packages.

(defvar eldev-vc-default-release-tag-regexp (rx bos (| "" "v") (group digit (0+ any)) eos))

(defvar eldev--vc-repository-packages nil
  "Alist of `(NAME . PKG-DESCRIPTOR).")


(defun eldev--vc-repository-package (name)
  "Return VC-fetched package with given NAME.
Will be nil if no such repository was registered or
`eldev--vc-fetch-repository' has not been called for it yet."
  (cdr (assq name eldev--vc-repository-packages)))

(defun eldev--vc-fetch-repository (repository &optional update release-tag-regexp)
  (let ((url    (plist-get (cdr repository) :git))
        (commit (plist-get (cdr repository) :commit))
        (dir    (eldev--vc-clone-dir repository))
        reuse-existing)
    ;; Git ignores `--depth' for local clones, but we need a consistent behavior, if only for tests.
    (when (file-name-absolute-p url)
      (setf url (format "file://%s" url)))
    (when (file-exists-p (expand-file-name ".git/config" dir))
      (let ((default-directory dir))
        (if (string= url (eldev-call-process (eldev-git-executable) '("config" "--get" "remote.origin.url")
                           :destination  '(t nil)
                           :discard-ansi t
                           :die-on-error t
                           (string-trim (buffer-string))))
            (setf reuse-existing t)
          (eldev-trace "Discarding existing Git clone at `%s': wrong remote URL" dir)
          (ignore-errors (delete-directory dir t)))))
    (if reuse-existing
        (eldev-trace "Reusing existing Git clone of `%s'..." dir)
      (eldev-verbose "Cloning Git repository `%s'..." url)
      (eldev-call-process (eldev-git-executable) `("clone" ,url "--single-branch" "--depth=1" "--no-checkout" ,dir)
        :die-on-error t))
    (let ((default-directory dir)
          tag+version)
      (if (and reuse-existing (not update))
          (setf tag+version (eldev--vc-current-commit-release-tag release-tag-regexp))
        (when (and eldev-prefer-stable-archives (null commit))
          (setf tag+version  (eldev--vc-pick-release-tag release-tag-regexp)))
        ;; Without `--tags' below, `eldev--vc-current-commit-release-tag' wouldn't be able
        ;; to do its job later, because the commit in the clone wouldn't be tagged.
        (let ((to-fetch (or commit (car tag+version) "HEAD")))
          (if update
              (eldev-verbose "Fetching updates from `%s', commit `%s'..." url to-fetch)
            (eldev-trace "Initially switching to commit `%s'..." to-fetch))
          (eldev-call-process (eldev-git-executable) `("fetch" "--depth=1" "--tags" "origin" ,to-fetch)
            :die-on-error t))
        (eldev-call-process (eldev-git-executable) `("checkout" "FETCH_HEAD")
          :die-on-error t)
        (when commit
          (setf tag+version (eldev--vc-pick-release-tag release-tag-regexp))))
      (let ((package (eldev-package-descriptor dir)))
        (unless (eq (package-desc-name package) (car repository))
          (signal 'eldev-error `(:hint ("This package was expected due to a call to `eldev-use-vc-repository'
However, the repository contains package `%s' instead" ,(package-desc-name package))
                                 "VC repository `%s' failed to provide package `%s'" ,(eldev--vc-repository-name repository) ,(car repository))))
        (if tag+version
            (setf (package-desc-version package) (cdr tag+version))
          (eldev-call-process (eldev-git-executable) `("--no-pager" "log" "-1" "--no-color" "--format=%cI" "--no-patch")
            :destination  '(t nil)
            :discard-ansi t
            :die-on-error t
            (let ((date-string (string-trim (buffer-string))))
              ;; Don't match the whole, but just a bit of assertion here.
              (when (string-match (rx bos (group (= 4 num)) "-" (group (= 2 num)) "-" (group (= 2 num)) "T" (group (= 2 num)) ":" (group (= 2 num))) date-string)
                (setf (package-desc-version package)
                      (append (package-desc-version package) `(,(string-to-number (format "%s%s%s" (match-string 1 date-string) (match-string 2 date-string) (match-string 3 date-string)))
                                                               ,(string-to-number (format "%s%s"   (match-string 4 date-string) (match-string 5 date-string))))))))))
        (eldev--assq-set (car repository) package eldev--vc-repository-packages)
        package))))

(defun eldev--vc-install-as-package (repository)
  ;; Unlike with local sources, for VC-fetched projects we generate and install Emacs
  ;; packages here rather than when loading.  The reason is that the source checkout is
  ;; controlled by Eldev and thus shouldn't ever be changed outside.
  (let ((tmp-package-dir (make-temp-file "eldev-vc-" t))
        (package         (eldev--vc-repository-package (car repository))))
    (unless package
      ;; It might happen that the package descriptor is not known yet, see e.g. test
      ;; `eldev-vc-repositories-4'.
      (setf package (eldev--vc-fetch-repository repository)))
    (eldev-verbose "Creating a package from `%s'" (eldev--vc-repository-name repository))
    (let ((default-directory (eldev--vc-clone-dir repository))
          (display-stdout    eldev-display-indirect-build-stdout)
          ;; Not using `--print-filename' here so that output can be better forwarded to
          ;; stdout if `eldev-display-indirect-build-stdout' asks for that.
          (command-line      `("package" "--output-dir" ,tmp-package-dir
                               "--force-version" ,(package-version-join (package-desc-version package))))
          (setup             (plist-get (cdr repository) :setup)))
      (when setup
        (setf command-line (append `("--setup" ,(prin1-to-string setup)) command-line)))
      (eldev-call-process (eldev-shell-command) command-line
        :forward-output     (if display-stdout t 'stderr)
        :destination        (if display-stdout t '(t nil))
        :trace-command-line (eldev-format-message "Full command line (in directory `%s')" default-directory)
        :die-on-error       (eldev-format-message "child Eldev process for VC-fetched package `%s'" (car repository)))
      (unless display-stdout
        (if (= (point-min) (point-max))
            (eldev-verbose "Child Eldev process produced no output (other than maybe on stderr)")
          (eldev-verbose "(Non-stderr) output of the child Eldev process:")
          (eldev-verbose (buffer-string))))
      (let ((generated (eldev-find-files '("./*.tar" "./*.el") t tmp-package-dir)))
        (unless generated
          (error "Child Eldev process succeeded, but apparently didn't generate a package"))
        (when (cdr generated)
          (error "Child Eldev process seems to have generated more than one (package) file"))
        ;; Not using `package-install-file' as it would ignore the forced version.
        (let ((tmp-package (copy-sequence package)))
          (setf (package-desc-kind tmp-package) (if (string-suffix-p ".tar" (car generated)) 'tar 'single))
          (with-temp-buffer
            (insert-file-contents (car generated))
            (package-unpack tmp-package))))
      (ignore-errors (delete-directory tmp-package-dir t)))))

(defun eldev--vc-pick-release-tag (&optional release-tag-regexp)
  "Pick the most recent release tag from the repository.
Current directory must be a clone with a remote named `origin'.
Returns a cons cell of (TAG-NAME . VERSION).  If there are no
appropriate tags at all, returns nil."
  (eldev-call-process (eldev-git-executable) `("ls-remote" "--tags" "origin")
    :die-on-error t
    (let ((case-fold-search nil)
          tags)
      (while (let ((from (search-forward "refs/tags/" nil t)))
               (when from
                 (end-of-line)
                 (push (buffer-substring from (point)) tags))))
      (let ((result (eldev--vc-most-recent-release-tag tags release-tag-regexp)))
        (if result
            (eldev-trace "Most recent stable release in the repository is tagged `%s'" (car result))
          (eldev-trace "There are apparently no stable releases in the repository; using HEAD instead"))
        result))))

(defun eldev--vc-current-commit-release-tag (&optional release-tag-regexp)
  (eldev-call-process (eldev-git-executable) `("--no-pager" "tag" "--no-color" "--points-at" "HEAD")
    :die-on-error t
    (eldev--vc-most-recent-release-tag (split-string (string-trim (buffer-string)) "\n") release-tag-regexp)))

(defun eldev--vc-most-recent-release-tag (tags &optional release-tag-regexp)
  (unless release-tag-regexp
    (setf release-tag-regexp eldev-vc-default-release-tag-regexp))
  (let (most-recent-tag
        most-recent-version)
    (dolist (tag tags)
      (when (let ((case-fold-search t))
              (string-match release-tag-regexp tag))
        (let ((version (ignore-errors (version-to-list (match-string 1 tag)))))
          (when (and version (or (null most-recent-version) (version-list-< most-recent-version version)))
            (setf most-recent-tag     tag
                  most-recent-version version)))))
    (when most-recent-tag
      (cons most-recent-tag most-recent-version))))



;; Real work of `eldev init' command is moved here to make Eldev startup slightly faster:
;; the command is rarely needed and this file is not loaded by default.

(declare-function eldev--autoloads-used-p 'eldev-plugins)

(defun eldev--do-init ()
  (when (file-exists-p eldev-file)
    (signal 'eldev-error `("File `%s' already exists in this project" ,eldev-file)))
  (let* ((package         (ignore-errors (eldev-package-descriptor)))
         (requirements    (when package (package-desc-reqs package)))
         (archives-to-use t)
         (vc-backend      (eldev-vc-detect))
         (vc-full-name    (eldev-vc-full-name vc-backend))
         autoloads
         .ignore)
    (if eldev-init-interactive
        (cond ((eldev-any-p (not (eq (car-safe it) 'emacs)) requirements)
               (when (eldev-y-or-n-p "Try to automatically select package archive(s) for dependency lookup? ")
                 (eldev-print "Please wait, this might take a while...")
                 (dolist (archive eldev--known-package-archives)
                   (unless (eldev--stable/unstable-archive-p (cadr archive))
                     (eldev-use-package-archive (car archive))))
                 (let ((archive-options (eldev--init-all-archive-combinations
                                         (mapcar #'car (eldev-filter (or (eldev--stable/unstable-archive-p (cadr it))
                                                                         (null (eldev--stable/unstable-archive-counterpart (cadr it))))
                                                                     eldev--known-package-archives)))))
                   (while archive-options
                     (let ((archives (pop archive-options))
                           simple)
                       (dolist (archive archives)
                         (let ((entry (eldev--resolve-package-archive archive)))
                           (if (eldev--stable/unstable-archive-p entry)
                               (progn (push (plist-get entry :stable)   simple)
                                      (push (plist-get entry :unstable) simple))
                             (push entry simple))))
                       (if (let ((package-archives (eldev-filter (memq it simple) package-archives)))
                             (eldev--fetch-archive-contents (eldev--determine-archives-to-fetch))
                             (package-read-all-archive-contents)
                             (package-load-all-descriptors)
                             (ignore-errors
                               (let ((inhibit-message t))
                                 (package-compute-transaction nil requirements))))
                           (progn (eldev-print "Autoguessed the following %s" (eldev-message-enumerate '("package archive:" "package archives:") archives))
                                  (setf archives-to-use archives
                                        archive-options nil))
                         (eldev-verbose "Cannot fetch project dependencies from %s" (eldev-message-enumerate "package archive" archives))))))
                 (when (eq archives-to-use t)
                   (eldev-warn "Failed to autoguess needed package archives; please edit `%s' as appropriate later" eldev-file))))
              (package
               (eldev-print "This project has no dependencies (yet), not activating any archives")))
      (eldev-trace (cond (requirements
                          "Not in interactive mode, not autodetermining package archives to use")
                         (package
                          "This project has no dependencies (yet)"))))
    (unless package
      (eldev-warn "This directory doesn't seem to contain a valid Elisp package (yet)")
      (eldev-print "If it does have main `.el' file, headers in it are likely corrupt or incomplete
Try evaluating `(package-buffer-info)' in a buffer with the file")
      ;; In non-interactive mode we continue anyway; in interactive we ask first.
      (when (and eldev-init-interactive (not (eldev-y-or-n-p "Continue anyway? ")))
        (signal 'eldev-quit 1)))
    (require 'eldev-plugins)
    (when (eldev--autoloads-used-p)
      (eldev-trace "Detected autoload cookies in project `.el' files")
      (setf autoloads (if eldev-init-interactive
                          (eldev-y-or-n-p (eldev-format-message "Autoload cookies (`;;;###autoload') detected; enable plugin `autoloads'? "))
                        (eldev-trace "Not in interactive mode, will enable plugin `autoloads' by default")
                        t)))
    (if vc-backend
        (progn
          (eldev-trace "Detected a %s repository" vc-full-name)
          (let ((filename (eldev--vc-ignore-file vc-backend)))
            (when (if eldev-init-interactive
                      (eldev-y-or-n-p (eldev-format-message "Usage of %s detected; modify `%s' as appropriate? " vc-full-name filename))
                    (eldev-trace "Not in interactive mode, will modify `%s' by default" filename)
                    t)
              (setf .ignore filename))))
      (eldev-verbose "This doesn't appear to be a supported VCS repository"))
    (with-temp-file eldev-file
      (insert "; -*- mode: emacs-lisp; lexical-binding: t -*-\n\n")
      (cond ((eq archives-to-use t)
             (eldev-trace "Adding a few commented-out calls to `eldev-use-package-archive' to `%s'" eldev-file)
             (insert ";; Uncomment some calls below as needed for your project.\n")
             (dolist (archive eldev--known-package-archives)
               (when (or (eldev--stable/unstable-archive-p (cadr archive))
                         (null (eldev--stable/unstable-archive-counterpart (cadr archive) t)))
                 (insert (format ";(eldev-use-package-archive '%s)\n" (car archive))))))
            (archives-to-use
             (eldev-trace "Adding the autodetermined package archives to `%s'" eldev-file)
             (insert ";; Autodetermined by `eldev init'.\n")
             (dolist (archive archives-to-use)
               (insert (format "(eldev-use-package-archive '%s)\n" archive))))
            (t
             (insert ";; Calls to `eldev-use-package-archive' are not needed: no dependencies\n")))
      (when autoloads
        (insert "\n(eldev-use-plugin 'autoloads)\n")))
    (eldev-print "Created file `%s' for this project" eldev-file)
    (when (and .ignore (or (not (eq vc-backend 'SVN)) (eldev-svn-executable 'warn)))
      ;; This we don't do with `vc' package, but rather directly.
      (let ((files-to-ignore       `(,eldev-cache-dir ,eldev-local-file))
            (dont-check-if-ignored (not (and (eq vc-backend 'Git) (eldev-git-executable t))))
            add-to-ignore)
        (if dont-check-if-ignored
            (setf add-to-ignore files-to-ignore)
          (dolist (file files-to-ignore)
            (if (= (eldev-call-process (eldev-git-executable) `("check-ignore" "--quiet" ,file)) 0)
                (eldev-trace "Git already ignores `%s'" file)
              (push file add-to-ignore)
              (eldev-trace "Git doesn't ignore `%s' currently" file)))
          (setf add-to-ignore (nreverse add-to-ignore)))
        (if add-to-ignore
            (let (failed)
              (eldev-verbose "Adding %s to `%s'" (eldev-message-enumerate "file" add-to-ignore) .ignore)
              (setf failed (eldev--vc-ignore-standard-files vc-backend files-to-ignore))
              (unless (or failed dont-check-if-ignored)
                (dolist (file files-to-ignore)
                  (when (/= (eldev-call-process (eldev-git-executable) `("check-ignore" "--quiet" ,file)) 0)
                    (eldev-warn "Failed to convince Git to ignore file `%s'" file)
                    (setf failed t))))
              (unless failed
                (eldev-print (eldev-format-message "Modified %s `%s'" (if (eq vc-backend 'SVN) "property" "file") .ignore))))
          (eldev-verbose "Git already ignores what Eldev thinks it should"))))))

(defun eldev--init-all-archive-combinations (all-archives)
  (let ((combinations (list nil)))
    (dotimes (k (length all-archives))
      (setf k            (- (length all-archives) k 1)
            combinations (append combinations (mapcar (lambda (combination) (cons k combination)) combinations))))
    (mapcar (lambda (combination)
              (mapcar (lambda (k) (nth k all-archives)) combination))
            (sort combinations (lambda (a b)
                                 (let ((length-a (length a))
                                       (length-b (length b)))
                                   (or (< length-a length-b)
                                       (and (= length-a length-b)
                                            (let (before)
                                              (while a
                                                (when (< (pop a) (pop b))
                                                  (setf before t
                                                        a      nil)))
                                              before)))))))))

(defun eldev--vc-ignore-file (backend)
  (eldev-pcase-exhaustive backend
    (`Git ".gitignore")
    (`Hg  ".hgignore")
    ;; Not really a file, but close enough.
    (`SVN "svn:ignore")))

(defun eldev--vc-ignore-standard-files (backend &optional files)
  (with-temp-buffer
    (let ((.ignore (eldev--vc-ignore-file backend)))
      (if (eq backend 'SVN)
          ;; This can return non-null status if there is no such property yet.
          (eldev-call-process (eldev-svn-executable) '("propget" "svn:ignore" ".")
            :destination  `(,(current-buffer) nil)
            :discard-ansi t)
        (condition-case nil
            (insert-file-contents .ignore t)
          ;; Pre-26 Emacsen don't know about `file-missing', so catch broadly.
          (file-error)))
      (goto-char (point-max))
      (unless (eolp)
        (insert "\n"))
      (when (looking-back (rx nonl "\n") nil)
        (insert "\n"))
      (unless (eq backend 'SVN)
        (insert (eldev-format-message "# Added automatically by `eldev init'.\n")))
      (dolist (file (or files `(,eldev-cache-dir ,eldev-local-file)))
        (insert (eldev-pcase-exhaustive backend
                  (`Git (format "/%s"  file))
                  (`Hg  (format "^%s$" (regexp-quote file)))
                  (`SVN file))
                "\n"))
      (if (eq backend 'SVN)
          (/= (eldev-call-process (eldev-svn-executable) `("propset" "svn:ignore" ,(buffer-string) ".")) 0)
        (eldev-write-to-file .ignore)
        nil))))


(defun eldev--do-githooks ()
  (unless (eq (eldev-vc-detect) 'Git)
    (signal 'eldev-error `("Directory `%s' doesn't appear to be a Git checkout" ,(abbreviate-file-name eldev-project-dir))))
  (let ((num-processed 0)
        (num-possible  0))
    (dolist (hook (eldev-find-files "./githooks/*"))
      (let ((link-as (replace-regexp-in-string (rx bos "githooks") ".git/hooks" hook t t)))
        (if eldev-githooks-uninstall
            (let ((link-target (file-symlink-p link-as)))
              (if link-target
                  (if (file-equal-p link-target hook)
                      (progn (delete-file link-as)
                             (setf num-processed (1+ num-processed))
                             (eldev-print "Uninstalled hook `%s' symlinked as `%s'" hook link-as))
                    (eldev-trace "Ignoring symlink `%s' as pointing to a wrong target `%s'" link-as link-target))
                (if (eldev--githooks-is-copy link-as hook)
                    (if eldev-githooks-force
                        (progn (delete-file link-as)
                               (setf num-processed (1+ num-processed))
                               (eldev-print "Uninstalled hook `%s' copied as `%s'" hook link-as))
                      (setf num-possible (1+ num-possible))
                      (eldev-warn "Hook file `%s' appears to be a copy (not a symlink) of `%s'" link-as hook))
                  (eldev-trace "Ignoring file `%s': not a copy of `%s'" link-as hook))))
          (condition-case nil
              (progn (make-symbolic-link (expand-file-name hook eldev-project-dir) (expand-file-name link-as eldev-project-dir) eldev-githooks-force)
                     (setf num-processed (1+ num-processed))
                     (eldev-print "Installed as symbolic link: `%s' -> `%s'" link-as hook))
            (file-already-exists (unless (file-equal-p link-as hook)
                                   (setf num-possible (1+ num-possible))
                                   (eldev-warn "Hook file `%s' exists and doesn't match `%s'" link-as hook)))))))
    (if (> num-possible 0)
        (eldev-print "Use option `--force' to %s" (cond (eldev-githooks-uninstall "delete such copies")
                                                        ((= num-possible 1)       "replace the existing hook file")
                                                        (t                        "replace existing hook files")))
      (when (= num-processed 0)
        (eldev-print-nothing-to-do)))))

;; FIXME: Rename and move somewhere?  Looks generally useful.
(defun eldev--githooks-is-copy (file1 file2)
  (ignore-errors (string= (with-temp-buffer
                            (insert-file-contents-literally file1)
                            (buffer-string))
                          (with-temp-buffer
                            (insert-file-contents-literally file2)
                            (buffer-string)))))

(defun eldev--githooks-list-not-installed ()
  ;; Keep in sync with `eldev--do-githooks'.
  (let (not-installed)
    (dolist (hook (eldev-find-files "./githooks/*"))
      (let* ((link-as     (replace-regexp-in-string (rx bos "githooks") ".git/hooks" hook t t))
             (link-target (file-symlink-p link-as)))
        (unless (or (and link-target (file-equal-p link-target hook))
                    (eldev--githooks-is-copy link-as hook))
          (push hook not-installed))))
    (nreverse not-installed)))


(provide 'eldev-vc)

;;; eldev-vc.el ends here
