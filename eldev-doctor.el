;;; eldev-doctor.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2022, 2023, 2024 Paul Pogonyshev

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
(require 'eldev-vc)


(defvar eldev--doctests nil)

;; Internal helper for `eldev-defdoctest'.
(defun eldev--register-doctest (doctest name keywords)
  (while keywords
    (eldev-pcase-exhaustive (pop keywords)
      ((and (or :caption :categories :depends-on) keyword)
       (eldev-put doctest keyword (pop keywords)))))
  (eldev--assq-set name doctest eldev--doctests))

(defmacro eldev-defdoctest (name arguments &rest body)
  "Define a doctor test.

Test function should return a plist with at least `result' key.
Associated value is then available to later doctests.  The plist
may also contain `ok', `warnings' and `short-answer' keys.  If
`ok' is not set, it is determined by whether `warnings' is
present and non-empty.  `short-answer' defaults to string “YES”
or “NO”, depending on `ok'."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body  (eldev-macroexp-parse-body body))
        (doctest-name (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-") (? "doctest-")) "" (symbol-name name))))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:name       (setf doctest-name (pop body)))
        (keyword (push keyword keywords) (push (pop body) keywords))))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (eldev--register-doctest ',name ',doctest-name ',(nreverse keywords)))))


(defun eldev--do-doctor (selectors)
  (when (or (null selectors) (string-prefix-p "-" (car selectors)))
    (push "all" selectors))
  (let (doctests)
    (dolist (selector selectors)
      (let ((negate (string-prefix-p "-" selector)))
        (setf selector (intern (replace-regexp-in-string (rx bos (any "+-")) "" selector)))
        (if (eq selector 'all)
            (setf doctests (unless negate (copy-sequence eldev--doctests)))
          (let ((matches (assq selector eldev--doctests)))
            (setf matches (if matches
                              (list matches)
                            (or (eldev-filter (memq selector (eldev-listify (eldev-get (cdr it) :categories))) eldev--doctests)
                                (when eldev-dwim
                                  (let ((regexp (rx-to-string (symbol-name selector))))
                                    (eldev-filter (string-match-p regexp (symbol-name (car it))) eldev--doctests))))))
            (unless matches
              (signal 'eldev-error `(:hint ,(when eldev-dwim "As `eldev-dwim' is set, also tried it as a name substring to no success")
                                     "Selector `%s' matches neither a doctest name nor a category" ,selector)))
            (setf doctests (eldev-filter (if (memq it matches) (not negate) (memq it doctests)) eldev--doctests))))))
    (if doctests
        (let ((num-executed-on-request 0)
              (num-visibly-failed      0)
              (doctests-sequence       (list nil))
              results
              generated-output
              last-with-warnings)
          (dolist (doctest (nreverse doctests))
            (eldev--doctor-build-sequence doctests-sequence (car doctest) t))
          (dolist (doctest (nreverse (car doctests-sequence)))
            (let* ((name           (car doctest))
                   (function       (cdr (assq name eldev--doctests)))
                   (user-requested (cdr doctest)))
              (eldev-verbose "Running doctest `%s' %s..." name (if user-requested "on user request" "needed by some other test"))
              (when user-requested
                (setf num-executed-on-request (1+ num-executed-on-request)))
              (let ((plist (funcall function results)))
                (when plist
                  (push (cons name (plist-get plist 'result)) results)
                  (unless (plist-member plist 'ok)
                    (setf plist (plist-put plist 'ok (null (plist-get plist 'warnings)))))
                  (let ((failed (not (plist-get plist 'ok))))
                    ;; Non-user-requested doctests are not reported even if they generate
                    ;; warnings.
                    (when (and user-requested (or failed eldev-doctor-print-successful))
                      (when failed
                        (setf num-visibly-failed (1+ num-visibly-failed)))
                      (let ((caption       (eldev-get function :caption))
                            (short-answer  (or (plist-get plist 'short-answer) (if failed "NO" "YES")))
                            (with-warnings (eldev-unless-quiet failed)))
                        (when (or (and with-warnings generated-output) last-with-warnings)
                          (eldev-output ""))
                        (unless (stringp caption)
                          (setf caption (eval caption t)))
                        (eldev-output "%s %s"
                                      (if with-warnings (eldev-colorize caption 'section) caption)
                                      (eldev-colorize short-answer (if failed 'error 'success)))
                        (when with-warnings
                          (dolist (warning (or (eldev-listify (plist-get plist 'warnings)) (eldev-colorize "No warning text provided" 'details)))
                            (unless (plist-get plist 'dont-reformat-warnings)
                              (with-temp-buffer
                                (insert warning)
                                (let ((fill-column 78))
                                  (fill-region (point-min) (point-max))
                                  (setf warning (buffer-string)))))
                            (eldev-output "\n%s" warning)))
                        (setf last-with-warnings with-warnings
                              generated-output   t))))))))
          (if (= num-visibly-failed 0)
              (eldev-print "\nRan %s, %s" (eldev-message-plural num-executed-on-request "doctest")
                           (eldev-colorize (if (= num-executed-on-request 1) "it didn't generate any warnings" "none generated any warnings") 'success))
            (eldev-warn "Ran %s, %s generated %s"
                        (eldev-message-plural num-executed-on-request "doctest")
                        (if (= num-visibly-failed num-executed-on-request)
                            (if (= num-executed-on-request 1) "it" "all of them")
                          (eldev-format-message "%d of them" num-visibly-failed))
                        (if (= num-visibly-failed 1) "a warning" "warnings"))
            (signal 'eldev-quit 1)))
      (eldev-print "Nothing to delete"))))

(defun eldev--doctor-build-sequence (sequence name user-requested &optional dependency-stack)
  ;; A project may disable certain tests.  They still run when non-user-requested
  ;; (i.e. because of depedencies), but results are not printed.
  (if (and user-requested (eldev--doctor-test-disabled-p name))
      (eldev-trace "Ignoring doctest `%s' as disabled in this project" name)
    (let ((scheduled (assq name (car sequence))))
      (if scheduled
          (when user-requested
            (setf (cdr scheduled) t))
        (when (memq name dependency-stack)
          (error "Circular dependency detected: `%s', %s" name (eldev-message-enumerate nil (car sequence))))
        (let ((function (cdr (assq name eldev--doctests))))
          (dolist (depends-on (eldev-listify (eldev-get function :depends-on)))
            (eldev--doctor-build-sequence sequence depends-on nil dependency-stack)))
        (push `(,name . ,user-requested) (car sequence))))))

(defun eldev--doctor-list-tests ()
  (dolist (entry (reverse eldev--doctests))
    (let* ((name     (car entry))
           (function (cdr entry))
           (caption  (eldev-get function :caption)))
      (unless (eldev--doctor-test-disabled-p name)
        (unless (stringp caption)
          (setf caption (eval caption t)))
        (if (eq eldev-verbosity-level 'quiet)
            (eldev-output "%s" name)
          (eldev-output "%-28s %s" (eldev-colorize name 'name) caption)))))
  (signal 'eldev-quit 0))

(defun eldev--doctor-test-disabled-p (name)
  (memq name (eldev-listify eldev-doctor-disabled-tests)))




(eldev-defdoctest eldev-doctest-eldev-presence (_results)
  :caption    (eldev-format-message "Does the project contain file `%s'?" eldev-file)
  :categories eldev
  (if (file-exists-p eldev-file)
      '(result t)
    `(result   nil
      warnings ,(eldev-format-message "\
It is recommended to have file `%s' in your project root if you use Eldev.
Otherwise, certain tools (e.g. Projectile, `flycheck-eldev' or `flymake-eldev')
won't consider your project to be Eldev-based.  It is even fine to have a
completely empty file if you don't have anything to configure or customize." eldev-file))))

(defvar no-byte-compile)

(eldev-defdoctest eldev-doctest-eldev-byte-compilable (results)
  :caption    (eldev-format-message "Is file `%s' byte-compilable?" eldev-file)
  :categories eldev
  :depends-on eldev-presence
  (when (cdr (assq 'eldev-presence results))
    (if (with-temp-buffer
          (insert-file-contents "Eldev")
          (hack-local-variables)
          ;; Can apparently fail (on 24.4?) otherwise.
          (when (boundp 'no-byte-compile)
            no-byte-compile))
        `(result   nil
          warnings ,(eldev-format-message "\
It is recommended to make file `%s' byte-compilable.  This would make it
validatable by `flycheck-eldev'.  Earlier examples of the file would often
contain local variable “no-byte-compile: t”, but this turned out to be
unnecessary." eldev-file))
      `(result t))))

(eldev-defdoctest eldev-doctest-explicit-main-file (_results)
  :caption    "Does the project specify its main file?"
  :categories (eldev package packaging)
  (cond (eldev-project-main-file
         '(result t))
        ((let ((pkg-file (eldev-package-descriptor-file-name)))
           (and pkg-file (file-readable-p pkg-file)))
         '(result nil short-answer "NO (but has a package descriptor)"))
        (t
         (let (with-package-info)
           (dolist (file (directory-files eldev-project-dir nil "\\.el\\'"))
             (with-temp-buffer
               (insert-file-contents file)
               (when (ignore-errors (package-buffer-info))
                 (push file with-package-info))))
           ;; 0 shouldn't happen, as then Eldev should fail to begin with.
           (if (<= (length with-package-info) 1)
               '(result nil short-answer "NO (but has unambiguous headers)")
             `(result   nil
               warnings ,(eldev-format-message "\
You should set variable `eldev-project-main-file' in file `%s' explicitly, as
there are several files at the top level with valid Elisp package headers: %s.
Eldev (and Emacs packaging system) will choose one at random, thus producing
unreliable results." eldev-file (eldev-message-enumerate nil with-package-info))))))))

(eldev-defdoctest eldev-doctest-explicit-emacs-version (_results)
  :caption    "Does the project state which Emacs version it needs?"
  :categories (dependencies deps)
  (let (found)
    (dolist (dependency (package-desc-reqs (eldev-package-descriptor)))
      (when (and (eq (car dependency) 'emacs) (cadr dependency))
        (setf found t)))
    (if found
        '(result t)
      '(result nil warnings "\
The project should explicitly state its minimum required Emacs version in its
package headers or the package descriptor file."))))

(eldev-defdoctest eldev-doctest-stable/unstable-archives (_results)
  :caption    "Are stable/unstable package archives used where possible?"
  :categories (dependencies deps)
  (let (warnings)
    (dolist (archive eldev--known-package-archives)
      (when (eldev--stable/unstable-archive-p (cadr archive))
        ;; We won't detect situations where e.g. `melpa-stable' and `melpa-unstable' are
        ;; added separately, but oh well.
        (let* ((stable        (plist-get (cadr archive) :stable))
               (unstable      (plist-get (cadr archive) :unstable))
               (stable-used   (member (nth 1 (assq stable   eldev--known-package-archives)) package-archives))
               (unstable-used (member (nth 1 (assq unstable eldev--known-package-archives)) package-archives)))
          (when (eldev-xor stable-used unstable-used)
            (push (eldev-format-message "\
It is recommended to use stable/unstable package archive `%s' instead of `%s'
directly.  This way you can switch between the variants using global options
`--stable' and `--unstable'."
                                        (car archive) (if stable-used stable unstable))
                  warnings)))))
    `(result   ,(null warnings)
      warnings ,warnings)))

(eldev-defdoctest eldev-doctest-recent-stable-releases (_results)
  :caption    "Are stable releases up-to-date?"
  :categories (version-control vc)
  (pcase (eldev--doctor-last-stable-release-data)
    (`nil
     '(result unknown short-answer "no VC detected"))
    (`none
     '(result nil short-answer "NO (but no stable releases at all)"))
    (`(,version ,commit-date ,commits-after)
     (cond ((= commits-after 0)
            `(result t short-answer ,(eldev-format-message "YES (no commits after %s)" version)))
           ((<= commits-after 10)
            `(result t short-answer ,(eldev-format-message "YES (only %s since %s)" (eldev-message-plural commits-after "commit") version)))
           ((null commit-date)
            `(result unknown short-answer ,(eldev-format-message "cannot determine %s release date" version)))
           (t
            (let* ((date        (float-time (date-to-time commit-date)))
                   (now         (float-time))
                   (days-passed (round (/ (- now date) 86400))))
              (if (<= days-passed 90)
                  `(result t short-answer ,(eldev-format-message "YES (only ~%s since %s)" (eldev-message-plural days-passed "day") version))
                `(result nil short-answer ,(eldev-format-message "NO (~%s and %s since %s)"
                                                                 (eldev-message-plural days-passed "day") (eldev-message-plural commits-after "commit") version)
                         warnings "\
If you have ever released a stable version (created a version-like tag in your
VCS), you should keep it up-to-date with the latest features.  Otherwise,
people using a stable package archive, e.g. MELPA Stable or GNU ELPA, will
never notice that your project is still being developed, new features appear
and bugs get fixed.

It is up to you to decide *when* a version is stable enough.  But please don't
release a stable version only to never do that again in future."))))))
    (backend
     `(result unknown short-answer ,(eldev-format-message "can't tell for %s" (eldev-vc-full-name backend))))))

(defun eldev--doctor-last-stable-release-data ()
  (eldev-with-vc nil
    (pcase backend
      (`Git
       (when (eldev-git-executable t)
         (let (last-version
               last-version-tag)
           (eldev-call-process (eldev-git-executable) '("for-each-ref" "refs/tags" "--format" "%(refname)")
             :destination  '(t nil)
             :discard-ansi t
             :die-on-error t
             (while (not (eobp))
               (let* ((tag     (string-remove-prefix "refs/tags/" (buffer-substring (point) (line-end-position))))
                      (version (string-remove-prefix (rx (any "rRvV")) tag)))
                 (when (and (ignore-errors (version-to-list version)) (or (null last-version) (version< last-version version)))
                   (setf last-version     version
                         last-version-tag tag)))
               (forward-line 1)))
           (if last-version
               `(,last-version
                 ;; `git show' doesn't print _only_ date for signed commits.
                 ,(eldev-call-process (eldev-git-executable) `("--no-pager" "log" "-1" "--no-color" "--format=%cI" "--no-patch" ,last-version-tag)
                    :destination  '(t nil)
                    :discard-ansi t
                    :die-on-error t
                    (let ((date-string (string-trim (buffer-string))))
                      ;; Don't match the whole, but just a bit of assertion here.
                      (when (string-match-p (rx bos (= 4 num) "-" (= 2 num) "-" (= 2 num)) date-string)
                        date-string)))
                 ,(eldev-call-process (eldev-git-executable) `("rev-list" ,(format "%s.." last-version-tag) "--count")
                    :destination  '(t nil)
                    :discard-ansi t
                    :die-on-error t
                    (string-to-number (buffer-string))))
             'none))))
      (_
       backend))))

(eldev-defdoctest eldev-doctest-githooks (_results)
  :caption    "Are project-recommended Git hooks installed?"
  :categories (version-control vc)
  (when (and (eq (eldev-vc-detect) 'Git) (eldev-find-files "./githooks/*"))
    (let ((not-installed (eldev--githooks-list-not-installed)))
      (if not-installed
          `(result nil warnings ,(eldev-format-message "\
This projects comes with recommended Git hooks, but you don't use %s.
Consider installing %s by running:

    $ eldev githooks"
                                                       (eldev-message-enumerate nil not-installed) (if (cddr not-installed) "them" "it")))
        '(result t)))))

(eldev-defdoctest eldev-up-to-date-copyright (_results)
  :caption    "Are copyright notices up-to-date?"
  :categories (package packaging)
  (eval-and-compile (require 'copyright))
  (eldev-with-vc nil
    (pcase backend
      (`nil
       '(result unknown short-answer "can't tell without VC"))
      (`Git
       (when (eldev-git-executable t)
         ;; Shares some similarities with `eldev--maintainer-update-copyright'.
         (let (outdated
               anything-checked)
           (dolist (file (eldev-find-and-trace-files `(:and (eldev-standard-fileset 'all) ,eldev-update-copyright-fileset) "file%s to check for copyright notice in"))
             (eldev-trace "Checking file `%s' for a copyright notice..." file)
             (with-temp-buffer
               (insert-file-contents file)
               (if (save-excursion (save-restriction (copyright-find-copyright)))
                   ;; Let's assume they won't change group number.
                   (let ((copyright-years (match-string 3)))
                     (when (string-match "[0-9]+\\'" copyright-years)
                       (let ((copyright-year (match-string 0 copyright-years)))
                         (setf copyright-year (when (or (= (length copyright-year) 4) (when (= (length copyright-year) 2) (setf copyright-year (concat "20" copyright-year))))
                                                (string-to-number copyright-year)))
                         (if copyright-year
                             (let (last-change-year)
                               (eldev-trace "Asking Git when file `%s' was last changed..." file)
                               (eldev-call-process (eldev-git-executable) `("--no-pager" "log" "--follow" "--no-decorate" "-1" "--format=%cs" ,file)
                                 :trace-command-line t
                                 :destination  '(t nil)
                                 :discard-ansi t
                                 :die-on-error t
                                 (when (looking-at (rx (group (= 4 num)) "-" (group (= 2 num)) "-" (group (= 2 num))))
                                   (setf last-change-year (string-to-number (match-string 1)))))
                               (if last-change-year
                                   (progn (setf anything-checked t)
                                          (when (< copyright-year last-change-year)
                                            (push (list file copyright-year last-change-year) outdated)))
                                 (eldev-warn "Unable to determine the year of the last Git change of file `%s'" file)))
                           (eldev-warn "Unable to determine the year from the copyright notice in file `%s'" file)))))
                 (eldev-trace "No such notice found, skipping this file"))))
           (if outdated
               `(result nil warnings ,(eldev-format-message "\
Copyright notices in the following file(s) appear to be outdated:

%s

Consider executing (you may need to enable plugin `maintainer' first):

    $ eldev update-copyright" (mapconcat (lambda (entry)
                                           (format "%s (mentions %s, last changed in %s)" (nth 0 entry) (nth 1 entry) (nth 2 entry)))
                                         (nreverse outdated) "\n"))
                        dont-reformat-warnings t)
             (if anything-checked
                 `(result t)
               `(result t short-answer "YES (no copyright notices?)"))))))
      (backend
       `(result unknown short-answer ,(eldev-format-message "can't tell for %s" (eldev-vc-full-name backend)))))))

(eldev-defdoctest eldev-doctest-eldev-file-owners (_results)
  :caption    "Are Eldev cache files owned by the current user?"
  :categories eldev
  (let ((expected-user          (user-uid))
        (expected-group         (group-gid))
        (unexpected-owner-files (list nil)))
    (dolist (cache-dir (list (eldev-global-cache-dir) (eldev-cache-dir nil)))
      ;; `directory-files-recursively' is too new for Emacs 24.
      (eldev--find-files-with-wrong-owner cache-dir cache-dir expected-user expected-group unexpected-owner-files))
    (if (setf unexpected-owner-files (nreverse (car unexpected-owner-files)))
        ;; It is just too hard to format several filenames sanely, so mention only the first.
        (let ((first (car unexpected-owner-files)))
          `(result nil warnings ,(eldev-format-message "\
File `%s' in Eldev cache directory `%s' appears to be owned by a
“wrong” user/group: `%s/%s' (expected is `%s/%s').%s

This is likely a consequence of erroneous Eldev behavior when
using Docker: hopefully all bugs that would cause this have been
fixed by now, but previous invocations might have lead to
existence of such files.  Unfortunately, you have to delete them
manually, otherwise Eldev might misbehave."
                                                       (nth 1 first) (nth 0 first)
                                                       (eldev--user-name (nth 2 first)) (eldev--group-name (nth 3 first))
                                                       (eldev--user-name expected-user) (eldev--group-name expected-group)
                                                       (if (cdr unexpected-owner-files)
                                                           (format "  There also appears to be %s." (eldev-message-plural (1- (length unexpected-owner-files)) "more such file"))
                                                         ""))))
      `(result t))))

(defun eldev--find-files-with-wrong-owner (root directory expected-user expected-group result)
  (setf directory (file-name-as-directory directory))
  (dolist (file (sort (file-name-all-completions "" directory) #'string<))
    (unless (member file '("./" "../"))
      (setf file (expand-file-name file directory))
      (let* ((attributes (file-attributes file 'integer))
             ;; `file-attribute-user-id' and `file-attribute-group-id' are only available
             ;; since Emacs 26.
             (file-user  (nth 2 attributes))
             (file-group (nth 3 attributes)))
        (if (and (equal file-user expected-user) (equal file-group expected-group))
            (when (equal (file-name-as-directory file) file)
              (eldev--find-files-with-wrong-owner root file expected-user expected-group result))
          (push `(,root ,(file-relative-name file root) ,file-user ,file-group) (car result)))))))

(defun eldev--user-name (uid)
  (or (ignore-errors (user-login-name uid)) uid))

(declare-function group-name nil) 
(defun eldev--group-name (gid)
  (or (ignore-errors (group-name gid)) gid))


(provide 'eldev-doctor)

;;; eldev-doctor.el ends here
