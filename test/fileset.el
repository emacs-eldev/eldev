(require 'emake)
(require 'ert)

(defvar emake--test-all-files       '("README"
                                      "bar/README"
                                      "files/README"
                                      "files/xyz/README"))
(defvar emake--test-root-files      '("README"))
(defvar emake--test-bar-files       '("bar/README"))
(defvar emake--test-files-files     '("files/README" "files/xyz/README"))
(defvar emake--test-files-xyz-files '("files/xyz/README"))


(defmacro emake--test-fileset (fileset expected-files)
  ;; Test `emake-find-files' and `emake-filter-files' on FILESET and
  ;; its negation.
  `(let ((other-files (emake-filter (not (member it (emake-listify ,expected-files))) emake--test-all-files)))
     (condition-case error
         (progn (should (equal (emake-find-files ,fileset nil "test/files") ,expected-files))
                (should (equal (emake-filter-files emake--test-all-files ,fileset nil "test/files") ,expected-files)))
       (error (emake-error "Failed for fileset %S" ,fileset)
              (signal (car error) (cdr error))))
     (condition-case error
         (progn (should (equal (emake-find-files '(:not ,fileset) nil "test/files") other-files))
                (should (equal (emake-filter-files emake--test-all-files '(:not ,fileset) nil "test/files") other-files)))
       (error (emake-error "Failed for fileset (:not %S)" ,fileset)
              (signal (car error) (cdr error))))))


(ert-deftest emake-test-empty-fileset-nil-1 ()
  (emake--test-fileset nil nil))

(ert-deftest emake-test-empty-fileset-1 ()
  (emake--test-fileset "!*" nil))
(ert-deftest emake-test-empty-fileset-2 ()
  (emake--test-fileset "!**" nil))
(ert-deftest emake-test-empty-fileset-3 ()
  (emake--test-fileset "!**/*" nil))
(ert-deftest emake-test-empty-fileset-4 ()
  (emake--test-fileset "!./*" nil))
(ert-deftest emake-test-empty-fileset-5 ()
  (emake--test-fileset "!./**" nil))
(ert-deftest emake-test-empty-fileset-6 ()
  (emake--test-fileset "!./**/*" nil))
(ert-deftest emake-test-empty-fileset-7 ()
  (emake--test-fileset "!/*" nil))
(ert-deftest emake-test-empty-fileset-8 ()
  (emake--test-fileset "!/**" nil))
(ert-deftest emake-test-empty-fileset-9 ()
  (emake--test-fileset "!/**/*" nil))

;; Some files are included first, but later everything gets excluded.
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-1 ()
  (emake--test-fileset '("README" "!*") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-2 ()
  (emake--test-fileset '("README" "!**") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-3 ()
  (emake--test-fileset '("README" "!**/*") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-4 ()
  (emake--test-fileset '("README" "!./**") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-5 ()
  (emake--test-fileset '("README" "!./**/*") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-6 ()
  (emake--test-fileset '("README" "!/**") nil))
(ert-deftest emake-test-empty-fileset-postfactum-exclusion-7 ()
  (emake--test-fileset '("README" "!/**/*") nil))

(ert-deftest emake-test-everything-fileset-1 ()
  (emake--test-fileset "*" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-2 ()
  (emake--test-fileset "**" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-3 ()
  (emake--test-fileset "**/*" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-4 ()
  (emake--test-fileset "./**" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-5 ()
  (emake--test-fileset "./**/*" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-6 ()
  (emake--test-fileset "/**" emake--test-all-files))
(ert-deftest emake-test-everything-fileset-7 ()
  (emake--test-fileset "/**/*" emake--test-all-files))

;; Some files are excluded first, but later everything gets included.
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-1 ()
  (emake--test-fileset '("!README" "*") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-2 ()
  (emake--test-fileset '("!README" "**") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-3 ()
  (emake--test-fileset '("!README" "**/*") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-4 ()
  (emake--test-fileset '("!README" "./**") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-5 ()
  (emake--test-fileset '("!README" "./**/*") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-6 ()
  (emake--test-fileset '("!README" "/**") emake--test-all-files))
(ert-deftest emake-test-everything-fileset-postfactum-inclusion-7 ()
  (emake--test-fileset '("!README" "/**/*") emake--test-all-files))

;; Test redundant (more than one in a row) '**'.
(ert-deftest emake-test-redundant-**-1 ()
  (emake--test-fileset "**/**/**/*" emake--test-all-files))
(ert-deftest emake-test-redundant-**-2 ()
  (emake--test-fileset "!**/**/**/*" nil))


;; Test fixed file and directory paths.
(ert-deftest emake-test-fixed-file-paths-1 ()
  (emake--test-fileset "./README" '("README")))
(ert-deftest emake-test-fixed-file-paths-2 ()
  (emake--test-fileset "/README" '("README")))
(ert-deftest emake-test-fixed-file-paths-3 ()
  (emake--test-fileset "./files/README" '("files/README")))
(ert-deftest emake-test-fixed-file-paths-4 ()
  (emake--test-fileset "/files/README" '("files/README")))
(ert-deftest emake-test-fixed-file-paths-5 ()
  (emake--test-fileset "./files/xyz/README" '("files/xyz/README")))
(ert-deftest emake-test-fixed-file-paths-6 ()
  (emake--test-fileset "/files/xyz/README" '("files/xyz/README")))


;; Test file paths that match directories.
(ert-deftest emake-test-directory-matches-1 ()
  (emake--test-fileset "xyz" emake--test-files-xyz-files))


;; Test that files don't get matched if their names are followed with a slash.
(ert-deftest emake-test-matching-files-as-directories-1 ()
  (emake--test-fileset "README/" nil))


;; Test simple composite filesets.
(ert-deftest emake-test-composite-fileset-1 ()
  (emake--test-fileset '(:or "/bar/" "/files/") (append emake--test-bar-files emake--test-files-files)))
(ert-deftest emake-test-composite-fileset-2 ()
  (emake--test-fileset '(:and "/bar/" "/files/") nil))
(ert-deftest emake-test-composite-fileset-3 ()
  (emake--test-fileset '(:or (:and "/bar/" "/files/") (:or "/bar/" "/files/")) (append emake--test-bar-files emake--test-files-files)))


;; Filesets that match a single file.

(ert-deftest emake-test-single-file-1 ()
  (dolist (file emake--test-all-files)
    (emake--test-fileset (format "./%s" file) (list file))))

(ert-deftest emake-test-single-file-2 ()
  (dolist (file emake--test-all-files)
    (emake--test-fileset (format "/%s" file) (list file))))


(provide 'test/fileset)
