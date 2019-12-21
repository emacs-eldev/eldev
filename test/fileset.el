(require 'eldev)
(require 'ert)

(defvar eldev--test-all-files       '("README"
                                      "bar/README"
                                      "files/README"
                                      "files/xyz/README"))
(defvar eldev--test-root-files      '("README"))
(defvar eldev--test-bar-files       '("bar/README"))
(defvar eldev--test-files-files     '("files/README" "files/xyz/README"))
(defvar eldev--test-files-xyz-files '("files/xyz/README"))


(defmacro eldev--test-fileset (fileset expected-files)
  ;; Test `eldev-find-files' and `eldev-filter-files' on FILESET and
  ;; its negation.
  `(let ((other-files (eldev-filter (not (member it (eldev-listify ,expected-files))) eldev--test-all-files)))
     (condition-case error
         (progn (should (equal (eldev-find-files ,fileset nil "test/files") ,expected-files))
                (should (equal (eldev-filter-files eldev--test-all-files ,fileset nil "test/files") ,expected-files)))
       (error (eldev-error "Failed for fileset %S" ,fileset)
              (signal (car error) (cdr error))))
     (condition-case error
         (progn (should (equal (eldev-find-files '(:not ,fileset) nil "test/files") other-files))
                (should (equal (eldev-filter-files eldev--test-all-files '(:not ,fileset) nil "test/files") other-files)))
       (error (eldev-error "Failed for fileset (:not %S)" ,fileset)
              (signal (car error) (cdr error))))))


(ert-deftest eldev-test-empty-fileset-nil-1 ()
  (eldev--test-fileset nil nil))

(ert-deftest eldev-test-empty-fileset-1 ()
  (eldev--test-fileset "!*" nil))
(ert-deftest eldev-test-empty-fileset-2 ()
  (eldev--test-fileset "!**" nil))
(ert-deftest eldev-test-empty-fileset-3 ()
  (eldev--test-fileset "!**/*" nil))
(ert-deftest eldev-test-empty-fileset-4 ()
  (eldev--test-fileset "!./*" nil))
(ert-deftest eldev-test-empty-fileset-5 ()
  (eldev--test-fileset "!./**" nil))
(ert-deftest eldev-test-empty-fileset-6 ()
  (eldev--test-fileset "!./**/*" nil))
(ert-deftest eldev-test-empty-fileset-7 ()
  (eldev--test-fileset "!/*" nil))
(ert-deftest eldev-test-empty-fileset-8 ()
  (eldev--test-fileset "!/**" nil))
(ert-deftest eldev-test-empty-fileset-9 ()
  (eldev--test-fileset "!/**/*" nil))

;; Some files are included first, but later everything gets excluded.
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-1 ()
  (eldev--test-fileset '("README" "!*") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-2 ()
  (eldev--test-fileset '("README" "!**") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-3 ()
  (eldev--test-fileset '("README" "!**/*") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-4 ()
  (eldev--test-fileset '("README" "!./**") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-5 ()
  (eldev--test-fileset '("README" "!./**/*") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-6 ()
  (eldev--test-fileset '("README" "!/**") nil))
(ert-deftest eldev-test-empty-fileset-postfactum-exclusion-7 ()
  (eldev--test-fileset '("README" "!/**/*") nil))

(ert-deftest eldev-test-everything-fileset-1 ()
  (eldev--test-fileset "*" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-2 ()
  (eldev--test-fileset "**" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-3 ()
  (eldev--test-fileset "**/*" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-4 ()
  (eldev--test-fileset "./**" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-5 ()
  (eldev--test-fileset "./**/*" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-6 ()
  (eldev--test-fileset "/**" eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-7 ()
  (eldev--test-fileset "/**/*" eldev--test-all-files))

;; Some files are excluded first, but later everything gets included.
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-1 ()
  (eldev--test-fileset '("!README" "*") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-2 ()
  (eldev--test-fileset '("!README" "**") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-3 ()
  (eldev--test-fileset '("!README" "**/*") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-4 ()
  (eldev--test-fileset '("!README" "./**") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-5 ()
  (eldev--test-fileset '("!README" "./**/*") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-6 ()
  (eldev--test-fileset '("!README" "/**") eldev--test-all-files))
(ert-deftest eldev-test-everything-fileset-postfactum-inclusion-7 ()
  (eldev--test-fileset '("!README" "/**/*") eldev--test-all-files))

;; Test redundant (more than one in a row) '**'.
(ert-deftest eldev-test-redundant-**-1 ()
  (eldev--test-fileset "**/**/**/*" eldev--test-all-files))
(ert-deftest eldev-test-redundant-**-2 ()
  (eldev--test-fileset "!**/**/**/*" nil))


;; Test fixed file and directory paths.
(ert-deftest eldev-test-fixed-file-paths-1 ()
  (eldev--test-fileset "./README" '("README")))
(ert-deftest eldev-test-fixed-file-paths-2 ()
  (eldev--test-fileset "/README" '("README")))
(ert-deftest eldev-test-fixed-file-paths-3 ()
  (eldev--test-fileset "./files/README" '("files/README")))
(ert-deftest eldev-test-fixed-file-paths-4 ()
  (eldev--test-fileset "/files/README" '("files/README")))
(ert-deftest eldev-test-fixed-file-paths-5 ()
  (eldev--test-fileset "./files/xyz/README" '("files/xyz/README")))
(ert-deftest eldev-test-fixed-file-paths-6 ()
  (eldev--test-fileset "/files/xyz/README" '("files/xyz/README")))


;; Test file paths that match directories.
(ert-deftest eldev-test-directory-matches-1 ()
  (eldev--test-fileset "xyz" eldev--test-files-xyz-files))


;; Test that files don't get matched if their names are followed with a slash.
(ert-deftest eldev-test-matching-files-as-directories-1 ()
  (eldev--test-fileset "README/" nil))


;; Test simple composite filesets.
(ert-deftest eldev-test-composite-fileset-1 ()
  (eldev--test-fileset '(:or "/bar/" "/files/") (append eldev--test-bar-files eldev--test-files-files)))
(ert-deftest eldev-test-composite-fileset-2 ()
  (eldev--test-fileset '(:and "/bar/" "/files/") nil))
(ert-deftest eldev-test-composite-fileset-3 ()
  (eldev--test-fileset '(:or (:and "/bar/" "/files/") (:or "/bar/" "/files/")) (append eldev--test-bar-files eldev--test-files-files)))


;; Filesets that match a single file.

(ert-deftest eldev-test-single-file-1 ()
  (dolist (file eldev--test-all-files)
    (eldev--test-fileset (format "./%s" file) (list file))))

(ert-deftest eldev-test-single-file-2 ()
  (dolist (file eldev--test-all-files)
    (eldev--test-fileset (format "/%s" file) (list file))))


(provide 'test/fileset)
