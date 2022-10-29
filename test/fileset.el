(require 'eldev)
(require 'ert)


(defvar eldev--test-pretend-files   '("AA.fake"
                                      "bb.fake"
                                      "bar/Abc.fake"
                                      "bar/z.fake"
                                      "files/A.fake"
                                      "files/B.fake"
                                      "files/Z.fake"
                                      "files/xyz/a-fake"))

(defvar eldev--test-all-files       '("AA.fake"
                                      "README"
                                      "bb.fake"
                                      "bar/Abc.fake"
                                      "bar/README"
                                      "bar/z.fake"
                                      "files/A.fake"
                                      "files/B.fake"
                                      "files/README"
                                      "files/Z.fake"
                                      ;; Added `zzz' to test ordering: must come before
                                      ;; subdirectories despite the name.
                                      "files/zzz"
                                      "files/xyz/README"
                                      "files/xyz/a-fake"))
(defvar eldev--test-root-files      '("AA.fake"
                                      "README"
                                      "bb.fake"))
(defvar eldev--test-bar-files       '("bar/Abc.fake"
                                      "bar/README"
                                      "bar/z.fake"))
(defvar eldev--test-files-files     '("files/A.fake"
                                      "files/B.fake"
                                      "files/README"
                                      "files/Z.fake"
                                      "files/zzz"
                                      "files/xyz/README"
                                      "files/xyz/a-fake"))
(defvar eldev--test-files-xyz-files '("files/xyz/README"
                                      "files/xyz/a-fake"))


(defmacro eldev--do-test-fileset (fileset all-files expected-files)
  `(let* ((all-files      ,all-files)
          (excluded-files (eldev-filter (not (member it (eldev-listify ,expected-files))) all-files))
          (file-types     (if eldev-pretend-files
                              (if eldev-consider-only-pretend-files "pretend files only" "real and pretend files")
                            "real files")))
     (condition-case error
         (progn (should (equal (eldev-find-files ,fileset nil "test/files") ,expected-files))
                (should (equal (eldev-filter-files all-files ,fileset nil "test/files") ,expected-files)))
       (error (eldev-error "Failed for fileset `%S' for %s" ,fileset file-types)
              (signal (car error) (cdr error))))
     (condition-case error
         (progn (should (equal (eldev-find-files '(:not ,fileset) nil "test/files") excluded-files))
                (should (equal (eldev-filter-files all-files '(:not ,fileset) nil "test/files") excluded-files)))
       (error (eldev-error "Failed for fileset `(:not %S)' for %s" ,fileset file-types)
              (signal (car error) (cdr error))))))

(defmacro eldev--test-fileset (fileset expected-files)
  ;; Test `eldev-find-files' and `eldev-filter-files' on FILESET and
  ;; its negation.
  `(progn
     (eldev--do-test-fileset ,fileset
                             (eldev-filter (not (string-match-p "fake" it)) eldev--test-all-files)
                             (eldev-filter (not (string-match-p "fake" it)) ,expected-files))
     (let ((eldev-pretend-files eldev--test-pretend-files))
       (let ((eldev-consider-only-pretend-files t))
         (eldev--do-test-fileset ,fileset
                                 (eldev-filter (string-match-p "fake" it) eldev--test-all-files)
                                 (eldev-filter (string-match-p "fake" it) ,expected-files)))
       (let ((eldev-consider-only-pretend-files nil))
         (eldev--do-test-fileset ,fileset eldev--test-all-files ,expected-files)))))


(ert-deftest eldev-empty-fileset-nil-1 ()
  (eldev--test-fileset nil nil))

(ert-deftest eldev-empty-fileset-1 ()
  (eldev--test-fileset "!*" nil))
(ert-deftest eldev-empty-fileset-2 ()
  (eldev--test-fileset "!**" nil))
(ert-deftest eldev-empty-fileset-3 ()
  (eldev--test-fileset "!**/*" nil))
(ert-deftest eldev-empty-fileset-4 ()
  (eldev--test-fileset "!./*" nil))
(ert-deftest eldev-empty-fileset-5 ()
  (eldev--test-fileset "!./**" nil))
(ert-deftest eldev-empty-fileset-6 ()
  (eldev--test-fileset "!./**/*" nil))
(ert-deftest eldev-empty-fileset-7 ()
  (eldev--test-fileset "!/*" nil))
(ert-deftest eldev-empty-fileset-8 ()
  (eldev--test-fileset "!/**" nil))
(ert-deftest eldev-empty-fileset-9 ()
  (eldev--test-fileset "!/**/*" nil))

;; Some files are included first, but later everything gets excluded.
(ert-deftest eldev-empty-fileset-postfactum-exclusion-1 ()
  (eldev--test-fileset '("README" "!*") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-2 ()
  (eldev--test-fileset '("README" "!**") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-3 ()
  (eldev--test-fileset '("README" "!**/*") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-4 ()
  (eldev--test-fileset '("README" "!./**") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-5 ()
  (eldev--test-fileset '("README" "!./**/*") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-6 ()
  (eldev--test-fileset '("README" "!/**") nil))
(ert-deftest eldev-empty-fileset-postfactum-exclusion-7 ()
  (eldev--test-fileset '("README" "!/**/*") nil))

(ert-deftest eldev-everything-fileset-1 ()
  (eldev--test-fileset "*" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-2 ()
  (eldev--test-fileset "**" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-3 ()
  (eldev--test-fileset "**/*" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-4 ()
  (eldev--test-fileset "./**" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-5 ()
  (eldev--test-fileset "./**/*" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-6 ()
  (eldev--test-fileset "/**" eldev--test-all-files))
(ert-deftest eldev-everything-fileset-7 ()
  (eldev--test-fileset "/**/*" eldev--test-all-files))

;; Some files are excluded first, but later everything gets included.
(ert-deftest eldev-everything-fileset-postfactum-inclusion-1 ()
  (eldev--test-fileset '("!README" "*") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-2 ()
  (eldev--test-fileset '("!README" "**") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-3 ()
  (eldev--test-fileset '("!README" "**/*") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-4 ()
  (eldev--test-fileset '("!README" "./**") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-5 ()
  (eldev--test-fileset '("!README" "./**/*") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-6 ()
  (eldev--test-fileset '("!README" "/**") eldev--test-all-files))
(ert-deftest eldev-everything-fileset-postfactum-inclusion-7 ()
  (eldev--test-fileset '("!README" "/**/*") eldev--test-all-files))

;; Test redundant (more than one in a row) '**'.
(ert-deftest eldev-redundant-**-1 ()
  (eldev--test-fileset "**/**/**/*" eldev--test-all-files))
(ert-deftest eldev-redundant-**-2 ()
  (eldev--test-fileset "!**/**/**/*" nil))


;; Test fixed file and directory paths.
(ert-deftest eldev-fixed-file-paths-1 ()
  (eldev--test-fileset "./README" '("README")))
(ert-deftest eldev-fixed-file-paths-2 ()
  (eldev--test-fileset "/README" '("README")))
(ert-deftest eldev-fixed-file-paths-3 ()
  (eldev--test-fileset "./files/README" '("files/README")))
(ert-deftest eldev-fixed-file-paths-4 ()
  (eldev--test-fileset "/files/README" '("files/README")))
(ert-deftest eldev-fixed-file-paths-5 ()
  (eldev--test-fileset "./files/xyz/README" '("files/xyz/README")))
(ert-deftest eldev-fixed-file-paths-6 ()
  (eldev--test-fileset "/files/xyz/README" '("files/xyz/README")))


;; Test file paths that match directories.
(ert-deftest eldev-directory-matches-1 ()
  (eldev--test-fileset "xyz" eldev--test-files-xyz-files))


;; Test that files don't get matched if their names are followed with a slash.
(ert-deftest eldev-matching-files-as-directories-1 ()
  (eldev--test-fileset "README/" nil))


;; Test simple composite filesets.
(ert-deftest eldev-composite-fileset-1 ()
  (eldev--test-fileset '(:or "/bar/" "/files/") (append eldev--test-bar-files eldev--test-files-files)))
(ert-deftest eldev-composite-fileset-2 ()
  (eldev--test-fileset '(:and "/bar/" "/files/") nil))
(ert-deftest eldev-composite-fileset-3 ()
  (eldev--test-fileset '(:or (:and "/bar/" "/files/") (:or "/bar/" "/files/")) (append eldev--test-bar-files eldev--test-files-files)))


;; Filesets that match a single file.

(ert-deftest eldev-single-file-1 ()
  (dolist (file eldev--test-all-files)
    (eldev--test-fileset (format "./%s" file) (list file))))

(ert-deftest eldev-single-file-2 ()
  (dolist (file eldev--test-all-files)
    (eldev--test-fileset (format "/%s" file) (list file))))


(ert-deftest eldev-pretend-file-matching-real-file ()
  (let ((eldev-pretend-files               '("README"))
        (eldev-consider-only-pretend-files nil))
    (eldev--do-test-fileset '("/README") (eldev-filter (not (string-match-p "fake" it)) eldev--test-all-files) '("README"))))


(provide 'test/fileset)
