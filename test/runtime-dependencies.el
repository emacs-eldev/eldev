(require 'test/common)


(ert-deftest eldev-runtime-dependencies-1 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("exec" "--dont-load"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))"
                          "(eldev-add-extra-dependencies 'runtime `(:package dependency-a :archive (\"archive-a\" . ,(expand-file-name \"../package-archive-a\"))))"
                          "(eldev-add-extra-dependencies 'runtime `(:package misc-a :archive (\"archive-b\" . ,(expand-file-name \"../package-archive-b\"))))"
                          "(eldev-load-extra-dependencies 'runtime)"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))")
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a" "misc-a")
      (should (string= stdout (eldev--test-lines "dependency-a 1.0" "misc-a 1.1")))
      (should (= exit-code 0)))))

(ert-deftest eldev-runtime-dependencies-2 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("exec" "--dont-load"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))"
                          "(eldev-add-extra-dependencies 'runtime `(:package dependency-a :archive (\"archive-b\" . ,(expand-file-name \"../package-archive-b\"))))"
                          "(eldev-add-extra-dependencies 'runtime `(:package misc-a :archive (\"archive-a\" . ,(expand-file-name \"../package-archive-a\"))))"
                          "(eldev-load-extra-dependencies 'runtime)"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))")
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a" "misc-a")
      (should (string= stdout (eldev--test-lines "dependency-a 1.1" "misc-a 1.0")))
      (should (= exit-code 0)))))

(ert-deftest eldev-runtime-dependencies-3 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("exec" "--dont-load"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))"
                          "(eldev-add-extra-dependencies 'runtime `(:package dependency-b :archive (\"archive-a\" . ,(expand-file-name \"../package-archive-a\"))))"
                          "(eldev-add-extra-dependencies 'runtime `(:package misc-a :archive (\"archive-b\" . ,(expand-file-name \"../package-archive-b\"))))"
                          "(eldev-load-extra-dependencies 'runtime)"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))")
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a" "dependency-b" "misc-a")
      (should (string= stdout (eldev--test-lines "dependency-a 1.0" "dependency-b 1.0" "misc-a 1.1")))
      (should (= exit-code 0)))))

(ert-deftest eldev-runtime-dependencies-4 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("exec" "--dont-load"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))"
                          "(eldev-add-extra-dependencies 'runtime `(:package dependency-b :archive (\"archive-b\" . ,(expand-file-name \"../package-archive-b\"))))"
                          "(eldev-add-extra-dependencies 'runtime `(:package misc-a :archive (\"archive-a\" . ,(expand-file-name \"../package-archive-a\"))))"
                          "(eldev-load-extra-dependencies 'runtime)"
                          "(when (require 'trivial-project nil t) (error \"must not be loaded\"))")
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a" "dependency-b" "misc-a")
      (should (string= stdout (eldev--test-lines "dependency-a 1.1" "dependency-b 1.1" "misc-a 1.0")))
      (should (= exit-code 0)))))


(provide 'test/runtime-dependencies)
