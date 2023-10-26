;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-plugins-1 ()
  (eldev--test-run "trivial-project" ("plugins")
    (should (string-prefix-p "No plugins" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-plugins-2 ()
  (eldev--test-run "trivial-project" ("--quiet" "plugins")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-plugins-3 ()
  ;; It's fine to activate the plugin like this in a non-integration
  ;; test: it won't download third-party packages if command `test' is
  ;; not used.
  (eldev--test-run "trivial-project" ("--setup" `(eldev-use-plugin 'undercover) "plugins")
    (should (string-prefix-p "undercover" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-plugins-4 ()
  (eldev--test-run "trivial-project" ("--setup" `(eldev-use-plugin 'undercover) "--quiet" "plugins")
    (should (string= stdout "undercover\n"))
    (should (= exit-code 0))))


(provide 'test/plugins)
