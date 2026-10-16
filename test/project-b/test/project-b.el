;; -*- lexical-binding: t -*-

(require 'project-b)
(require 'ert)

;; One test fails.

(ert-deftest project-b-test-hello ()
  (should (string= (project-b-hello) "Hello")))

(ert-deftest project-b-test-triviality-failing ()
  (should nil))

(ert-deftest project-b-test-unserializable-form-failing ()
  (with-temp-buffer
    ;; This assertion obviously fails, but most importantly it cannot be serialized/unserialized, see issue
    ;; #124.  Before 1.11.4 this would affect previous test result storing, see `eldev-test-read-ert-results'.
    (should (null (current-buffer)))))
