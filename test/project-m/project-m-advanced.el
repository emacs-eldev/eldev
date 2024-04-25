(defvar project-m-hello-to-verb "Hello")

(defun project-m-hello-to (whom)
  "Doctest me:
>> (project-m-hello-to \"world\")
=> \"Hello, world!\""
  (format "%s, %s!" project-m-hello-to-verb whom))

(provide 'project-m-advanced)
