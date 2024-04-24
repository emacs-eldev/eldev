(defun project-m-hello-to (whom)
  "Doctest me:
>> (project-m-hello-to \"world\")
=> \"Hello, world!\""
  (format "Hello, %s!" whom))

(provide 'project-m-advanced)
