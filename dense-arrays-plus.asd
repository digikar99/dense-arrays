(defsystem "dense-arrays-plus"
  :depends-on ("dense-arrays+static-vectors"
               "dense-arrays-plus-lite"
               "fiveam"
               "generic-cl"
               "py4cl2"
               "reader")
  :pathname #P"plus/"
  :serial t
  :components ((:file "package")
               (:file "py4cl2")
               (:file "generic-cl")
               (:file "reader"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS)"))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS-PLUS-LITE)"))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS-PLUS)"))))
