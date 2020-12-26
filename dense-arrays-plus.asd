(defsystem "dense-arrays-plus"
  :depends-on ("dense-arrays-plus-lite"
               "fiveam"
               "generic-cl"
               "py4cl2")
  :pathname #P"plus/"
  :components ((:file "package")
               (:file "py4cl2")
               (:file "generic-cl"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS)"))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS-PLUS-LITE)"))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS-PLUS)"))))
