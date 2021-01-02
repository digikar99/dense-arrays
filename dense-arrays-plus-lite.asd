(defsystem "dense-arrays-plus-lite"
  :depends-on ("dense-arrays"
               "trivial-coerce"
               "uiop")
  :pathname #P"plus/"
  :components ((:file "lite"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS)"))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS-PLUS-LITE)"))))
