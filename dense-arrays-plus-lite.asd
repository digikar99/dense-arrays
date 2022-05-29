(defsystem "dense-arrays-plus-lite"
  :depends-on ("dense-arrays"
               "trivial-coerce"
               "uiop")
  :pathname #P"plus/"
  :components ((:file "lite"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                            (5AM:*ON-ERROR* :DEBUG))
                                        (5AM:RUN! :DENSE-ARRAYS)
                                        (5AM:RUN! :DENSE-ARRAYS-PLUS-LITE))"))))
