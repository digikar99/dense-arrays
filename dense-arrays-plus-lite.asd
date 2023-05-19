(defsystem "dense-arrays-plus-lite"
  :depends-on ("dense-arrays"
               "extensible-optimizing-coerce"
               "extensible-compound-types-interfaces"
               "trivial-package-local-nicknames"
               "uiop")
  :pathname #P"plus/"
  :components ((:file "lite"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                            (5AM:*ON-ERROR* :DEBUG))
                                        (DOLIST (DENSE-ARRAYS:*ARRAY-LAYOUT*
                                                 '(:ROW-MAJOR :COLUMN-MAJOR))
                                         (FORMAT T \"Running tests with *ARRAY-LAYOUT* bound to ~S\"
                                                 DENSE-ARRAYS:*ARRAY-LAYOUT*)
                                         (5AM:RUN! :DENSE-ARRAYS)
                                         (5AM:RUN! :DENSE-ARRAYS-PLUS-LITE)))"))))
