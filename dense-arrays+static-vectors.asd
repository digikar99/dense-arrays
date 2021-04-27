(defsystem "dense-arrays+static-vectors"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("dense-arrays"
               "static-vectors")
  :serial t
  :components ((:module "plus"
                :components ((:file "static-vectors"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                              (FIND-CLASS 'DENSE-ARRAYS::STATIC-DENSE-ARRAY))
                                            (5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                       (5AM:RUN :DENSE-ARRAYS))"))))
