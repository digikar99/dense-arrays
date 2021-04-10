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
             (eval (read-from-string "(LET ((DENSE-ARRAYS:*DENSE-ARRAY-BACKEND* :STATIC))
                                       (5AM:RUN :DENSE-ARRAYS))"))))
