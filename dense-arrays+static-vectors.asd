(defsystem "dense-arrays+static-vectors"
  :author "Shubhamkar B. Ayare"
  :description "Redefines dense-arrays:make-array and adds a few symbols for providing
compatibility layer for using static-vectors."
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("dense-arrays"
               "static-vectors"
               "trivial-garbage")
  :serial t
  :components ((:module "src"
                :components ((:file "static-vectors")))
               (:module "optim"
                :components ()))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((DENSE-ARRAYS:*USE-STATIC-VECTORS* T))
                                        (5AM:RUN :DENSE-ARRAYS))"))))
