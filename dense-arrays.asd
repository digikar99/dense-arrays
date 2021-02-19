
(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("alexandria"
               ;; TODO: replace compiler-macro with trivial-form-type
               ;; after making the latter
               "compiler-macro"
               "fiveam"
               "iterate"
               "static-vectors"
               "trivial-garbage"
               "trivial-package-local-nicknames"
               "trivial-types"
               "uiop")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "dense-arrays")
                             (:file "broadcast")
                             (:file "do-arrays")
                             (:file "argwhere")
                             (:file "aref")
                             (:file "copy")))
               (:module "optim"
                :components ((:file "misc")
                             (:file "aref"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS)"))))
