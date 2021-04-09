
(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ((:version "asdf" "3.3.4")
               "abstract-arrays"
               "adhoc-polymorphic-functions"
               "alexandria"
               ;; TODO: replace compiler-macro with trivial-form-type
               ;; after making the latter
               "compiler-macro"
               "fiveam"
               "iterate"
               "trivial-garbage"
               "trivial-package-local-nicknames"
               "trivial-types"
               "uiop")
  :serial t
  :components ((:module "src"
                        :serial t
                :components ((:file "package")
                             (:file "dense-arrays")
                             (:file "broadcast")
                             (:file "do-arrays")
                             (:file "argwhere")
                             (:file "aref")
                             (:file "copy")))
               (:module "optim"
                        :serial t
                :components ((:file "misc")
                             (:file "aref"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(5AM:RUN :DENSE-ARRAYS)"))))
