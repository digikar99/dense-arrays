
(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("iterate"
               "alexandria"
               "fiveam"
               "trivial-types"
               "uiop")
  :components ((:file "dense-arrays")
               (:file "do-arrays")
               (:file "aref")
               (:file "copy")))
