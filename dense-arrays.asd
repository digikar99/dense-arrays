
(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.1.3" ; beta
  :depends-on ("abstract-arrays"
               "polymorphic-functions"
               "alexandria"
               "compiler-macro-notes"
               "closer-mop"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               "fiveam"
               "iterate"
               "trivial-garbage"
               "cl-form-types"
               "trivial-types"
               "uiop")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "protocol")
                             (:file "types")
                             (:file "dense-arrays")
                             (:file "broadcast")
                             (:file "do-arrays")
                             (:file "argwhere")
                             (:file "aref")
                             (:file "copy")
                             (:file "unupgraded")))
               (:module "optim"
                :serial t
                :components ((:file "misc")
                             (:file "aref")
                             (:file "unupgraded"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                       (DOLIST (DENSE-ARRAYS:*ARRAY-LAYOUT*
                                                 '(:ROW-MAJOR :COLUMN-MAJOR))
                                         (FORMAT T \"Running tests with *ARRAY-LAYOUT* bound to ~S\"
                                                 DENSE-ARRAYS:*ARRAY-LAYOUT*)
                                         (5AM:RUN! :DENSE-ARRAYS)))"))))
