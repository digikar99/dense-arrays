(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.3.0" ; beta
  :defsystem-depends-on ("asdf-system-connections")
  :depends-on ("abstract-arrays"
               "polymorphic-functions"
               "alexandria"
               "compiler-macro-notes"
               "closer-mop"
               "extensible-compound-types"
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

(defsystem-connection "dense-arrays/magicl"
  :requires ("dense-arrays" "magicl")
  :description "Exports 4 additional symbols from DENSE-ARRAYS package:
- MAGICL-FUNCALL
- MAGICL-DENSE-ARRAY
- MAGICL-ARRAY
- SIMPLE-MAGICL-ARRAY"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :serial t
  :components ((:module "plus"
                :components ((:file "magicl")))))

(defsystem-connection "dense-arrays/static-vectors"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0"
  :requires ("dense-arrays" "static-vectors")
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
