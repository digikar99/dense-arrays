(defsystem "dense-arrays"
  :author "Shubhamkar B. Ayare"
  :description "Numpy like array objects for Common Lisp"
  :license "MIT"
  :version "0.5.0" ; beta
  :defsystem-depends-on ("alternate-asdf-system-connections")
  :depends-on ("abstract-arrays"
               "alexandria"
               "closer-mop"
               "fiveam"
               "iterate"
               "peltadot"
               "peltadot-traits-library"
               "trivial-garbage"
               "trivial-types"
               "uiop"
               "float-features")
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

(define-system-connection "dense-arrays/magicl"
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

(define-system-connection "dense-arrays/static-vectors"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0"
  :requires ("dense-arrays" "static-vectors")
  :serial t
  :pathname #P"plus/"
  :components ((:file "static-vectors"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                              (FIND-CLASS 'DENSE-ARRAYS::STATIC-DENSE-ARRAY))
                                            (5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                        (5AM:RUN :DENSE-ARRAYS))"))))

(define-system-connection "dense-arrays/py4cl2"
  :requires ("dense-arrays" "py4cl2")
  :depends-on ("dense-arrays-plus-lite")
  :components ((:file "plus/py4cl2")))

(define-system-connection "dense-arrays/py4cl2-cffi"
  :requires ("dense-arrays" "py4cl2-cffi")
  :components ((:file "plus/py4cl2-cffi")))

(define-system-connection "dense-arrays/cl-cuda"
  :requires ("cl-cuda" "dense-arrays")
  :pathname #P"plus/"
  :components ((:file "cl-cuda"))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    ;; Other tests won't pass because do-arrays is a compile time thing.
                    ;; Or, they make certain assumptions about the backend.
                    (eval (read-from-string "(LET ((CL-CUDA:*SHOW-MESSAGES* NIL)
                                            (DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                               'DENSE-ARRAYS:CUDA-DENSE-ARRAY))
                                        (5AM:RUN 'DENSE-ARRAYS::BACKEND-INDEPENDENT)))"))))
