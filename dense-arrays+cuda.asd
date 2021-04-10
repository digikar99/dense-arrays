(defsystem "dense-arrays+cuda"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("dense-arrays"
               "trivial-package-local-nicknames"
               "cl-cuda")
  :serial t
  :components ((:module "plus"
                :components ((:file "cl-cuda"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Other tests won't pass because do-arrays is a compile time thing.
             ;; Or, they make certain assumptions about the backend.
             (eval (read-from-string "(PROGN
                                        (IN-PACKAGE :DENSE-ARRAYS)
                                        (LET ((CL-CUDA:*SHOW-MESSAGES* NIL)
                                              (*DENSE-ARRAY-BACKEND* :CUDA))
                                          (5AM:RUN 'BACKEND-INDEPENDENT)))"))))
 
