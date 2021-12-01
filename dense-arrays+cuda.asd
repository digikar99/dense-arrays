(defsystem "dense-arrays+cuda"
  :author "Shubhamkar B. Ayare"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("dense-arrays"
               "cl-cuda")
  :serial t
  :components ((:module "plus"
                :components ((:file "cl-cuda"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Other tests won't pass because do-arrays is a compile time thing.
             ;; Or, they make certain assumptions about the backend.
             (eval (read-from-string "(LET ((CL-CUDA:*SHOW-MESSAGES* NIL)
                                            (DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                               'DENSE-ARRAYS:CUDA-DENSE-ARRAY))
                                        (5AM:RUN 'DENSE-ARRAYS::BACKEND-INDEPENDENT)))"))))
 
