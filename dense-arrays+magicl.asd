
(defsystem "dense-arrays+magicl"
  :author "Shubhamkar B. Ayare"
  :description "Exports 4 additional symbols from DENSE-ARRAYS package:
- MAGICL-FUNCALL
- MAGICL-DENSE-ARRAY
- MAGICL-ARRAY
- SIMPLE-MAGICL-ARRAY"
  :license "MIT"
  :version "0.0.0" ; alpha - no versioning maintained at the moment
  :depends-on ("dense-arrays"
               "magicl")
  :serial t
  :components ((:module "plus"
                :components ((:file "magicl")
                             (:file "magicl-utils"))))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Other tests won't pass because do-arrays is a compile time thing.
             ;; Or, they make certain assumptions about the backend.
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG)
                                            (DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                               'DENSE-ARRAYS:MAGICL-DENSE-ARRAY))
                                        (5AM:RUN 'DENSE-ARRAYS::BACKEND-INDEPENDENT)))"))))
