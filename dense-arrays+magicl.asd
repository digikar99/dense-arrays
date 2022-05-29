
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
                :components ((:file "magicl")))))
