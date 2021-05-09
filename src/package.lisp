#.(cl:let ((abstract-array-symbols '(:arrayp
                                     :array-dimensions
                                     :array-dimension
                                     :array-element-type
                                     :array-total-size
                                     :aref
                                     :row-major-aref
                                     :array-rank
                                     :array-storage))
           (shadow-symbols '(:array
                             :simple-array
                             :array-storage-allocator
                             :array-storage-deallocator
                             :upgraded-array-element-type
                             :define-dense-array-backend-specialization
                             :define-dense-array-backend
                             :dense-array-type-class
                             :*dense-array-class*

                             :narray-dimensions
                             :array-displacement
                             :array-displaced-to
                             :array=
                             :make-array
                             :*array-element-print-format*
                             :print-array
                             :copy-array
                             :do-arrays)))
    `(uiop:define-package :dense-arrays
       (:mix :adhoc-polymorphic-functions :abstract-arrays
             :cl :iterate :alexandria :5am :trivial-types)
       (:export ,@shadow-symbols
                ,@abstract-array-symbols)
       (:shadow ,@shadow-symbols)
       (:import-from :abstract-arrays
                     :define-class-with-required-slots
                     :storage
                     :dimensions
                     :rank
                     :element-type
                     :intersection-type-types)
       (:import-from :trivial-form-type
                     #:primary-form-type
                     #:primary-form-typep)
       (:local-nicknames (:env :introspect-environment))))

(in-package :dense-arrays)

(def-suite :dense-arrays)
(def-suite backend-dependent :in :dense-arrays)
(def-suite backend-independent :in :dense-arrays
  :description "DENSE-ARRAY tests that are supposed to pass irrespective of the backend.")
