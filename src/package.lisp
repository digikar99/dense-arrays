#.(cl:let ((abstract-array-symbols '("ARRAYP"
                                     "ARRAY-DIMENSIONS"
                                     "ARRAY-DIMENSION"
                                     "ARRAY-ELEMENT-TYPE"
                                     "ARRAY-TOTAL-SIZE"
                                     "AREF"
                                     "ROW-MAJOR-AREF"
                                     "ARRAY-RANK"
                                     "ARRAY-STORAGE"
                                     "DEFINE-ARRAY-CLASS"))
           (shadow-symbols '("ARRAY"
                             "SIMPLE-ARRAY"

                             "ARRAY-STORAGE-ALLOCATOR"
                             "ARRAY-STORAGE-DEALLOCATOR"
                             "DENSE-ARRAY-TYPE-CLASS"
                             "STANDARD-DENSE-ARRAY"
                             "STANDARD-DENSE-ARRAY-CLASS"
                             "*DENSE-ARRAY-CLASS*"

                             "ARRAY-DISPLACEMENT"
                             "ARRAY-DISPLACED-TO"
                             "ARRAY-OFFSET"
                             "ARRAY-OFFSETS"
                             "ARRAY-STRIDE"
                             "ARRAY-STRIDES"
                             "NARRAY-DIMENSIONS"
                             "ARRAY="
                             "MAKE-ARRAY"
                             "*ARRAY-ELEMENT-TYPE*"
                             "*ARRAY-ELEMENT-TYPE-ALIST*"
                             "*ARRAY-ELEMENT-PRINT-FORMAT*"
                             "PRINT-ARRAY"
                             "COPY-ARRAY"
                             "COPY-DENSE-ARRAY"
                             "DO-ARRAYS"
                             "BROADCAST-ARRAY"
                             "BROADCAST-ARRAYS"
                             "BROADCAST-COMPATIBLE-P"

                             "UNUPGRADED-DENSE-ARRAY"
                             "UNUPGRADED-ARRAY"
                             "SIMPLE-UNUPGRADED-ARRAY")))
    `(uiop:define-package :dense-arrays
       (:mix :polymorphic-functions :abstract-arrays
             :cl :iterate :alexandria :5am :trivial-types)
       (:export ,@shadow-symbols
                ,@abstract-array-symbols)
       (:shadow ,@shadow-symbols)
       (:import-from :abstract-arrays
                     #:+abstract-array-slot-order+
                     #:define-ordered-class-with-required-slots
                     #:storage
                     #:dimensions
                     #:rank
                     #:element-type
                     #:intersection-type-types)
       (:import-from :cl-form-types
                     #:nth-form-type)
       (:import-from :polymorphic-functions
                     #:typexpand
                     #:policy-quality
                     #:optim-speed
                     #:env)))

(in-package :dense-arrays)

(defvar *array-element-type*)

(setf (documentation '*array-element-type* 'variable)
      "If BOUND, this is the default value of the ELEMENT-TYPE or TYPE argument.
Overrides *ARRAY-ELEMENT-TYPE-ALIST*.
Is overriden by explicitly passing an ELEMENT-TYPE or TYPE argument.")

(defvar *array-element-type-alist* nil
  "An ALIST mapping package to the default element-type used in that package.
(Inspired from SWANK:*READTABLE-ALIST*)
Overrides none.
Is overriden by *ARRAY-ELEMENT-TYPE* when bound, or by explicitly passing an
  ELEMENT-TYPE or TYPE argument.")

(define-symbol-macro package-local-element-type
    (cdr (assoc *package* *array-element-type-alist*)))

(define-symbol-macro default-element-type
    (or (when (boundp '*array-element-type*)
          *array-element-type*)
        package-local-element-type
        t))

(def-suite :dense-arrays)
(def-suite backend-dependent :in :dense-arrays)
(def-suite backend-independent :in :dense-arrays
  :description "DENSE-ARRAY tests that are supposed to pass irrespective of the backend.")

(in-suite :dense-arrays)

(deftype size () `(unsigned-byte 62))
(deftype int-index () `(signed-byte 62))

(defmacro the-size (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    size ,form))

(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    int-index ,form))

(deftype int32 () `(signed-byte 32))
(deftype uint32 () `(unsigned-byte 32))
(deftype int64 () `(signed-byte 64))
(deftype uint64 () `(unsigned-byte 64))
