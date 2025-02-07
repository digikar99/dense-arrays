#.(cl:let ((abstract-array-symbols '("ARRAYP"
                                     "ARRAY-DIMENSIONS"
                                     "NARRAY-DIMENSIONS"
                                     "ARRAY-DIMENSION"
                                     "ARRAY-ELEMENT-TYPE"
                                     "ARRAY-TOTAL-SIZE"
                                     "AREF"
                                     "ROW-MAJOR-AREF"
                                     "ARRAY-RANK"
                                     "ARRAY-STORAGE"
                                     "DEFINE-ARRAY-CLASS"
                                     "ARRAY-LAYOUT"
                                     "INVALID-ARRAY-INDEX"))
           (shadow-symbols '("ARRAY"
                             "SIMPLE-ARRAY"
                             "AREF*"

                             "ARRAY-STORAGE-ALLOCATOR"
                             "ARRAY-STORAGE-DEALLOCATOR"
                             "DENSE-ARRAY-TYPE-CLASS"
                             "STANDARD-DENSE-ARRAY"
                             "STANDARD-DENSE-ARRAY-CLASS"
                             "*DENSE-ARRAY-CLASS*"

                             "STORAGE-ACCESSOR"
                             "STORAGE-ALLOCATOR"
                             "STORAGE-DEALLOCATOR"
                             "STORAGE-ELEMENT-TYPE-UPGRADER"
                             "STORAGE-TYPE-INFERRER-FROM-ARRAY-TYPE"

                             "ARRAY-DISPLACEMENT"
                             "ARRAY-DISPLACED-TO"
                             "ARRAY-OFFSET"
                             "ARRAY-STRIDE"
                             "ARRAY-STRIDES"
                             "ARRAY="
                             "MAKE-ARRAY"
                             "*ARRAY-ELEMENT-TYPE*"
                             "*ARRAY-ELEMENT-TYPE-ALIST*"
                             "*ARRAY-ELEMENT-PRINT-FORMAT*"
                             "*ARRAY-LAYOUT*"
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
    `(peltadot/utils:defpackage :dense-arrays
       (:shadowing-import-exported-symbols #:iterate)
       (:use #:abstract-arrays
             #:peltadot
             #:iterate
             #:alexandria
             #:5am)
       (:export ,@shadow-symbols
                ,@abstract-array-symbols
                #:*print-tail-length*)
       (:shadow ,@shadow-symbols)
       (:shadowing-import-from #:cl #:ftype)
       (:shadowing-import-from #:peltadot
                               #:subtypep
                               #:type=
                               #:named-lambda)
       (:shadowing-import-from #:abstract-arrays

                               #:storage
                               #:dimensions
                               #:rank
                               #:element-type
                               #:index

                               #:array-dimension
                               #:array-dimensions
                               #:array-rank
                               #:array-element-type
                               #:array-total-size
                               #:array-storage

                               #:arrayp
                               #:aref
                               #:row-major-aref

                               #:defstruct*)
       (:import-from #:trivial-types
                     #:function-designator)
       (:local-nicknames
        (:cl-form-types #:peltadot/form-types))))

(in-package :dense-arrays)

(defvar *array-element-type*)
(defvar *array-layout* :row-major
  "Specifies the default layout constructed by DENSE-ARRAYS:MAKE-ARRAY and
constructor functions like ASARRAY, ZEROS, ONES, etc in the
DENSE-ARRAYS-PLUS-LITE package.")

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

(deftype size () `(unsigned-byte 62))
(deftype int-index () `(signed-byte 62))

(defmacro the-size (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl cl:the
    size ,form))

(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl cl:the
    int-index ,form))

(deftype int32 () `(signed-byte 32))
(deftype uint32 () `(unsigned-byte 32))
(deftype int64 () `(signed-byte 64))
(deftype uint64 () `(unsigned-byte 64))
