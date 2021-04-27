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
                     :define-struct-with-required-slots
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

(define-struct-with-required-slots (dense-array (:include abstract-array)
                                                (:predicate dense-array-p)
                                                (:constructor nil)
                                                (:copier copy-dense-array))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (displaced-to nil :required t)
  (strides      nil :required t)
  (offsets      nil :required t :type list)
  (contiguous-p nil :required t)
  ;; (backend      *dense-array-backend* :read-only t :type symbol)
  (root-array   nil :required t))

(macrolet ((def (name)
             `(progn
                (defgeneric ,name (dense-array-class)))))
  (def backend-constructor)
  (def backend-storage-accessor)
  (def backend-storage-allocator)
  (def backend-storage-deallocator)
  (def backend-element-type-upgrader)
  (def backend-storage-type-inferrer-from-array)
  (def backend-storage-type-inferrer-from-array-type))

(defmacro define-dense-array-backend (dense-array-class
                                      &key constructor
                                        storage-type-inferrer-from-array-type
                                        storage-type-inferrer-from-array
                                        storage-accessor
                                        storage-allocator
                                        storage-deallocator
                                        element-type-upgrader)
  "
STORAGE-ALLOCATOR
  - a function with signature (SIZE &KEY ELEMENT-TYPE INITIAL-ELEMENT)
    that allocates a VECTOR of length SIZE of ELEMENT-TYPE with each element as
    INITIAL-ELEMENT for use as a STORAGE-VECTOR for the ABSTRACT-ARRAY.
STORAGE-DEALLOCATOR
  - A function to be called to delete the STORAGE when the ABSTRACT-ARRAY
    goes out of scope. This function should take only the STORAGE object
    as its argument. (See static-vectors.lisp and the DENSE-ARRAYS:MAKE-ARRAY
    function for reference.)
  "
  `(progn
     (defmethod backend-constructor ((array-class (eql ,dense-array-class)))
       ,constructor)
     (defmethod backend-storage-accessor ((array-class (eql ,dense-array-class)))
       ,storage-accessor)
     (defmethod backend-storage-allocator ((array-class (eql ,dense-array-class)))
       ,storage-allocator)
     (defmethod backend-storage-deallocator ((array-class (eql ,dense-array-class)))
       ,storage-deallocator)
     (defmethod backend-element-type-upgrader ((array-class (eql ,dense-array-class)))
       ,element-type-upgrader)
     (defmethod backend-storage-type-inferrer-from-array
         ((array-class (eql ,dense-array-class)))
       ,storage-type-inferrer-from-array)
     (defmethod backend-storage-type-inferrer-from-array-type
         ((array-class (eql ,dense-array-class)))
       ,storage-type-inferrer-from-array-type)))

(defstruct (standard-dense-array (:include dense-array)))

(defparameter *dense-array-class* (find-class 'standard-dense-array))
;;; Some implementations like CCL do not have a #'(setf cl:aref)
(declaim (inline cl-aref (setf cl-aref)))
(defun cl-aref (array index)
  (cl:row-major-aref array index))
(defun (setf cl-aref) (new array index)
  (setf (cl:row-major-aref array index) new))

(define-dense-array-backend *dense-array-class*
  :constructor 'make-standard-dense-array
  :storage-accessor 'cl-aref
  :storage-allocator 'cl:make-array
  :storage-deallocator nil
  :element-type-upgrader 'cl:upgraded-array-element-type
  :storage-type-inferrer-from-array
  (lambda (array)
    (declare (type abstract-array array)
             (optimize speed))
    `(cl:simple-array ,(array-element-type array) 1))
  :storage-type-inferrer-from-array-type
  (lambda (array-type)
    `(cl:simple-array ,(array-type-element-type array-type) 1)))

(defun simple-dense-array-p (object)
  (and (dense-array-p object)
       (loop :for o :of-type size :in (array-offsets object)
             :always (zerop o))
       (let ((total-size (array-total-size object)))
         (loop :for s :in (array-strides object)
               :for d :in (narray-dimensions object)
               :always (= s (/ total-size d))
               :do (setq total-size (floor total-size d))))))

(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
;;; TODO: Put simple-array type to use
(define-array-specialization-type simple-array (and standard-dense-array
                                                    simple-dense-array))

;; For internal usage
(define-array-specialization-type %dense-array dense-array)
