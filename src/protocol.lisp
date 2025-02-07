(in-package :dense-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct* (dense-array-metadata (:conc-name dam-))
    "
STORAGE-ACCESSOR:

A SYMBOL that is fbound to an accessor function that takes
 (STORAGE INDEX) as arguments and returns the element at INDEX in STORAGE.
The function is an accessor function in the sense that SYMBOL should also be
associated with (SETF SYMBOL) function that takes (NEW-VALUE STORAGE INDEX) as
arguments and sets the STORAGE element at INDEX to NEW-VALUE.
  This function is primarily used inside AREF, ROW-MAJOR-AREF and DO-ARRAYS,
and their SETF counterparts.
  See src/protocol.lisp and plus/cl-cuda.lisp for reference.

STORAGE-ALLOCATOR:

A SYMBOL fbound to a function with signature
  (SIZE &KEY ELEMENT-TYPE INITIAL-ELEMENT)
that allocates a VECTOR of length SIZE of ELEMENT-TYPE with each element as
INITIAL-ELEMENT for use as a STORAGE-VECTOR for the ABSTRACT-ARRAY.

STORAGE-DEALLOCATOR:

A symbol fbound to a function to be called
to delete the STORAGE when the ABSTRACT-ARRAY goes out of scope. This function should
take only the STORAGE object as its argument.
  Internally, this function plays a role in the finalizer of the garbage collection
using TRIVIAL-GARBAGE.
  See plus/static-vectors.lisp and the DENSE-ARRAYS:MAKE-ARRAY function for reference.

STORAGE-ELEMENT-TYPE-UPGRADER:

Equivalent to the CL:UPGRADED-ARRAY-ELEMENT-TYPE, this is a symbol fbound to a function
that takes a single argument element-type as input and returns the upgraded
array element type for the array class given by CLASS used for STORAGE.
The upgraded array element type is then stored in the dense-array object and
used for other tasks downstream.
  See plus/cl-cuda.lisp and the DENSE-ARRAYS:MAKE-ARRAY function for reference.

STORAGE-TYPE-INFERRER-FROM-ARRAY-TYPE:

This is a symbol fbound to a function that takes as input the ARRAY-TYPE and returns
the possibly specialized type of storage that the corresponding array object
will have. This is primarily used for optimization purposes inside DENSE-ARRAYS:DO-ARRAYS
and the compiler macros of DENSE-ARRAYS:AREF DENSE-ARRAYS:ROW-MAJOR-AREF and SETF
counterparts.
  See src/protocol.lisp, plus/cl-cuda.lisp, src/do-arrays.lisp and optim/aref.lisp
for reference."

    (name :required t :type symbol)
    (array-type :type symbol)
    (simple-array-type :type symbol)
    (dense-array-constructor :required t :type symbol)
    (storage-accessor :required t :type symbol)
    (storage-allocator :required t :type symbol)
    (storage-deallocator :required t :type symbol)
    (storage-element-type-upgrader
     :required t :type symbol :initform 'cl:upgraded-array-element-type)
    (storage-type-inferrer-from-array-type :required t :type symbol)
    (storage-type-inferrer-from-element-type :required t :type symbol))

  (define-orthogonally-specializing-type dense-array-metadata () ())

  (defvar *dam-hash-table* (make-hash-table))

  (defun dam-object (metadata-name &optional (error-if-not-exists t))
    (declare (cl:type symbol metadata-name))
    (multiple-value-bind (dam-object existsp)
        (gethash metadata-name *dam-hash-table*)
      (cond (existsp
             dam-object)
            (error-if-not-exists
             (error "No DENSE-ARRAY-METADATA exists with :NAME ~S" metadata-name))
            (t
             nil))))

  (defun (setf dam-object) (dam-object metadata-name)
    (declare (cl:type symbol metadata-name)
             (cl:type (or dense-array-metadata null) dam-object))
    (if dam-object
        (setf (gethash metadata-name *dam-hash-table*) dam-object)
        (remhash metadata-name *dam-hash-table*))))



(define-trait dense-array (abstract-arrays:array) ()

  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSET is the offset inside array-storage."
  ;; TODO: Add more documentation with a proper example
  ;; Like CL:ARRAY, DENSE-ARRAY can actually never be instantiated.
  ;; What can be instantiated is a CL:ARRAY with an ELEMENT-TYPE specified.
  ;; For DENSE-ARRAY, this ELEMENT-TYPE depends on the underlying storage itself.

  (dense-array-strides (dense-array) list)
  (dense-array-offset (dense-array) size)
  (dense-array-layout (dense-array) (member nil :row-major :column-major))
  ;; LAYOUT can be NIL in the case of a non SIMPLE-ARRAY
  ;; LAYOUT will be ROW-MAJOR or COLUMN-MAJOR only for SIMPLE-ARRAY
  (dense-array-root-array (dense-array) (or null dense-array))

;;; FIXME: This comment
;;; Below, we are using CLASS instead of CLASS-NAME because the CLASS objects
;;; will have a hierarchy, not the CLASS-NAMEs. This allows for partial specialization.
;;; An example of a preprovided partial specialization is STATIC-DENSE-ARRAY
;;; as a subclass of STANDARD-DENSE-ARRAY with the partial specializations:
;;; - STORAGE-ALLOCATOR
;;; - STORAGE-DEALLOCATOR

  (dense-array-metadata (dense-array) dense-array-metadata))

;;; STANDARD-DENSE-ARRAY

(defstruct*
    (standard-dense-array (:include abstract-arrays:abstract-array))
  (strides :required t :type list :read-only t)
  (offset :required t :type size :read-only t)
  (layout :required t :type (member nil :row-major :column-major) :read-only t)
  (root-array :required t :type (or null standard-dense-array) :read-only t)
  (metadata :required t :type dense-array-metadata :read-only t))

;;; Some implementations like CCL do not have a #'(setf cl:aref)
(declaim (inline cl-aref (setf cl-aref)))
(defun cl-aref (array index)
  (cl:row-major-aref array index))
(define-compiler-macro cl-aref (array index)
  `(cl:row-major-aref ,array ,index))
(defun (setf cl-aref) (new array index)
  (setf (cl:row-major-aref array index) new))
(define-compiler-macro (setf cl-aref) (new array index)
  `(setf (cl:row-major-aref ,array ,index) ,new))

(defun standard-dense-array-storage-type-inferrer-from-array-type (array-type)
  `(cl:simple-array ,(dense-array-type-element-type array-type) 1))

(defun standard-dense-array-storage-type-inferrer-from-element-type (element-type)
  `(cl:simple-array ,element-type 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (dam-object 'standard-dense-array)
        (make-dense-array-metadata
         :name 'standard-dense-array
         :dense-array-constructor 'make-standard-dense-array
         :storage-accessor 'cl-aref
         :storage-allocator 'cl:make-array
         :storage-deallocator nil
         :storage-element-type-upgrader 'cl:upgraded-array-element-type
         :storage-type-inferrer-from-array-type
         'standard-dense-array-storage-type-inferrer-from-array-type
         :storage-type-inferrer-from-element-type
         'standard-dense-array-storage-type-inferrer-from-element-type)))

(defvar *dense-array-metadata* (dam-object 'standard-dense-array)
  "Specifies the default value of METADATA in DENSE-ARRAYS:MAKE-ARRAY
and other functions. (TODO: Specify these other functions.)")
