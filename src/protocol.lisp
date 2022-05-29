(in-package :dense-arrays)

(defclass dense-array-class (abstract-array-class) ())

(define-array-class dense-array
  ;; TODO: Add more documentation with a proper example
  ;; Like CL:ARRAY, DENSE-ARRAY can actually never be instantiated.
  ;; What can be instantiated is a CL:ARRAY with an ELEMENT-TYPE specified.
  ;; For DENSE-ARRAY, this ELEMENT-TYPE depends on the underlying storage itself.

  ((strides      :required t)
   (offsets      :required t)
   (layout       :required t
                 ;; LAYOUT can be NIL in the case of a non SIMPLE-ARRAY
                 ;; LAYOUT will be ROW-MAJOR or COLUMN-MAJOR only for SIMPLE-ARRAY
                 :type (member nil :row-major :column-major))
   (root-array   :required t))
  (:documentation "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."))

;;; Below, we are using CLASS instead of CLASS-NAME because the CLASS objects
;;; will have a hierarchy, not the CLASS-NAMEs. This allows for partial specialization.
;;; An example of a preprovided partial specialization is STATIC-DENSE-ARRAY
;;; as a subclass of STANDARD-DENSE-ARRAY with the partial specializations:
;;; - STORAGE-ALLOCATOR
;;; - STORAGE-DEALLOCATOR

(defgeneric storage-accessor (class)
  (:documentation "Returns a SYMBOL that is fbound to an accessor function that takes
 (STORAGE INDEX) as arguments and returns the element at INDEX in STORAGE.
The function is an accessor function in the sense that SYMBOL should also be
associated with (SETF SYMBOL) function that takes (NEW-VALUE STORAGE INDEX) as
arguments and sets the STORAGE element at INDEX to NEW-VALUE.
  This function is primarily used inside AREF, ROW-MAJOR-AREF and DO-ARRAYS,
and their SETF counterparts.
  See src/protocol.lisp and plus/cl-cuda.lisp for reference."))

(defgeneric storage-allocator (class)
  (:documentation "Returns a symbol fbound to a function with signature
  (SIZE &KEY ELEMENT-TYPE INITIAL-ELEMENT)
that allocates a VECTOR of length SIZE of ELEMENT-TYPE with each element as
INITIAL-ELEMENT for use as a STORAGE-VECTOR for the ABSTRACT-ARRAY."))

(defgeneric storage-deallocator (class)
  (:documentation "Returns either NIL or a symbol fbound to a function to be called
to delete the STORAGE when the ABSTRACT-ARRAY goes out of scope. This function should
take only the STORAGE object as its argument.
  Internally, this function plays a role in the finalizer of the garbage collection
using TRIVIAL-GARBAGE.
  See plus/static-vectors.lisp and the DENSE-ARRAYS:MAKE-ARRAY function for reference."))

(defgeneric storage-element-type-upgrader (class)
  (:documentation "Equivalent to the CL:UPGRADED-ARRAY-ELEMENT-TYPE, this returns a function
that takes a single argument element-type as input and returns the upgraded
array element type for the array class given by CLASS used for STORAGE.
The upgraded array element type is then stored in the dense-array object and
used for other tasks downstream.
  See plus/cl-cuda.lisp and the DENSE-ARRAYS:MAKE-ARRAY function for reference."))

(defgeneric storage-type-inferrer-from-array-type (class)
  (:documentation "This should return a function that takes as input the ARRAY-TYPE and returns
the possibly specialized type of storage that the corresponding array object
will have. This is primarily used for optimization purposes inside DENSE-ARRAYS:DO-ARRAYS
and the compiler macros of DENSE-ARRAYS:AREF DENSE-ARRAYS:ROW-MAJOR-AREF and SETF
counterparts.
  See src/protocol.lisp, plus/cl-cuda.lisp, src/do-arrays.lisp and optim/aref.lisp
for reference."))


(macrolet ((def (name)
             `(progn
                (defmethod ,name (class)
                  (if (subtypep class 'dense-array)
                      (error "Method ~S has not been specialized for ~S"
                             ',name (class-of class))
                      (error "Illegal to call ~S on something that is not the name of a
subclass of DENSE-ARRAY." ',name))))))
  (def dense-array-constructor)
  (def storage-accessor)
  (def storage-allocator)
  (def storage-deallocator)
  (def storage-element-type-upgrader)
  (def storage-type-inferrer-from-array-type))


;;; STANDARD-DENSE-ARRAY

(defclass standard-dense-array-class (dense-array-class) ())
(defclass standard-dense-array (dense-array)
  ()
  (:metaclass standard-dense-array-class))

(defvar *dense-array-class* (find-class 'standard-dense-array))

;;; Some implementations like CCL do not have a #'(setf cl:aref)
(declaim (inline cl-aref (setf cl-aref)))
(defun cl-aref (array index)
  (cl:row-major-aref array index))
(defun (setf cl-aref) (new array index)
  (setf (cl:row-major-aref array index) new))

(defmethod storage-accessor ((class standard-dense-array-class))
  'cl-aref)
(defmethod storage-allocator ((class standard-dense-array-class))
  'cl:make-array)
(defmethod storage-deallocator ((class standard-dense-array-class))
  nil)
(defmethod storage-element-type-upgrader ((class standard-dense-array-class))
  'cl:upgraded-array-element-type)
(defmethod storage-type-inferrer-from-array-type ((class standard-dense-array-class))
  (lambda (array-type)
    `(cl:simple-array ,(array-type-element-type array-type) 1)))

