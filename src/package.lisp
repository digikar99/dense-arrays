#.(cl:let ((abstract-array-symbols '(:arrayp
                                     :array-dimensions
                                     :array-dimension
                                     :array-element-type
                                     :array-total-size
                                     :aref
                                     :array-rank
                                     :array-storage-set
                                     :array-storage-ref))
           (shadow-symbols '(:array
                             :simple-array
                             :array-storage-allocator
                             :array-storage-deallocator
                             :upgraded-array-element-type
                             :define-dense-array-backend-specialization
                             :make-backend
                             :*dense-array-backend*

                             :narray-dimensions
                             :array-displacement
                             :array-displaced-to
                             :array=
                             :row-major-aref
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
                     :element-type)
       (:local-nicknames (:cm :sandalphon.compiler-macro)
                         (:env :introspect-environment))))

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

(defparameter *dense-array-backends* nil)
(defparameter *dense-array-backend* :cl)

(define-struct-with-required-slots (backend (:constructor create-backend))
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
  (name                  nil :required t :type symbol)
  ;; We need an accessor with a setf expansion, to play nice in do-arrays
  (storage-accessor      nil :required t :type function-name)
  (storage-allocator     nil :required t :type function-designator)
  (storage-deallocator   nil :required t :type (or null function-designator))
  (element-type-upgrader nil :required t :type function-designator)
  (storage-type-inferrer-from-array nil :required t :type function-designator)
  (storage-type-inferrer-from-array-type nil :required t :type function-designator))

(defmethod print-object ((o backend) s)
  (print-unreadable-object (o s :type t :identity t)
    (write (backend-name o) :stream s)))

(defun make-backend (name &rest args &key storage-type-inferrer-from-array-type
                                       storage-type-inferrer-from-array
                                       storage-accessor
                                       storage-allocator
                                       storage-deallocator
                                       element-type-upgrader)
  (declare (ignore storage-type-inferrer-from-array-type
                   storage-type-inferrer-from-array
                   storage-accessor
                   storage-allocator
                   storage-deallocator
                   element-type-upgrader))
  (let ((backend (apply #'create-backend :name name args)))
    (removef *dense-array-backends* name :key #'backend-name)
    (push backend *dense-array-backends*)
    backend))

(define-condition no-existing-backend (error)
  ((name :initarg :name
         :accessor no-existing-backend-name))
  (:report (lambda (condition stream)
             (format stream "There is no backend with name ~S.
Existing backend names include:~{~^~%  ~S~}"
                     (no-existing-backend-name condition)
                     (mapcar #'backend-name *dense-array-backends*)))))

(defun find-backend (name)
  (or (find name *dense-array-backends* :key #'backend-name)
      (error 'no-existing-backend :name name)))

;;; Some implementations like CCL do not have a #'(setf cl:aref)
(declaim (inline cl-aref (setf cl-aref)))
(defun cl-aref (array &rest subscripts)
  (declare (optimize speed)
           (dynamic-extent subscripts))
  (apply #'cl:aref array subscripts))
(defun (setf cl-aref) (new array &rest subscripts)
  (declare (optimize speed)
           (dynamic-extent subscripts))
  (setf (apply #'cl:aref array subscripts) new))

(defparameter *standard-dense-array-backend*
  (make-backend :cl
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
                  `(cl:simple-array ,(array-type-element-type array-type) 1))))

(define-struct-with-required-slots (dense-array (:include abstract-array)
                                                (:predicate dense-array-p)
                                                (:constructor make-dense-array)
                                                (:copier copy-dense-array))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (displaced-to nil :required t)
  (strides      nil :required t)
  (offsets      nil :required t :type list)
  (contiguous-p nil :required t)
  (backend      *dense-array-backend* :read-only t :type symbol)
  (root-array   nil :required t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun backend-p-fn-name (backend-name)
    (check-type backend-name (and symbol (not (eql nil)))) ; any other names to disallow?
    (intern (uiop:strcat "DENSE-ARRAY-BACKEND-"
                         (write-to-string backend-name)
                         "-P")
            :dense-arrays)))

(defmacro define-dense-array-backend-specialization (backend-name)
  (check-type backend-name symbol)
  (let ((fn-name (backend-p-fn-name backend-name)))
    `(defun ,fn-name (object)
       (and (dense-array-p object)
            (eq ,backend-name (dense-array-backend object))))))

(defun simple-dense-array-p (object)
  (and (dense-array-p object)
       (loop :for o :of-type size :in (array-offsets object)
             :always (zerop o))
       (let ((total-size (array-total-size object)))
         (loop :for s :in (array-strides object)
               :for d :in (narray-dimensions object)
               :always (= s (/ total-size d))
               :do (setq total-size (floor total-size d))))))

(define-dense-array-backend-specialization :cl)
(deftype standard-dense-array () `(and dense-array (satisfies ,(backend-p-fn-name :cl))))
(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
;;; TODO: Put simple-array type to use
(define-array-specialization-type simple-array (and standard-dense-array
                                                    simple-dense-array))

;; For internal usage
(define-array-specialization-type %dense-array dense-array)
