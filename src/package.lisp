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

(in-suite :dense-arrays)

(defvar *use-static-vectors-alist* nil
  "An ALIST mapping package to a boolean. If the boolean corresponding to *PACKAGE* is true,
dense-arrays:make-array uses static-vectors to allocate the storage/displaced vector.
Can be overriden by both
  - binding *USE-STATIC-VECTORS*
  - providing keyword arg :STATIC to DENSE-ARRAYS:MAKE-ARRAY")

(defvar *use-static-vectors*)
(setf (documentation '*use-static-vectors* 'variable)
      "If T dense-arrays:make-array uses static-vectors to allocate the underlying
storage/displaced vector. Can be overriden by providing keyword arg :STATIC
to DENSE-ARRAYS:MAKE-ARRAY")

(define-symbol-macro use-static-vectors-p
    (if (boundp '*use-static-vectors*)
        *use-static-vectors*
        (cdr (assoc *package* *use-static-vectors-alist*))))

(defvar *use-static-vectors*)

(defmacro unless-static-vectors ((num-passes) &body body)
  `(if use-static-vectors-p
       (dotimes (i ,num-passes) (pass "Skipping for static vectors"))
       (locally ,@body)))

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

(define-struct-with-required-slots (backend (:constructor create-backend))
  "
STORAGE-ALLOCATOR
  - a function with signature (SIZE &KEY ELEMENT-TYPE INITIAL-ELEMENT)
    that allocates a VECTOR of length SIZE of ELEMENT-TYPE with each element as
    INITIAL-ELEMENT for use as a STORAGE-VECTOR for the ABSTRACT-ARRAY.

STORAGE-DEALLOCATOR (FIXME: A function that returns a function?)
  - A function to be called when the ABSTRACT-ARRAY goes out of scope.
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
  (apply #'cl:aref array subscripts))
(defun (setf cl-aref) (new array &rest subscripts)
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
                  `(cl:array ,(array-element-type array) 1))
                :storage-type-inferrer-from-array-type
                (lambda (array-type)
                  `(cl:array ,(array-type-element-type array-type) 1))))

(define-struct-with-required-slots (dense-array (:include abstract-array)
                                                (:predicate dense-array-p)
                                                (:constructor make-dense-array)
                                                (:copier copy-dense-array))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (displaced-to nil :required t :type (cl:simple-array * 1))
  (strides      nil :required t)
  (offsets      nil :required t :type list)
  (contiguous-p nil :required t)
  (backend      :cl :read-only t :type symbol)
  (root-array   nil :required t))

(defun standard-dense-array-p (object)
  (and (dense-array-p object)
       (eq :cl (dense-array-backend object))))

(defun simple-dense-array-p (object)
  (and (dense-array-p object)
       (loop :for o :of-type size :in (array-offsets object)
             :always (zerop o))
       (let ((total-size (array-total-size object)))
         (loop :for s :in (array-strides object)
               :for d :in (narray-dimensions object)
               :always (= s (/ total-size d))
               :do (setq total-size (floor total-size d))))))

(deftype standard-dense-array () `(and dense-array (satisfies standard-dense-array-p)))
(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
(define-array-specialization-type simple-array (and standard-dense-array
                                                    simple-dense-array))

;; For internal usage
(define-array-specialization-type %dense-array dense-array)
