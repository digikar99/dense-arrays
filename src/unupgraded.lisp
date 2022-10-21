(in-package :dense-arrays)

(defclass unupgraded-dense-array-class (standard-dense-array-class) ())

(defclass unupgraded-dense-array (dense-array)
  ()
  (:metaclass unupgraded-dense-array-class))

(defmethod storage-element-type-upgrader ((class unupgraded-dense-array-class))
  'identity)

;; TODO: Prepare a SIMPLE-UNUPGRADED-ARRAY type
(define-array-specialization-type unupgraded-array unupgraded-dense-array)
(define-array-specialization-type simple-unupgraded-array (and unupgraded-dense-array
                                                               simple-dense-array))

(define-array-specializations (string) ())

(def-suite unupgraded-array :in :dense-arrays)
(in-suite unupgraded-array)

(def-test unupgraded-array/make-array ()

  (let ((*dense-array-class* 'unupgraded-dense-array))

    (is-true (make-array 2 :element-type 'string
                           :initial-element ""))
    (is-true (make-array 2 :element-type 'string
                           :initial-contents '("" "")))
    (is-true (make-array 2 :element-type 'string
                           :constructor (lambda (&rest args)
                                          (declare (ignore args))
                                          "")))

    (signals error
      (make-array 2 :element-type 'string :initial-element 0))
    (signals error
      (make-array 2 :element-type 'string :initial-contents '(0 0)))
    (signals error
      (make-array 2 :element-type 'string :constructor (lambda (&rest args)
                                                         (declare (ignore args))
                                                         0)))))

(def-test unupgraded-array/aref ()
  (let ((*dense-array-class* 'unupgraded-dense-array))
    (let ((array (make-array 2 :element-type 'string :initial-element "")))
      (signals error
        (setf (aref* array 0) 0))
      (signals error
        (setf (aref* array) 0))
      (signals error
        (setf (row-major-aref array 0) 0))
      (signals error
        (setf (row-major-aref (aref* array) 0) 0)))))

