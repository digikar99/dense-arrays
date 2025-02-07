(in-package :dense-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (dam-object 'unupgraded-dense-array)
        (let ((dam-object (copy-dense-array-metadata (dam-object 'standard-dense-array))))
          (setf (dam-storage-element-type-upgrader dam-object) 'identity)
          dam-object))
  (define-dense-array-types standard-dense-array unupgraded-dense-array
    unupgraded-array simple-unupgraded-array))

;; TODO: Prepare a SIMPLE-UNUPGRADED-ARRAY type
(define-array-specializations (string) ())

(def-suite unupgraded-array :in :dense-arrays)
(in-suite unupgraded-array)

(def-test unupgraded-array/make-array ()

  (let ((*dense-array-metadata* 'unupgraded-dense-array))

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
  (let ((*dense-array-metadata* 'unupgraded-dense-array))
    (let ((array (make-array 2 :element-type 'string :initial-element "")))
      (signals error
        (setf (aref* array 0) 0))
      (signals error
        (setf (aref* array) 0))
      (signals error
        (setf (row-major-aref array 0) 0))
      (signals error
        (setf (row-major-aref (aref* array) 0) 0)))))

