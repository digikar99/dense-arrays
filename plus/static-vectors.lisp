(in-package :dense-arrays)
(in-suite :dense-arrays)

(make-backend :static
              :storage-accessor 'cl-aref
              :storage-allocator 'static-vectors:make-static-vector
              :storage-deallocator 'static-vectors:free-static-vector
              :element-type-upgrader 'cl:upgraded-array-element-type
              :storage-type-inferrer-from-array
              (lambda (array)
                (declare (type abstract-array array)
                         (optimize speed))
                `(cl:array ,(array-element-type array) 1))
              :storage-type-inferrer-from-array-type
              (lambda (array-type)
                `(cl:array ,(array-type-element-type array-type) 1)))

(defun static-dense-array-p (array)
  (and (dense-array-p array)
       (eq :static (dense-array-backend array))))
(deftype static-dense-array () `(satisfies static-dense-array-p))
(define-array-specialization-type static-array (and dense-array static-dense-array))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'static-array :dense-arrays))

;; TODO: No separate test because no way to check if a vector is static-vector
;; https://github.com/sionescu/static-vectors/issues/13
;; In theory, can use a dynamic-variable inside the finalizer, but
;; performance plus lack of guarantees.
