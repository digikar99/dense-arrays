(in-package :dense-arrays)
(in-suite :dense-arrays)

(defclass static-dense-array-class (standard-dense-array-class) ())

(defclass static-dense-array (dense-array)
  ()
  (:metaclass static-dense-array-class))

(defmethod storage-allocator ((class static-dense-array-class))
  'static-vectors:make-static-vector)
(defmethod storage-deallocator ((class static-dense-array-class))
  'static-vectors:free-static-vector)

(define-array-specialization-type static-array static-dense-array)
(define-array-specialization-type simple-static-array (and static-dense-array
                                                           simple-dense-array))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(static-array simple-static-array static-dense-array)
          (find-package :dense-arrays)))

;; TODO: No separate test because no way to check if a vector is static-vector
;; https://github.com/sionescu/static-vectors/issues/13
;; In theory, can use a dynamic-variable inside the finalizer, but
;; performance plus lack of guarantees.
