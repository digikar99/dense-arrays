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

;; TODO: No separate test because no way to check if a vector is static-vector
;; https://github.com/sionescu/static-vectors/issues/13
;; In theory, can use a dynamic-variable inside the finalizer, but
;; performance plus lack of guarantees.
