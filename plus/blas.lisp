;; Example file not intended to be included in dense-arrays or derivative systems yet
(in-package :dense-arrays-plus)

(cffi:define-foreign-library blas
  (:unix "/usr/lib/x86_64-linux-gnu/libgslcblas.so.0"))

(cffi:define-foreign-library blas
  (:unix "/home/shubhamkar/miniconda3/lib/libcblas.so"))

(cffi:use-foreign-library blas)

(defun ptr (array)
  (static-vectors:static-vector-pointer (array-displaced-to array)))

(defun m+ (a b &optional out)
  (declare (type (array single-float) a b out)
           (optimize speed))
  (let ((b (or out (copy-array b))))
    (cffi:foreign-funcall "cblas_saxpy"
                          :int (array-total-size a)
                          :float 1.0e0
                          :pointer (the sb-sys:system-area-pointer (ptr a))
                          :int 1
                          :pointer (the sb-sys:system-area-pointer (ptr b))
                          :int 1
                          :void)
    b))

(defun cl-m+ (a b &optional out)
  (declare (type (array single-float) a b out)
           (optimize speed))
  (let ((b (or out (copy-array b))))
    (do-arrays ((b-elt b single-float)
                (a-elt a single-float))
      (declare (type single-float a-elt b-elt))
      (setf b-elt (+ a-elt b-elt)))
    b))

(defun cl-sin (a &optional out)
  (declare (type (array double-float) a out)
           (optimize speed))
  (let* ((b (or out (zeros-like a)))
         (b-sv (array-displaced-to b))
         (a-sv (array-displaced-to a))
         (lim  (array-total-size a)))
    (declare (type (cl:simple-array double-float) a-sv b-sv)
             (optimize speed))
    (loop :for i :below lim
          :do (setf (cl:aref b-sv i)
                    (sin (cl:aref a-sv i))))
    b))
