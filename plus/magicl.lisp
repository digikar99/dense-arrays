(in-package :dense-arrays)

(in-suite :dense-arrays)

(defpolymorph aref ((array magicl:abstract-tensor) &rest subscripts) t
  (apply #'magicl:tref array subscripts))

(defpolymorph array-element-type ((magicl-tensor (or magicl::vector magicl::matrix magicl:tensor))) t
  (magicl:element-type magicl-tensor))

(defpolymorph array-dimensions ((magicl-tensor (or magicl::vector magicl::matrix magicl:tensor))) t
  (magicl:shape magicl-tensor))

(defpolymorph array-total-size ((magicl-tensor (or magicl::vector magicl::matrix magicl:tensor)))
    fixnum
  (magicl:size magicl-tensor))

(defpolymorph array-rank ((magicl-tensor (or magicl::vector magicl::matrix magicl:tensor)))
    fixnum
  (magicl:order magicl-tensor))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass magicl-dense-array-class (dense-array-class) ())
  (defclass magicl-dense-array (dense-array)
    ()
    (:metaclass magicl-dense-array-class)))

(defun upgraded-magicl-array-element-type (element-type)
  (cond ((member element-type
                 '(single-float
                   double-float
                   (complex single-float)
                   (complex double-float))
                 :test #'type=)
         element-type)
        ((subtypep element-type '(signed-byte 32))
         '(signed-byte 32))
        (t
         magicl::*default-tensor-type*)))

(defmethod storage-element-type-upgrader ((class magicl-dense-array-class))
  'upgraded-magicl-array-element-type)

(defun make-magicl-vector (size &key element-type initial-element)
  ;; TODO: This should take shape
  (magicl:const (coerce initial-element element-type) (list size)
                :type element-type
                :layout :row-major))

(defmethod storage-allocator ((class magicl-dense-array-class))
  'make-magicl-vector)

(defmethod storage-deallocator ((class magicl-dense-array-class))
  nil)

(defmethod storage-accessor ((class magicl-dense-array-class))
  'magicl:tref)

(defmethod storage-type-inferrer-from-array-type ((class magicl-dense-array-class))
  (lambda (array-type)
    (if (eq 'cl:* (array-type-element-type array-type))
        'magicl::vector
        (switch ((array-type-element-type array-type) :test #'type=)
          ('single-float 'magicl:vector/single-float)
          ('double-float 'magicl:vector/double-float)
          ('(complex single-float) 'magicl:vector/complex-single-float)
          ('(complex double-float) 'magicl:vector/complex-double-float)
          (t 'magicl::vector)))))

(define-array-specialization-type magicl-array magicl-dense-array)
(define-array-specialization-type simple-magicl-array magicl-dense-array)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(magicl-funcall magicl-dense-array magicl-array simple-magicl-array)
          (find-package :dense-arrays)))
