(in-package :dense-arrays)

(defun magicl-tensor-type (rank element-type)
  (let ((abstract-tensor-type (case rank
                                (1 "VECTOR")
                                (2 "MATRIX")
                                (t "TENSOR")))
        (tensor-element-type (alexandria:switch (element-type :test #'subtypep)
                               ('single-float "SINGLE-FLOAT")
                               ('double-float "DOUBLE-FLOAT")
                               ('(signed-byte 32) "INT32")
                               ('integer "SINGLE-FLOAT")
                               ('(complex single-float) "COMPLEX-SINGLE-FLOAT")
                               ('(complex double-float) "COMPLEX-DOUBLE-FLOAT"))))
    (find-symbol (concatenate 'string abstract-tensor-type "/" tensor-element-type)
                 :magicl)))


;; TODO: Provide options for changing LAYOUT

(defun as-magicl-tensor (standard-dense-array &key (copy t))
  "Converts STANDARD-DENSE-ARRAY to an appropriate subtype of MAGICL:ABSTRACT-TENSOR

If COPY is non-NIL then avoids making a copy of the storage of STANDARD-DENSE-ARRAY
only if the STANDARD-DENSE-ARRAY is a non-view. In other words, a copy will be created
if STANDARD-DENSE-ARRAY is a view (see ARRAY-VIEW-P) regardless of the value of COPY."
  (let* ((a standard-dense-array)
         (a (if (or copy (array-view-p a))
                (copy-array a)
                a)))
    (declare (optimize speed)
             (type standard-dense-array standard-dense-array a))
    (magicl:make-tensor (magicl-tensor-type
                         (array-rank a)
                         (array-element-type a))
                        (array-dimensions a)
                        :storage (array-storage a)
                        :layout (dense-array-layout a))))

(defun from-magicl-tensor (magicl-tensor &key (copy t))
  "Converts MAGICL:ABSTRACT-TENSOR to a STANDARD-DENSE-ARRAY

If COPY is non-NIL, this copies over the underlying MAGICL::STORAGE"
  (let* ((rank         (magicl:order magicl-tensor))
         (storage      (slot-value magicl-tensor 'magicl::storage))
         (storage      (if copy (alexandria:copy-array storage) storage))
         (element-type (magicl:element-type magicl-tensor))
         (layout       (magicl:layout magicl-tensor))
         (dimensions   (magicl:shape magicl-tensor))
         (strides      (dimensions->strides dimensions))
         (strides      (ecase layout
                         (:row-major layout)
                         (:column-major (nreverse strides))))
         (offsets      (make-list rank :initial-element 0))
         (total-size   (magicl:size magicl-tensor)))
    (make-instance 'standard-dense-array
                   :layout layout
                   :element-type element-type
                   :rank rank
                   :dimensions dimensions
                   :strides strides
                   :offsets offsets
                   :total-size total-size
                   :storage storage
                   :contiguous-p t
                   :root-array nil)))

(defun magicl-funcall (function &rest arguments)
  "Converts STANDARD-DENSE-ARRAY in ARGUMENTS to appropriate MAGICL TENSOR / MATRIX / VECTOR.
Converts the results back to STANDARD-DENSE-ARRAY"
  (declare (optimize speed))
  (multiple-value-call
      (lambda (&rest values)
        (loop :for rem-values :on values
              :for (value . rest) := rem-values
              :if (typep value 'magicl:abstract-tensor)
                :do (setf (first rem-values)
                          (from-magicl-tensor value :copy nil))
              :finally (return (values-list values))))
    (apply function
           (loop :for rem-arguments :on arguments
                 :for (arg . rest) := rem-arguments
                 :if (typep arg 'standard-dense-array)
                   :do (setf (first rem-arguments)
                             (as-magicl-tensor arg :copy nil))
                 :finally (return arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(magic-funcall as-magicl-tensor from-magicl-tensor) :dense-arrays))
