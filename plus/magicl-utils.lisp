(in-package :dense-arrays)

(defun copy-magicl-array (array)
  "Returns a copy of ARRAY. Creates a completely new array even if ARRAY
is a VIEW (see ARRAY-VIEW-P)."
  (declare (type magicl-array array))
  (let ((new-array (make-array (array-dimensions array)
                               :element-type (array-element-type array)
                               :initial-element (row-major-aref array 0)
                               :class 'magicl-dense-array)))
    (do-arrays ((new new-array cl:* :class magicl-dense-array)
                (old array cl:* :class magicl-dense-array))
      (setf new old))
    new-array))

(declaim (notinline magicl-funcall))
(defun magicl-funcall (function &rest arguments)
  "Converts MAGICL-DENSE-ARRAY in ARGUMENTS to appropriate MAGICL TENSOR / MATRIX / VECTOR.
Converts the results back to MAGICL-DENSE-ARRAY"
  (declare (type function-designator function))
  (loop :for args :on arguments
        :for arg := (first args)
        :if (typep arg 'magicl-dense-array)
          :do (locally (declare (type magicl-dense-array arg))
                (when (array-view-p arg)
                  (setq arg (copy-magicl-array arg)))
                (setf (first args)
                      (magicl:reshape (array-storage arg)
                                      (array-dimensions arg)))))
  (let ((results (multiple-value-list (apply function arguments))))
    (loop :for res :on results
          :for result := (first res)
          :if (typep result '(or magicl::vector magicl::matrix magicl:tensor))
            :do (locally
                    (declare (type (or magicl::vector magicl::matrix magicl:tensor) result)
                             (optimize speed))
                  (let ((rank (array-rank result)))
                    ;; FIXME: Think and implement LAYOUT
                    ;; (unless (eq :row-major (layout result))
                    ;;   (let ((row-major (magicl:zeros (magicl:shape result)
                    ;;                                  :type (magicl:element-type result)
                    ;;                                  :layout :row-major)))
                    ;;     (magicl:map)))
                    (setf (first res)
                          (make-instance 'magicl-dense-array
                                         :storage (magicl:reshape result
                                                                  (list (array-total-size result)))
                                         :element-type (array-element-type result)
                                         :dimensions (array-dimensions result)
                                         :strides (make-list rank :initial-element 0)

                                         :offsets (make-list rank :initial-element 0)
                                         :contiguous-p t
                                         :total-size   (array-total-size result)
                                         :rank rank
                                         :root-array nil)))))
    (values-list results)))
