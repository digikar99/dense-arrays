(in-package :dense-arrays)

(defun copy-array (array)
  "Returns a copy of ARRAY. Creates a completely new array even if ARRAY
is a VIEW (see ARRAY-VIEW-P)."
  (declare (type array array))
  (let ((new-array (make-array (array-dimensions array)
                               :element-type (array-element-type array)
                               :initial-element (row-major-aref array 0)
                               :layout (dense-array-layout array))))
    (do-arrays ((new new-array)
                (old array))
      (setf new old))
    new-array))
