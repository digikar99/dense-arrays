(in-package :dense-arrays-plus)

(defmethod reader:get-val ((array dense-arrays::dense-array) &rest subscripts)
  (apply #'aref array subscripts))

(defmethod (setf reader:get-val) (new-value (array dense-arrays::dense-array) &rest subscripts)
  (apply #'(setf aref) new-value array subscripts))
