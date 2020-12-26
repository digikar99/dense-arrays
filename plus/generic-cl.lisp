(in-package :dense-arrays-plus)

(defmethod generic-cl:coerce ((array cl:array) (result (eql 'dense-arrays:array)))
  (asarray array))

;; TODO
(macrolet ((stub (wrapper-fn base-fn)
             `(defmethod ,wrapper-fn ((a dense-arrays::dense-array)
                                      (b dense-arrays::dense-array))
                (destructuring-bind (a b)
                    (dense-arrays::broadcast-arrays a b)
                  (let ((c (zeros-like a)))
                    (do-arrays ((a-elt a)
                                (b-elt b)
                                (c-elt c))
                      (setf c-elt (,base-fn a-elt b-elt)))
                    c)))))
  (stub generic-cl:add      cl:+)
  (stub generic-cl:subtract cl:-)
  (stub generic-cl:divide   cl:/)
  (stub generic-cl:multiply cl:*))



