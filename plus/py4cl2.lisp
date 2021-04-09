(cl:in-package :dense-arrays-plus)

(5am:in-suite :dense-arrays-plus)

(unless (getf py4cl2:*arrayfiers* :dense-arrays)
  (setq py4cl2:*arrayfiers*
        (append '(:dense-arrays asarray)
                py4cl2:*arrayfiers*)))

(def-test py4cl2 ()
  (is (array= (make-array '(2 3)
                          :initial-contents '((1 2 3) (4 5 6)))
              (let ((py4cl2:*array-type* :dense-arrays))
                (py4cl2:pyeval #2A((1 2 3) (4 5 6))))))
  (is (array= (make-array '(3 2)
                          :element-type 'int32
                          :initial-contents '((1 4) (2 5) (3 6)))
              (let ((py4cl2:*array-type* :dense-arrays))
                (py4cl2:pyeval (transpose (asarray #2A((1 2 3) (4 5 6)) 'int32))))))
  (is (array= (make-array (list 2 (py4cl2:config-var 'py4cl2:numpy-pickle-lower-bound))
                          :element-type 'int32)
              (let ((py4cl2:*array-type* :dense-arrays))
                (py4cl2:pyeval
                 (make-array (list 2 (py4cl2:config-var 'py4cl2:numpy-pickle-lower-bound))
                             :element-type 'int32))))))

(defmethod py4cl2:pythonize ((object dense-arrays::dense-array))
  (py4cl2:pythonize (as-cl-array object)))

