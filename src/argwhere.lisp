(in-package :dense-arrays)

;; TODO: Merge the two functions below

(defun argwhere (bit-array)
  (declare (optimize speed)
           ;; TODO: Optimize
           (type (array bit) bit-array)
           (compiler-macro-notes:muffle compiler-macro-notes:note))
  (let ((elt-count 0)
        (rank      (array-rank bit-array))
        (dims      (narray-dimensions bit-array)))
    (declare (type size rank elt-count))
    (do-arrays ((a bit-array bit))
      (unless (zerop (the bit a))
        (setq elt-count (+ 1 elt-count))))
    (let ((index-array (make-array (list elt-count rank)
                                   :element-type 'uint64
                                   :initial-element 0))
          (elt-idx      0))
      (declare (type size elt-idx)
               (type dense-array index-array))
      (labels ((index-iter (depth indices)
                 (declare (type size depth))
                 (if (= depth rank)
                     (unless (zerop (the bit (apply #'aref bit-array indices)))
                       (loop :for i :of-type size :from 0
                             :for idx :in indices
                             :do (funcall #'(setf aref)
                                          idx
                                          index-array elt-idx i))
                       (incf elt-idx))
                     (loop :for i :from 0 :below (the size (nth depth dims))
                           :do (setf (nth depth indices) i)
                               (index-iter (1+ depth) indices)
                               (setf (nth depth indices) 0)))
                 nil))
        (index-iter 0 (make-list rank :initial-element 0))
        index-array))))

(defun nonzero (bit-array)
  (declare (optimize speed)
           ;; TODO: Optimize
           (type (%dense-array bit) bit-array)
           (compiler-macro-notes:muffle compiler-macro-notes:note))
  (let ((elt-count 0)
        (rank      (array-rank bit-array))
        (dims      (narray-dimensions bit-array)))
    (declare (type size rank elt-count))
    (do-arrays ((a bit-array bit))
      (unless (zerop (the bit a))
        (setq elt-count (+ 1 elt-count))))
    (let ((index-arrays (loop :repeat rank
                              :collect (make-array elt-count
                                                   :element-type 'uint64
                                                   :initial-element 0)))
          (elt-idx      0))
      (declare (type size elt-idx))
      (labels ((index-iter (depth indices)
                 (declare (type size depth))
                 (if (= depth rank)
                     (unless (zerop (the bit (apply #'aref bit-array indices)))
                       (loop :for idx-array :of-type dense-array :in index-arrays
                             :for idx :in indices
                             :do (funcall #'(setf aref)
                                          idx
                                          (the dense-array idx-array)
                                          elt-idx))
                       (incf elt-idx))
                     (loop :for i :from 0 :below (the size (nth depth dims))
                           :do (setf (nth depth indices) i)
                               (index-iter (1+ depth) indices)
                               (setf (nth depth indices) 0)))
                 nil))
        (index-iter 0 (make-list rank :initial-element 0))
        index-arrays))))
