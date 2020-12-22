(in-package :dense-arrays)

;; TODO: Merge the two functions below

(defun argwhere (bit-array)
  (declare (optimize speed)
           ;; TODO: Optimize
           (type (array bit) bit-array))
  (let ((elt-count 0)
        (rank      (array-rank bit-array))
        (dims      (narray-dimensions bit-array)))
    (declare (type (unsigned-byte 62) elt-count)
             (type int32 rank))
    (do-arrays ((a bit-array bit))
      (unless (zerop (the bit a))
        (setq elt-count (+ 1 elt-count))))
    (let ((index-array (make-array (list elt-count rank)
                                   :element-type 'uint64
                                   :initial-element 0))
          (elt-idx      0))
      (declare (type (unsigned-byte 62) elt-idx))
      (labels ((index-iter (depth indices)
                 (declare (type int32 depth))
                 (if (= depth rank)
                     (unless (zerop (the bit (apply #'aref bit-array indices)))
                       (loop :for i :of-type int32 :from 0
                             :for idx :in indices
                             :do (setf (aref index-array elt-idx i) idx))
                       (incf elt-idx))
                     (loop :for i :from 0 :below (the int32 (nth depth dims))
                           :do (setf (nth depth indices) i)
                               (index-iter (1+ depth) indices)
                               (setf (nth depth indices) 0)))
                 nil))
        (index-iter 0 (make-list rank :initial-element 0))
        index-array))))

(defun nonzero (bit-array)
  (declare (optimize speed)
           ;; TODO: Optimize
           (type (array bit) bit-array))
  (let ((elt-count 0)
        (rank      (array-rank bit-array))
        (dims      (narray-dimensions bit-array)))
    (declare (type (unsigned-byte 62) elt-count)
             (type int32 rank))
    (do-arrays ((a bit-array bit))
      (unless (zerop (the bit a))
        (setq elt-count (+ 1 elt-count))))
    (let ((index-arrays (loop :repeat rank
                              :collect (make-array elt-count
                                                   :element-type 'uint64
                                                   :initial-element 0)))
          (elt-idx      0))
      (declare (type (unsigned-byte 62) elt-idx))
      (labels ((index-iter (depth indices)
                 (declare (type int32 depth))
                 (if (= depth rank)
                     (unless (zerop (the bit (apply #'aref bit-array indices)))
                       (loop :for idx-array :in index-arrays
                             :for idx :in indices
                             :do (setf (aref idx-array elt-idx) idx))
                       (incf elt-idx))
                     (loop :for i :from 0 :below (the int32 (nth depth dims))
                           :do (setf (nth depth indices) i)
                               (index-iter (1+ depth) indices)
                               (setf (nth depth indices) 0)))
                 nil))
        (index-iter 0 (make-list rank :initial-element 0))
        index-arrays))))
