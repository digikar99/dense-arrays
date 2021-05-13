(in-package :dense-arrays)

(defun broadcast-array (array broadcast-dimensions)
  (declare (optimize debug))
  (unless (arrayp array)
    ;; Should probably warn if ARRAY is not an array
    (setq array (make-array 1 :initial-element array :element-type (type-of array))))
  (when (equalp broadcast-dimensions (narray-dimensions array))
    (return-from broadcast-array array))
  (with-slots (dimensions element-type strides offsets displaced-to) array
    (multiple-value-bind (strides offsets)
        (let* ((blen (length broadcast-dimensions))
               (len  (length dimensions))
               (dim  (append (make-list (- blen len)
                                        :initial-element 1)
                             dimensions))
               (strides (append (make-list (- blen len)
                                           :initial-element 0)
                                strides))
               (new-offsets nil)
               (offsets offsets))
          (values
           (loop :for s :in strides
                 :for b :in broadcast-dimensions
                 :for d :in dim
                 :for o := (or (first offsets) 0)
                 :collect
                 (cond ((= b d)
                        (push o new-offsets)
                        (setq offsets (rest offsets))
                        s)
                       ((= d 1)
                        (push 0 new-offsets)
                        0)
                       (t (error "~D of dim ~D cannot be broadcasted to dim ~D"
                                 array dim broadcast-dimensions))))
           (nreverse new-offsets)))
      (let ((total-size (apply #'* broadcast-dimensions)))
        (make-dense-array
         :dimensions broadcast-dimensions
         :element-type element-type
         :strides strides
         :offsets offsets
         :displaced-to displaced-to
         ;; TODO: Raises the question of semantics of array being contiguous
         :contiguous-p (= (first strides)
                          (/ total-size (first broadcast-dimensions)))
         :total-size total-size
         :root-array (or (dense-array-root-array array) array)
         :rank (length broadcast-dimensions)
         :storage (array-storage array)
         :backend (dense-array-backend array))))))

(defun %broadcast-compatible-p (dimensions-a dimensions-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two dimensions are broadcast compatible.
  The second value is the dimensions of the array resulting from the broadcast."
  (if (equalp dimensions-a dimensions-b)
      (values t dimensions-a)
      (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1))
        ;; We do not use a "for in" clause because we want to terminate at the
        ;; maximum of the two lists rather than the minimum
        (for dim-a initially (reverse dimensions-a)
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (reverse dimensions-b)
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b) (= a 1) (= b 1))
                     (max a b)
                     (return nil))
          into broadcast-dimensions-reversed)
        (finally (return (values t
                                 (nreverse broadcast-dimensions-reversed)))))))

(defun broadcast-compatible-p (&rest arrays)
  "Returns two values:
  The first value is a generalized boolean indicating whether the arrays can be broadcasted.
  The second value is the dimension of the array resulting from the broadcast."
  (declare (dynamic-extent arrays)
           (optimize speed))
  (case (length arrays)
    (0 t)
    (1 (values t (narray-dimensions (first arrays))))
    (2 (%broadcast-compatible-p (narray-dimensions (first arrays))
                                (narray-dimensions (second arrays))))
    (t (multiple-value-bind (compatible-p broadcast-dimensions)
           (%broadcast-compatible-p
            (narray-dimensions (first arrays))
            (narray-dimensions (second arrays)))
         ;; Can this be simplified?
         (when compatible-p
           (multiple-value-bind (compatible-p-rest broadcast-dimensions-rest)
               (apply 'broadcast-compatible-p (cddr arrays))
             (when compatible-p-rest
               (%broadcast-compatible-p broadcast-dimensions
                                        broadcast-dimensions-rest))))))))

(defun broadcast-arrays (&rest arrays)
  "Returns two values. The first value is the list of broadcasted arrays
  if the second value is non-NIL."
  (declare (dynamic-extent arrays))
  (setq arrays (loop :for array :in arrays
                     :collect (if (arrayp array)
                                  array
                                  (make-array 1
                                              :element-type (type-of array)
                                              :initial-element array))))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (apply #'broadcast-compatible-p arrays)
    (if broadcast-compatible-p
        (values (loop :for array :in arrays
                      :if (equalp broadcast-dimensions (narray-dimensions array))
                        :collect array
                      :else
                        :collect (broadcast-array array broadcast-dimensions))
                t)
        (values nil nil))))

