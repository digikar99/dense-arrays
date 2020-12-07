(in-package :dense-arrays)

(in-suite :dense-arrays)

(defun array= (array1 array2 &key (test #'equalp))
  (and (equalp (narray-dimensions array1)
               (narray-dimensions array2))
       (let ((array= t))
         (do-arrays ((elt1 array1)
                     (elt2 array2))
           (setf array= (funcall test elt1 elt2))
           (unless array=               
             (return-from array= nil)))
         t)))

(defun aref (array &rest subscripts)
  ;; DONE: Handle nested aref
  ;; DONE: Handle displaced-index-offsets
  ;; TODO: Handle non-integer subscripts
  ;; TODO: Optimize single-index aref
  ;; TODO [hard?]: Introduce parametric types in CL.
  ;;  In the special case where this reduces to CL:AREF, this function is over 20 times slower.
  ;;  However, I do not see a way of optimizing this even using compiler-macros without
  ;;  parametric user-defined types (for array). However, perhaps, due to
  ;;  the function-call-overhead in python, this is about 2 times faster in that special-case.
  ;;  In fact this is 2-2.5 times faster than python-numpy in either case.
  (declare (optimize speed)
           (type array array)
           (dynamic-extent subscripts))
  (with-slots (displaced-to element-type strides offsets dim rank) array
    (declare (type (cl:simple-array * (*)) displaced-to)
             (type int32 rank))
    (cond ((and (= rank (length subscripts))
                (every #'integerp subscripts))
           (cl:aref displaced-to
                    (let ((index 0))
                      (declare (type int32 index))
                      (loop :for stride :of-type int32 :in strides
                            :for subscript :of-type int32 :in subscripts
                            :for offset :of-type int32 :in offsets
                            :do (incf index (+ offset
                                               (* stride
                                                  subscript))))
                      index)))
          ((or (some #'cl:arrayp subscripts)
               (some #'arrayp subscripts))
           (apply #'%aref array subscripts))
          (t
           (apply #'%aref-view array subscripts)))))


(declaim (inline normalize-index))
(defun normalize-index (index dimension)
  (declare (optimize speed)
           (type int32 index dimension))
  (if (< index 0)
      (+ index dimension)
      index))

(define-condition invalid-index (error)
  ((index :accessor index :initarg :index))
  (:report (lambda (condition stream)
             (format stream "Index ~S is invalid" (index condition)))))

(defun %aref-view (array &rest subscripts)
  "Returns a VIEW of the subscripted ARRAY (no copy)"
  (declare (optimize speed)
           (dynamic-extent subscripts)
           (type array array))
  (with-slots (displaced-to strides offsets dim rank element-type) array
    (multiple-value-bind (dimensions strides offsets contiguous-p rank)
        (let ((new-offsets    nil)
              (new-dimensions nil)
              (new-strides    nil)
              (contiguous-p   t)
              (saw-a-t        nil)
              (rank           rank)
              (dim            dim)
              (strides        strides)
              (offsets        offsets)
              (offset-carry   0))
          (declare (type int32 rank offset-carry))
          ;; TODO: Use offset-carry
          (loop :repeat rank
                :do
                   (let ((s  (car strides))
                         (ss (car subscripts))
                         (d  (car dim))
                         (o  (car offsets)))
                     (declare (type int32 s o)
                              (type (or t int32) ss))
                     (cond ((typep ss 'int32)
                            (setq ss (normalize-index ss d))
                            ;; (incf offset (* s (the int32 ss)))
                            (decf rank)
                            ;; (push (+ o (* ss s)) new-offsets)
                            (let ((offset (+ o (the int32 (* s (the int32 ss))))))
                              (if (first new-offsets)
                                  (incf (the int32 (first new-offsets))
                                        offset)
                                  (incf offset-carry offset))))
                           ;; ((eq t ss)
                           ;;  (setq new-dimensions (cons (car dim) new-dimensions))
                           ;;  (setq new-strides (cons s new-strides))
                           ;;  (setq saw-a-t t))
                           ((listp ss)
                            (destructuring-bind (&optional (start o) &key (stop d) (step 1)) ss
                              (declare (type int32 start stop step))
                              (push (ceiling (- stop start) step) new-dimensions)
                              (push (* s step) new-strides)
                              (push (+ o offset-carry (the int32 (* start s))) new-offsets)
                              (setq offset-carry 0)
                              (setq saw-a-t        t)))
                           (t (error 'invalid-index :index ss)))
                     (when (and saw-a-t (typep ss 'int32))
                       (setq contiguous-p nil)))
                   (setq dim        (cdr dim)
                         strides    (cdr strides)
                         subscripts (cdr subscripts)
                         offsets    (cdr offsets)))
          (values (append (nreverse new-dimensions) dim)
                  (append (nreverse new-strides) strides)
                  (nreverse new-offsets)
                  contiguous-p
                  rank))
      (make-dense-array
       :displaced-to displaced-to
       :element-type element-type
       :dim dimensions
       :strides strides
       :offsets offsets
       :contiguous-p contiguous-p
       :total-size (apply #'* dimensions)
       :root-array (or (array-root-array array) array)
       :rank rank))))

(defun %aref (array &rest subscripts)
  "Returns a copy of the subscripted array."
  (declare (optimize speed)
           (dynamic-extent subscripts)
           (type array array)))

(defun (setf aref) (new-element/s array &rest subscripts)
  (declare (type array array)
           (dynamic-extent subscripts))
  (with-slots (displaced-to element-type strides offsets dim rank) array
    (declare (type (cl:simple-array * (*)) displaced-to))
    (if (and (= rank (length subscripts))
             (every #'integerp subscripts))
        (setf (cl:aref displaced-to
                       (let ((index 0))
                         (declare (type int32 index))
                         (loop :for stride :of-type int32 :in strides
                               :for subscript :of-type int32 :in subscripts
                               :for offset :of-type int32 :in offsets
                               :do (incf index (* stride (+ offset subscript))))
                         index))
              new-element/s)
        (apply #'(setf %aref-view) new-element/s array subscripts))))

(defun (setf %aref-view) (new-array array &rest subscripts)
  ;; TODO: Optimize
  (declare (dynamic-extent subscripts))
  (let ((sub-array (apply #'aref array subscripts)))
    (do-arrays ((new (broadcast-array new-array (array-dimensions sub-array)))
                (old sub-array))
      (setf old new)))
  array)

(def-test aref ()
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+)))
    (is (= 9 (aref array 9 0)))
    (is (= 9 (aref array 8 1)))
    (is (array= array (aref array)))
    (is (array= array (aref array nil)))
    (is (array= array (aref array nil nil)))    
    (is (array= (aref array 0)
                (make-array 2 :constructor #'+)))
    (is (array= (aref array 0 nil)
                (make-array 2 :constructor #'+)))
    (is (array= (aref array nil 0)
                (make-array 10 :constructor #'+))))
  (symbol-macrolet ((array (make-array '(2 3) :constructor #'+)))
    (is (equalp '(2 2) (array-dimensions (aref array nil '(0 :step 2)))))
    (is (equalp '(1 3) (array-dimensions (aref array '(0 :step 2)))))
    (is (= 0 (aref (aref array '(0 :step 2)) 0 0))))
  (is (array= (aref (make-array '(5 5) :constructor #'+) '(1) '(1))
              (make-array '(4 4) :constructor (lambda (x y) (+ 2 x y))))))

(def-test setf-aref ()
  (is (array= (make-array '(2 3 4) :initial-element 1)
              (setf (aref (make-array '(2 3 4) :initial-element 0))
                    1)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)))
              (setf (aref (make-array '(2 3) :initial-element 0) nil 0)
                    (make-array 2 :initial-contents '(2 3))))))

(defun row-major-aref (array index)
  "Return the element of ARRAY corresponding to the row-major INDEX.
This is SETFable"
  (declare ;; (optimize speed)
           (type array array)
           (type int32 index))
  (if (array-contiguous-p array)
      (cl:aref (array-displaced-to array)
               (the fixnum (+ index (the int32 (array-displaced-index-offset array)))))
      (let ((row-major-index   0)
            (apparant-strides  (rest (collect-reduce-from-end #'* (narray-dimensions array) 1))))
        (declare (type int32 row-major-index))
        (loop :for s  :of-type int32 :in (array-strides array)
              :for as :of-type int32 :in apparant-strides
              :for o  :of-type int32 :in (array-offsets array)
              :do (incf row-major-index (+ o (* s index)))
                  (setf index (rem index as)))
        (cl:aref (array-displaced-to array)
                 row-major-index))))

(defun (setf row-major-aref) (new-element array index)
  (declare ;; (optimize speed)
           (type array array)
           (type int32 index))
  (if (array-contiguous-p array)
      (setf (cl:aref (array-displaced-to array)
                     (the fixnum (+ index (the int32 (array-displaced-index-offset array)))))
            new-element)
      (let ((row-major-index   0)
            (apparant-strides  (rest (collect-reduce-from-end #'* (narray-dimensions array) 1))))
        (declare (type int32 row-major-index))
        (loop :for s  :of-type int32 :in (array-strides array)
              :for as :of-type int32 :in apparant-strides              
              :for o  :of-type int32 :in (array-offsets array)
              :do (incf row-major-index (+ o (* s index)))
                  (setf index (rem index as)))
        (setf (cl:aref (array-displaced-to array)
                       row-major-index)
              new-element))))

(def-test row-major-aref ()
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+)))
    (is (= 5 (row-major-aref array 9)))
    (is (= 9 (row-major-aref array 18)))
    (is (= 10 (row-major-aref (aref array nil 1) 9)))))

(def-test setf-row-major-aref ()
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)))
              (let ((array (make-array '(2 3) :constructor #'+)))
                (setf (row-major-aref array 4) 0)
                array)))
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)))
              (let ((array (make-array '(2 3) :constructor #'+)))
                (setf (row-major-aref (aref array nil 1) 1) 0)
                array))))
