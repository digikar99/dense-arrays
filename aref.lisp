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
  (declare ;; (optimize speed)
           (dynamic-extent subscripts)
           (type array array))
  ;; TODO: Optimize this
  ;; TODO: Combining basic and advanced indexing - raise an issue if this is needed
  ;; Reference: https://numpy.org/doc/stable/reference/arrays.indexing.html#advanced-indexing
  (setq subscripts
        (apply #'broadcast-arrays
               (iter (for subscript in subscripts)
                 ;; TYPE-EXPAND is required for CCL and ECL
                 (if (typep subscript (type-expand '(array bit)))
                     (appending (nonzero subscript))
                     (collect   subscript)))))
  (cond
    ((= (length subscripts) (array-rank array))
     (let ((subscript (first subscripts)))
       (declare (type array subscript))
       (let ((result (make-array (array-dimensions subscript)
                                 :element-type (array-element-type array)))
             (rank   (array-rank subscript))
             (dims   (narray-dimensions subscript)))
         (declare (type int32 rank))
         (labels ((ss-iter (depth ss-idx)
                    (declare (type int32 depth))
                    (if (= depth rank)
                        (let ((ss-elt (mapcar (lambda (array)
                                                (apply #'aref array ss-idx))
                                              subscripts)))
                          (setf (apply #'aref result ss-idx)
                                (apply #'aref array ss-elt)))
                        (loop :for i :from 0 :below (the int32 (nth depth dims))
                              :do (setf (nth depth ss-idx) i)
                                  (ss-iter (1+ depth) ss-idx)
                              :finally (setf (nth depth ss-idx) 0)))
                    nil))
           (ss-iter 0 (make-list rank :initial-element 0))
           result))))
    (t
     (error "Only implemented (= (length subscripts) (array-rank array)) case"))))

(defun (setf aref) (new-element/s array &rest subscripts)
  (declare (type array array)
           (dynamic-extent subscripts))
  (with-slots (displaced-to element-type strides offsets dim rank) array
    (declare (type (cl:simple-array * (*)) displaced-to))
    (cond ((and (= rank (length subscripts))
                (every #'integerp subscripts))
           (setf (cl:aref displaced-to
                          (let ((index 0))
                            (declare (type int32 index))
                            (loop :for stride :of-type int32 :in strides
                                  :for subscript :of-type int32 :in subscripts
                                  :for offset :of-type int32 :in offsets
                                  :do (incf index (+ offset
                                                     (* stride
                                                        subscript))))
                            index))
                 new-element/s))
          ((or (some #'cl:arrayp subscripts)
               (some #'arrayp subscripts))
           (apply #'(setf %aref) new-element/s array subscripts))
          (t
           (apply #'(setf %aref-view) new-element/s array subscripts))))
  new-element/s)

(defun (setf %aref-view) (new-array array &rest subscripts)
  ;; TODO: Optimize
  (declare (dynamic-extent subscripts))
  (let ((sub-array (apply #'aref array subscripts)))
    (do-arrays ((new (broadcast-array new-array (array-dimensions sub-array)))
                (old sub-array))
      (setf old new)))
  new-array)

(defun (setf %aref) (new-array array &rest subscripts)
  (declare ;; (optimize speed)
           (dynamic-extent subscripts)
           (type array array))
  ;; TODO: Optimize this
  (destructuring-bind (new-array &rest subscripts)
      (apply #'broadcast-arrays
             new-array
             (iter (for subscript in subscripts)
               (if (typep subscript (type-expand '(array bit)))
                   (appending (nonzero subscript))
                   (collect   subscript))))
    (cond
      ((= (length subscripts) (array-rank array))
       (let ((subscript (first subscripts)))
         (declare (type array subscript))
         (let ((rank   (array-rank        new-array))
               (dims   (narray-dimensions new-array)))
           (declare (type int32 rank))
           (labels ((ss-iter (depth ss-idx)
                      (declare (type int32 depth))
                      (if (= depth rank)
                          (let ((ss-elt (mapcar (lambda (array)
                                                  (apply #'aref array ss-idx))
                                                subscripts)))
                            (setf (apply #'aref array ss-elt)
                                  (apply #'aref new-array ss-idx)))
                          (loop :for i :from 0 :below (the int32 (nth depth dims))
                                :do (setf (nth depth ss-idx) i)
                                    (ss-iter (1+ depth) ss-idx)
                                :finally (setf (nth depth ss-idx) 0)))
                      nil))
             (ss-iter 0 (make-list rank :initial-element 0))))))
      (t
       (error "Only implemented (= (length subscripts) (array-rank array)) case"))))
  new-array)

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

(def-test advanced-aref ()
  (is (array= (make-array '(2 2) :initial-contents '((2 6) (1 4)))
              (aref (make-array '(2 3)
                                :initial-contents '((1 2 3) (4 5 6)))
                    (make-array '(2 2)
                                :initial-contents '((0 1) (0 1)))
                    (make-array '(2 2)
                                :initial-contents '((1 2) (0 0))))))
  (is (array= (make-array '(3) :initial-contents '(1 3 6))
              (aref (make-array '(2 3)
                                :initial-contents '((1 2 3) (4 5 6)))
                    (make-array '(2 3)
                                :element-type 'bit
                                :initial-contents '((1 0 1) (0 0 1)))))))

(def-test setf-aref ()
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 2 -1)))
              (let ((a (make-array '(2 3) :constructor #'+)))
                (setf (aref a 1 2) -1)
                a)))
  (is (array= (make-array '(2 3 4) :initial-element 1)
              (let ((a (make-array '(2 3 4) :initial-element 0)))
                (setf (aref a) 1)
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)))
              (let ((a (make-array '(2 3) :initial-element 0)))
                (setf (aref a nil 0)
                      (make-array 2 :initial-contents '(2 3)))
                a))))

(def-test setf-advanced-aref ()
  (is (array= (make-array '(2 3) :initial-contents '((2 2 2) (4 5 2)))
              (let ((a (make-array '(2 3)
                                   :initial-contents '((1 2 3) (4 5 6)))))
                (setf (aref a (make-array '(2 3)
                                          :element-type 'bit
                                          :initial-contents '((1 0 1) (0 0 1))))
                      2)
                a))))

(defun row-major-aref (array index)
  "Return the element of ARRAY corresponding to the row-major INDEX.
This is SETFable"
  (declare ;; (optimize speed)
   (type array array)
   (type int32 index))
  (if (array-contiguous-p array)
      (cl:aref (array-displaced-to array)
               (the int32 (apply #'+ index (array-offsets array))))
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
                     (the int32 (apply #'+ index (array-offsets array))))
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
