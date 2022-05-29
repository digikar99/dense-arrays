
(in-package :dense-arrays)

(defun array= (array1 array2 &key (test #'equalp))
  (and (equalp (narray-dimensions array1)
               (narray-dimensions array2))
       (loop :for i :below (array-total-size array1)
             :always (funcall test
                              (row-major-aref array1 i)
                              (row-major-aref array2 i)))))

(declaim (inline normalize-index))
(defun normalize-index (index dimension)
  (declare (optimize speed)
           (type int-index index dimension))
  (the size (if (< index 0)
                (+ index dimension)
                index)))

(defun %aref-view (array &rest subscripts)
  "Returns a VIEW of the subscripted ARRAY (no copy)"
  (declare (optimize speed)
           (dynamic-extent subscripts)
           (type dense-array array))
  (with-slots (storage strides offsets dimensions rank element-type layout) array
    (multiple-value-bind (class dimensions strides offsets rank)
        (let ((new-offsets    nil)
              (new-dimensions nil)
              (new-strides    nil)
              (rank           rank)
              (dim            dimensions)
              (strides        strides)
              (offsets        offsets)
              (offset-carry   0))
          (declare (type size rank offset-carry))
          (loop :repeat rank
                :do
                   (let ((s  (car strides))
                         (ss (car subscripts))
                         (d  (car dim))
                         (o  (car offsets)))
                     (declare (type size o)
                              (type int-index s))
                     (cond ((typep ss 'int-index)
                            (setq ss (normalize-index ss d))
                            (decf rank)
                            (let ((offset (the-size (+ o (the-int-index (* s ss))))))
                              (declare (type size offset))
                              (if (first new-offsets)
                                  (incf (the-size (first new-offsets)) offset)
                                  (incf offset-carry        offset))))
                           ((listp ss)
                            (destructuring-bind (&optional (start 0)
                                                 &key (end d endp) (step 1))
                                ss
                              (declare (type int-index start end step))
                              (psetq start (normalize-index start d)
                                     end   (normalize-index end   d))
                              (when (and (< step 0) (not endp))
                                (setq end -1))
                              (push (ceiling (- end start) step) new-dimensions)
                              (push (the-int-index (* s step)) new-strides)
                              (push (the-size (+ o offset-carry (the-int-index (* start s))))
                                    new-offsets)
                              (setq offset-carry 0)))
                           (t (error 'invalid-index :index ss))))
                   (setq dim        (cdr dim)
                         strides    (cdr strides)
                         subscripts (cdr subscripts)
                         offsets    (cdr offsets)))
          (values (class-of array)
                  (append (nreverse new-dimensions) dim)
                  (append (nreverse new-strides) strides)
                  (nreverse new-offsets)
                  rank))
      (make-instance class
                     :storage storage
                     :element-type element-type
                     :dimensions dimensions
                     :strides strides
                     :offsets offsets
                     :layout nil
                     :total-size (apply #'* dimensions)
                     :root-array (or (dense-array-root-array array) array)
                     :rank rank))))

(defun %aref (array &rest subscripts)
  "Returns a copy of the subscripted array."
  (declare ;; (optimize speed)
           (dynamic-extent subscripts)
           (type dense-array array))
  ;; TODO: Optimize this
  ;; TODO: Combining basic and advanced indexing - raise an issue if this is needed
  ;; Reference: https://numpy.org/doc/stable/reference/arrays.indexing.html#advanced-indexing
  (setq subscripts
        (apply #'broadcast-arrays
               (iter (for subscript in subscripts)
                 ;; TYPE-EXPAND is required for CCL and ECL
                 (if (typep subscript (typexpand '(%dense-array bit)))
                     (appending (nonzero subscript))
                     (collect   subscript)))))
  (cond
    ((= (length subscripts) (array-rank array))
     (let ((subscript (first subscripts)))
       (declare (type dense-array subscript))
       (let ((result (make-array (array-dimensions subscript)
                                 :element-type (array-element-type array)))
             (rank   (array-rank subscript))
             (dims   (narray-dimensions subscript)))
         (declare (type size rank))
         (labels ((ss-iter (depth ss-idx)
                    (declare (type size depth))
                    (if (= depth rank)
                        (let ((ss-elt (mapcar (lambda (array)
                                                (apply #'aref array ss-idx))
                                              subscripts)))
                          (setf (apply #'aref result ss-idx)
                                (apply #'aref array ss-elt)))
                        (loop :for i :from 0 :below (the size (nth depth dims))
                              :do (setf (nth depth ss-idx) i)
                                  (ss-iter (1+ depth) ss-idx)
                              :finally (setf (nth depth ss-idx) 0)))
                    nil))
           (ss-iter 0 (make-list rank :initial-element 0))
           result))))
    (t
     (error "Only implemented (= (length subscripts) (array-rank array)) case"))))

(defpolymorph (aref :inline t) ((array dense-array) &rest subscripts) t
  (declare ;; (optimize speed)
           (type dense-array array)
           (dynamic-extent subscripts))
  (cond ((and (= (array-rank array) (length subscripts))
              (every #'integerp subscripts))
         (let* ((dense-array-class (class-of array)))
           (assert-type
            (funcall (storage-accessor dense-array-class)
                     (array-storage array)
                     (the int-index
                          (let ((index 0))
                            (declare (type size index))
                            ;; TODO: Better error reporting for negative indices
                            (loop :for stride :of-type int-index :in (array-strides array)
                                  :for subscript :of-type int-index :in subscripts
                                  :for offset :of-type size :in (array-offsets array)
                                  :for dimension :of-type size :in (narray-dimensions array)
                                  :do (incf index (+ offset
                                                     (the-size
                                                      (* stride
                                                         (normalize-index subscript dimension))))))
                            index)))
            (array-element-type array))))
        ((or (some #'cl:arrayp subscripts)
             (some #'arrayp subscripts))
         (apply #'%aref array subscripts))
        (t
         (apply #'%aref-view array subscripts))))

(define-condition invalid-index (error)
  ((index :accessor index :initarg :index))
  (:report (lambda (condition stream)
             (format stream "Index ~S is invalid" (index condition)))))

(defun (setf %aref-view) (new-array array &rest subscripts)
  ;; TODO: Optimize
  (declare (dynamic-extent subscripts))
  (let* ((sub-array         (apply #'aref array subscripts))
         (broadcasted-array (broadcast-array new-array (array-dimensions sub-array))))
    (loop for i below (array-total-size sub-array)
          do (setf (row-major-aref sub-array i)
                   (row-major-aref broadcasted-array i))))
  new-array)

(defun (setf %aref) (new-array array &rest subscripts)
  (declare ;; (optimize speed)
           (dynamic-extent subscripts)
           (type dense-array array))
  ;; TODO: Optimize this
  (destructuring-bind (new-array &rest subscripts)
      (apply #'broadcast-arrays
             new-array
             (iter (for subscript in subscripts)
               (if (typep subscript (typexpand '(%dense-array bit)))
                   (appending (nonzero subscript))
                   (collect   subscript))))    
    (cond
      ((= (length subscripts) (array-rank array))
       (let ((rank   (array-rank        new-array))
             (dims   (narray-dimensions new-array))
             (elt-type (array-element-type array)))
         (declare (type size rank))
         (labels ((ss-iter (depth ss-idx)
                    (declare (type size depth))
                    (if (= depth rank)
                        (let ((ss-elt (mapcar (lambda (array)
                                                (apply #'aref array ss-idx))
                                              subscripts)))
                          (setf (apply #'aref array ss-elt)
                                (assert-type (apply #'aref new-array ss-idx)
                                             elt-type)))
                        (loop :for i :from 0 :below (the size (nth depth dims))
                              :do (setf (nth depth ss-idx) i)
                                  (ss-iter (1+ depth) ss-idx)
                              :finally (setf (nth depth ss-idx) 0)))
                    nil))
           (ss-iter 0 (make-list rank :initial-element 0)))))
      (t
       (error "Only implemented (= (length subscripts) (array-rank array)) case"))))
  new-array)

(defpolymorph ((setf aref) :inline t) (new-element/s (array dense-array) &rest subscripts) t
  (declare (type dense-array array)
           ;; (optimize speed)
           (dynamic-extent subscripts))
  (with-slots (storage element-type strides offsets dimensions rank) array
    (cond ((and (= rank (length subscripts))
                (every #'integerp subscripts))
           (assert-type new-element/s (array-element-type array))
           (let* ((dense-array-class (class-of array)))
             (funcall (fdefinition `(setf ,(storage-accessor dense-array-class)))
                      new-element/s
                      storage
                      (let ((index 0))
                        (declare (type size index))
                        ;; TODO: Better error reporting for negative indices
                        (loop :for stride :of-type int-index :in strides
                              :for subscript :of-type int-index :in subscripts
                              :for offset :of-type size :in offsets
                              :for dimension :of-type size :in (narray-dimensions array)
                              :do (incf index
                                      (the-size
                                       (+ offset
                                          (the-int-index
                                           (* stride
                                              (normalize-index subscript dimension)))))))
                        index))))
          ((or (some #'cl:arrayp subscripts)
               (some #'arrayp subscripts))
           (apply #'(setf %aref) new-element/s array subscripts))
          (t
           (apply #'(setf %aref-view) new-element/s array subscripts))))
  new-element/s)

(def-test aref (:suite backend-independent)
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+ :element-type 'int32)))
    (is (= 9 (aref array 9 0)))
    (is (= 9 (aref array 8 1)))
    (is (= 9 (aref array -2 -1)))
    (is (array= array (aref array)))
    (is (array= array (aref array nil)))
    (is (array= array (aref array nil nil)))
    (is (array= (aref array 0)
                (make-array 2 :constructor #'+ :element-type 'int32)))
    (is (array= (aref array 0 nil)
                (make-array 2 :constructor #'+ :element-type 'int32)))
    (is (array= (aref array nil 0)
                (make-array 10 :constructor #'+ :element-type 'int32)))
    (is (array= (make-array 5 :initial-contents '(5 4 3 2 1) :element-type 'int32)
                (aref (make-array 5 :initial-contents '(1 2 3 4 5) :element-type 'int32)
                      '(-1 :step -1))))
    (is (array= (aref (aref array '(2 :end 4)) () 0)
                (make-array '(2) :initial-contents '(2 3)
                                 :element-type 'int32))))
  (symbol-macrolet ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
    (is (equalp '(2 2) (array-dimensions (aref array nil '(0 :step 2)))))
    (is (equalp '(1 3) (array-dimensions (aref array '(0 :step 2)))))
    (is (= 0 (aref (aref array '(0 :step 2)) 0 0))))
  (is (array= (aref (make-array '(5 5) :constructor #'+ :element-type 'int32)
                    '(1) '(1))
              (make-array '(4 4) :constructor (lambda (x y) (+ 2 x y))
                                 :element-type 'int32))))

(def-test advanced-aref (:suite backend-dependent)
  (is (array= (make-array '(2 2) :initial-contents '((2 6) (1 4)) :element-type 'int32)
              (aref (make-array '(2 3)
                                :element-type 'int32
                                :initial-contents '((1 2 3) (4 5 6)))
                    (make-array '(2 2)
                                :element-type 'int32
                                :initial-contents '((0 1) (0 1)))
                    (make-array '(2 2)
                                :element-type 'int32
                                :initial-contents '((1 2) (0 0))))))
  (is (array= (make-array '(3) :initial-contents '(1 3 6) :element-type 'int32)
              (aref (make-array '(2 3)
                                :element-type 'int32
                                :initial-contents '((1 2 3) (4 5 6)))
                    (make-array '(2 3)
                                :element-type 'bit
                                :initial-contents '((1 0 1) (0 0 1)))))))

(def-test setf-aref (:suite backend-independent)
  ;; The array constructed in BROADCAST-ARRAY has an implicit assumption
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 2 -1)) :element-type 'int32)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (aref a 1 2) -1)
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 2 -1)) :element-type 'int32)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (aref a -1 -1) -1)
                a)))
  (is (array= (make-array '(2 3 4) :initial-element 1 :element-type 'int32)
              (let ((a (make-array '(2 3 4) :initial-element 0 :element-type 'int32)))
                (setf (aref a) 1)
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)) :element-type 'int32)
              (let ((a (make-array '(2 3) :initial-element 0 :element-type 'int32)))
                (setf (aref a nil 0)
                      (make-array 2 :initial-contents '(2 3) :element-type 'int32))
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)) :element-type 'int32)
              (let ((a (make-array '(2 3) :initial-element 0 :element-type 'int32)))
                (setf (aref a nil -3)
                      (make-array 2 :initial-contents '(2 3) :element-type 'int32))
                a))))

(def-test setf-advanced-aref (:suite backend-dependent)
  (is (array= (make-array '(2 3) :initial-contents '((2 2 2) (4 5 2)) :element-type 'int32)
              (let ((a (make-array '(2 3)
                                   :initial-contents '((1 2 3) (4 5 6)) :element-type 'int32)))
                (setf (aref a (make-array '(2 3)
                                          :element-type 'bit
                                          :initial-contents '((1 0 1) (0 0 1))
                                          :class 'standard-dense-array))
                      2)
                a))))

(defpolymorph (row-major-aref :inline t) ((array dense-array) index) t
  (declare (type int-index index))
  (let ((row-major-index   0)
        (apparant-strides  (rest (collect-reduce-from-end #'*
                                                          (narray-dimensions array) 1))))
    ;; APPARANT-STRIDES corresponds to the INDEX if DIMENSIONS were the true dimensions.
    ;; A user supplies the INDEX assuming that DIMENSIONS are the true dimensions.
    ;; But this need not be true due to strides and offsets.
    ;; Therefore, we obtain the INDEX from the APPARANT-STRIDES with each dimension
    ;; of calculation.
    (declare (type int-index row-major-index))
    (loop :for s  :of-type int-index :in (array-strides array)
          :for as :of-type int-index :in apparant-strides
          :for o  :of-type int-index :in (array-offsets array)
          :do (incf row-major-index (the-int-index (+ o (the-int-index
                                                         (* s (floor index as))))))
              (setf index (rem index as)))
    (assert-type (funcall (fdefinition (storage-accessor (class-of array)))
                          (array-storage array)
                          row-major-index)
                 (array-element-type array))))

(defpolymorph (row-major-aref :inline t) ((array simple-dense-array) index) t
  ;; TODO: Use contiguous-dense-array instead of simple-dense-array
  (declare (type int-index index))
  (assert-type (funcall (fdefinition (storage-accessor (class-of array)))
                        (array-storage array)
                        index)
               (array-element-type array)))

(defpolymorph ((setf row-major-aref) :inline t) (new-element (array dense-array) index) t
  (declare (type int-index index))
  (assert-type new-element (array-element-type array))
  (let ((row-major-index   0)
        (apparant-strides  (rest (collect-reduce-from-end #'* (narray-dimensions array) 1))))
    (declare (type int-index row-major-index))
    (loop :for s  :of-type int-index :in (array-strides array)
          :for as :of-type int-index :in apparant-strides
          :for o  :of-type int-index :in (array-offsets array)
          :do (incf row-major-index (the-int-index (+ o (the-int-index
                                                         (* s (floor index as))))))
              (setf index (rem index as)))
    (funcall (fdefinition `(setf ,(storage-accessor (class-of array))))
             new-element
             (array-storage array)
             row-major-index)))

(defpolymorph ((setf row-major-aref) :inline t) (new-element (array simple-dense-array) index) t
  ;; TODO: Use contiguous-dense-array instead of simple-dense-array
  (declare (type int-index index))
  (assert-type new-element (array-element-type array))
  (funcall (fdefinition `(setf ,(storage-accessor (class-of array))))
           new-element
           (array-storage array)
           index))

(def-test row-major-aref (:suite backend-independent)
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+ :element-type 'int32)))
    (is (= 5 (row-major-aref array 9)))
    (is (= 9 (row-major-aref array 18)))
    (is (= 10 (row-major-aref (aref array nil 1) 9)))
    (is (= 1 (row-major-aref (aref (aref array '(-1 :step -1))
                                   '(9))
                             1)))))

(def-test setf-row-major-aref (:suite backend-independent)
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)) :element-type 'int32)
              (let ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (row-major-aref array 4) 0)
                array)))
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)) :element-type 'int32)
              (let ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (row-major-aref (aref array nil 1) 1) 0)
                array))))
