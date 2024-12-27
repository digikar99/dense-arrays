(in-package :dense-arrays)

(define-condition invalid-array-index-for-axis (invalid-array-index)
  ((axis  :initarg :axis)
   (valid :initarg :valid :initform nil))
  (:report (lambda (c s)
             (with-slots (axis index valid) c
               (format s "Invalid index ~A for array axis of length ~A."
                       index axis)
               (when valid
                 (format s "~%Valid index range is from ~A to ~A (both inclusive)."
                         (car valid) (cdr valid)))))))

(defun array= (array1 array2 &key (test #'equalp))
  "Returns non-NIL if each element of ARRAY1 is equal to each corresponding
element of ARRAY2 using TEST, which should be a two-argument function that takes
the one element of the first array and the corresponding element of the second
and tests for their equality."
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
  (let ((normalized-index (if (< index 0)
                              (+ index dimension)
                              index)))
    (declare (type int-index normalized-index))
    (if (and (<= 0 normalized-index) (< normalized-index dimension))
        (the-size normalized-index)
        (error 'invalid-array-index-for-axis :index index :axis dimension
               :valid (cons (- dimension) (1- dimension))))))

(defun %aref-view (array &rest subscripts)
  "Returns a VIEW of the subscripted ARRAY (without copying the contents)"
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
                                     end   (1+ (normalize-index (1- end)   d)))
                              (when (and (< step 0) (not endp))
                                (setq end -1))
                              (push (ceiling (- end start) step) new-dimensions)
                              (push (the-int-index (* s step)) new-strides)
                              (push (the-size (+ o offset-carry (the-int-index (* start s))))
                                    new-offsets)
                              (setq offset-carry 0)))
                           (t (error 'invalid-array-index :array array :index ss))))
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

(defpolymorph (aref :inline t :static-dispatch-name da-ref)
    ((array dense-array) &rest subscripts) t
  (declare (optimize speed)
           (type list subscripts))
  (assert (and (= (the-size (array-rank array))
                  (length subscripts))
               (every (lambda (s d)
                        (and (integerp s)
                             (locally (declare (type int-index s d))
                               (<= 0 s (1- d)))))
                      subscripts
                      (narray-dimensions array)))
          (array subscripts)
          'invalid-array-index
          :index subscripts
          :array array
          :suggestion "Did you mean to use DENSE-ARRAYS:AREF* ?")
  (let* ((dense-array-class (class-of array)))
    (funcall (fdefinition (storage-accessor dense-array-class))
             (array-storage array)
             (the size
                  (let ((index 0))
                    (declare (type size index))
                    (loop :for stride :of-type size :in (array-strides array)
                          :for subscript :of-type size :in subscripts
                          :for offset :of-type size :in (array-offsets array)
                          :do (incf index (+ offset
                                             (the-size (* stride subscript)))))
                    index)))))

(defun aref* (dense-array &rest subscripts)
  "Accessor function for DENSE-ARRAYS::DENSE-ARRAY with semantics
intended to be similar to numpy's indexing semantics.
See https://numpy.org/doc/stable/user/basics.indexing.html

Each element of SUBSCRIPTS can be
- either an integer denoting the position within the axis which is to be indexed
- or a list of the form (&OPTIONAL START &KEY END STEP) with each of START END
  STEP being integers if supplied. START denotes the start position within the
  axis, END denotes the ending position within the axis, STEP denotes at what
  distance within the axis the next element should come after the previous,
  starting from START

Each of the SUBSCRIPTS, START, END, STEP can also be negative integers, in which
case the last element along the axis is given the index -1, the second last is
given the index -2 and so on. Thus, `(aref ... '(-1 :step -1))` can reverse a one
dimensional array.

Like, CL:AREF or ABSTRACT-ARRAYS:AREF, returns the element corresponding to SUBSCRIPTS
if all the subscripts are integers and there as many subscripts
as the rank of the array.

If the number (aka length) of SUBSCRIPTS were less than the array's rank, or
if some of the SUBSCRIPTS were lists described above, then returns a VIEW
of the arrays. A VIEW is a window into the original array and thus
avoids copying the elements of the original array.

Examples illustrating the numpy-equivalent indexes:

    a[::]       (aref a nil)
    a[::2]      (aref a '(0 :step 2))
    a[3, ::-1]  (aref a 3 '(-1 :step -1))
    a[3::, -1]  (aref a '(3) -1)

The SUBSCRIPTS can also be integer or boolean arrays, denoting which elements
to select from each of the axes. But in this case the corresponding elements
of the array are copied over into a new array."
  (declare ;; (optimize speed)
   (type dense-array dense-array)
   (dynamic-extent subscripts))
  (cond ((and (= (array-rank dense-array) (length subscripts))
              (every #'integerp subscripts))
         (let* ((dense-array-class (class-of dense-array)))
           (funcall (storage-accessor dense-array-class)
                    (array-storage dense-array)
                    (the int-index
                         (let ((index 0))
                           (declare (type int-index index))
                           (loop :for stride :of-type int-index
                                   :in (array-strides dense-array)
                                 :for subscript :of-type int-index :in subscripts
                                 :for offset :of-type int-index
                                   :in (array-offsets dense-array)
                                 :for dimension :of-type int-index
                                   :in (narray-dimensions dense-array)
                                 :do (incf index
                                         (+ offset
                                            (the-int-index
                                             (* stride
                                                (normalize-index subscript dimension))))))
                           index)))))
        ((or (some #'cl:arrayp subscripts)
             (some #'arrayp subscripts))
         (apply #'%aref dense-array subscripts))
        (t
         (apply #'%aref-view dense-array subscripts))))

(defun (setf %aref-view) (new-array array &rest subscripts)
  ;; TODO: Optimize
  (declare (dynamic-extent subscripts))
  (let* ((sub-array         (apply #'aref* array subscripts))
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
                                                (apply #'aref* array ss-idx))
                                              subscripts)))
                          (setf (apply #'aref* array ss-elt)
                                (assert-type (apply #'aref* new-array ss-idx)
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

(defpolymorph ((setf aref) :inline t)
    (new-element/s (array dense-array) &rest subscripts)
    t
  (declare (type dense-array array)
           (type list subscripts)
           (optimize speed)
           (dynamic-extent subscripts))
  (assert (and (= (array-rank array)
                  (length subscripts))
               (every (lambda (s d)
                        (and (integerp s)
                             (locally (declare (type int-index s d))
                               (<= 0 s (1- d)))))
                      subscripts
                      (narray-dimensions array)))
          (array subscripts)
          'invalid-array-index
          :index subscripts
          :array array
          :suggestion "Did you mean to use (SETF DENSE-ARRAYS:AREF*) ?")
  (with-slots (storage element-type strides offsets dimensions rank) array
    (assert-type new-element/s element-type)
    (let* ((dense-array-class (class-of array)))
      (funcall (fdefinition `(setf ,(storage-accessor dense-array-class)))
               new-element/s
               storage
               (let ((index 0))
                 (declare (type size index))
                 (loop :for stride :of-type int-index :in strides
                       :for subscript :of-type int-index :in subscripts
                       :for offset :of-type size :in offsets
                       :do (incf index
                               (the-size
                                (+ offset
                                   (the-int-index (* stride subscript))))))
                 index))))
  new-element/s)

(defun (setf aref*) (new-element/s dense-array &rest subscripts)
  (declare (type dense-array dense-array)
           ;; (optimize speed)
           (dynamic-extent subscripts))
  (with-slots (storage element-type strides offsets dimensions rank) dense-array
    (cond ((and (= rank (length subscripts))
                (every #'integerp subscripts))
           (assert-type new-element/s (array-element-type dense-array))
           (let* ((dense-array-class (class-of dense-array)))
             (funcall (fdefinition `(setf ,(storage-accessor dense-array-class)))
                      new-element/s
                      storage
                      (let ((index 0))
                        (declare (type size index))
                        (loop :for stride :of-type int-index :in strides
                              :for subscript :of-type int-index :in subscripts
                              :for offset :of-type size :in offsets
                              :for dimension :of-type size
                                :in (narray-dimensions dense-array)
                              :do (incf index
                                        (the-size
                                         (+ offset
                                            (the-size
                                             (* stride
                                                (normalize-index subscript dimension)))))))
                        index))))
          ((or (some #'cl:arrayp subscripts)
               (some #'arrayp subscripts))
           (apply #'(setf %aref) new-element/s dense-array subscripts))
          (t
           (apply #'(setf %aref-view) new-element/s dense-array subscripts))))
  new-element/s)

(def-test aref* (:suite backend-independent)
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+ :element-type 'int32)))
    (is (= 9 (aref* array 9 0)))
    (is (= 9 (aref* array 8 1)))
    (is (= 9 (aref* array -2 -1)))
    (is (array= array (aref* array)))
    (is (array= array (aref* array nil)))
    (is (array= array (aref* array nil nil)))
    (is (array= (aref* array 0)
                (make-array 2 :constructor #'+ :element-type 'int32)))
    (is (array= (aref* array 0 nil)
                (make-array 2 :constructor #'+ :element-type 'int32)))
    (is (array= (aref* array nil 0)
                (make-array 10 :constructor #'+ :element-type 'int32)))
    (is (array= (make-array 5 :initial-contents '(5 4 3 2 1) :element-type 'int32)
                (aref* (make-array 5 :initial-contents '(1 2 3 4 5) :element-type 'int32)
                       '(-1 :step -1))))
    (is (array= (aref* (aref* array '(2 :end 4)) () 0)
                (make-array '(2) :initial-contents '(2 3)
                                 :element-type 'int32))))
  (symbol-macrolet ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
    (is (equalp '(2 2) (array-dimensions (aref* array nil '(0 :step 2)))))
    (is (equalp '(1 3) (array-dimensions (aref* array '(0 :step 2)))))
    (is (= 0 (aref* (aref* array '(0 :step 2)) 0 0))))
  (is (array= (aref* (make-array '(5 5) :constructor #'+ :element-type 'int32)
                     '(1) '(1))
              (make-array '(4 4) :constructor (lambda (x y) (+ 2 x y))
                                 :element-type 'int32))))

(def-test advanced-aref (:suite backend-dependent)
  (is (array= (make-array '(2 2) :initial-contents '((2 6) (1 4)) :element-type 'int32)
              (aref* (make-array '(2 3)
                                 :element-type 'int32
                                 :initial-contents '((1 2 3) (4 5 6)))
                     (make-array '(2 2)
                                 :element-type 'int32
                                 :initial-contents '((0 1) (0 1)))
                     (make-array '(2 2)
                                 :element-type 'int32
                                 :initial-contents '((1 2) (0 0))))))
  (is (array= (make-array '(3) :initial-contents '(1 3 6) :element-type 'int32)
              (aref* (make-array '(2 3)
                                 :element-type 'int32
                                 :initial-contents '((1 2 3) (4 5 6)))
                     (make-array '(2 3)
                                 :element-type 'bit
                                 :initial-contents '((1 0 1) (0 0 1)))))))

(def-test setf-aref* (:suite backend-independent)
  ;; The array constructed in BROADCAST-ARRAY has an implicit assumption
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 2 -1)) :element-type 'int32)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (aref* a 1 2) -1)
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 2 -1)) :element-type 'int32)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (aref* a -1 -1) -1)
                a)))
  (is (array= (make-array '(2 3 4) :initial-element 1 :element-type 'int32)
              (let ((a (make-array '(2 3 4) :initial-element 0 :element-type 'int32)))
                (setf (aref* a) 1)
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)) :element-type 'int32)
              (let ((a (make-array '(2 3) :initial-element 0 :element-type 'int32)))
                (setf (aref* a nil 0)
                      (make-array 2 :initial-contents '(2 3) :element-type 'int32))
                a)))
  (is (array= (make-array '(2 3) :initial-contents '((2 0 0) (3 0 0)) :element-type 'int32)
              (let ((a (make-array '(2 3) :initial-element 0 :element-type 'int32)))
                (setf (aref* a nil -3)
                      (make-array 2 :initial-contents '(2 3) :element-type 'int32))
                a))))

(def-test setf-advanced-aref (:suite backend-dependent)
  (is (array= (make-array '(2 3) :initial-contents '((2 2 2) (4 5 2)) :element-type 'int32)
              (let ((a (make-array '(2 3)
                                   :initial-contents '((1 2 3) (4 5 6)) :element-type 'int32)))
                (setf (aref* a (make-array '(2 3)
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

(def-test row-major-aref (:suite backend-independent)
  (symbol-macrolet ((array (make-array '(10 2) :constructor #'+ :element-type 'int32)))
    (is (= 5 (row-major-aref array 9)))
    (is (= 9 (row-major-aref array 18)))
    (is (= 10 (row-major-aref (aref* array nil 1) 9)))
    (is (= 1 (row-major-aref (aref* (aref* array '(-1 :step -1))
                                    '(9))
                             1)))))

(def-test setf-row-major-aref (:suite backend-independent)
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)) :element-type 'int32)
              (let ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (row-major-aref array 4) 0)
                array)))
  (is (array= (make-array '(2 3) :initial-contents '((0 1 2) (1 0 3)) :element-type 'int32)
              (let ((array (make-array '(2 3) :constructor #'+ :element-type 'int32)))
                (setf (row-major-aref (aref* array nil 1) 1) 0)
                array))))
