(in-package :dense-arrays)

(in-suite :dense-arrays)

(def-test array-type ()
  (is (subtypep '(array single-float) 'array))
  (is (subtypep '(array single-float 2) '(array single-float 2)))
  (is (subtypep '(array single-float 2) 'array)))

(defun dimensions->strides (dimensions)
  (cond ((null dimensions) ())
        ((null (rest dimensions))
         (list 1))
        (t
         (let ((strides-so-far (dimensions->strides (rest dimensions))))
           (cons (* (first strides-so-far) (second dimensions))
                 strides-so-far)))))

(def-test dimensions->strides ()
  (is (equalp '()  (dimensions->strides '())))
  (is (equalp '(1) (dimensions->strides '(1))))
  (is (equalp '(1) (dimensions->strides '(5))))
  (is (equalp '(2 1)
              (dimensions->strides '(3 2))))
  (is (equalp '(6 2 1)
              (dimensions->strides '(1 3 2)))))

(defmacro ensure-single (&rest symbols)
  `(when (rest (remove-if #'null (list ,@symbols)))
     (error "Exactly one of~%  ~{~S~^~%  ~}~%expected but supplied:~%  ~{~S~^~%  ~}"
            ',symbols
            (loop :for sym :in ',symbols
                  :for val :in (list ,@symbols)
                  :if val :collect sym))))

(defun make-array (dimensions &rest args
                   &key (element-type t)

                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     (constructor nil constructor-p)

                     (strides nil strides-p)
                     (adjustable nil adjustable-p)
                     (fill-pointer nil fill-pointer-p)

                     (displaced-to nil displaced-to-p)
                     (offsets nil offsets-p)
                     (displaced-index-offset 0 displaced-index-offset-p))
  ;; TODO: Handle adjustable
  ;; TODO: Handle fill-pointer
  ;; TODO: Sanitize displaced-to and perhaps, displaced-index-offset
  ;; TODO: Take displaced-index-offset and strides into account while calculating dimensions
  (declare ;; (optimize speed)
   (ignore args adjustable fill-pointer displaced-to-p)
   (type function-designator constructor))
  (ensure-single initial-element-p
                 initial-contents-p
                 constructor-p)
  (ensure-single offsets-p
                 displaced-index-offset-p)
  (when fill-pointer-p
    (error "FILL-POINTER has not been handled yet in DENSE-ARRAY"))
  (when adjustable-p
    (error "ADJUSTABLE has not been handled yet in DENSE-ARRAY"))

  (let* ((dimensions (if (listp dimensions)
                         dimensions
                         (list dimensions)))
         (displaced-vector-initial-element
           (cond (initial-element-p initial-element)
                 (t
                  (case element-type
                    ;; TODO: Include more types?
                    (single-float 0.0s0)
                    (double-float 0.0d0)
                    (t 0)))))
         (rank (length dimensions))
         (total-size (apply #'* dimensions))
         (displaced-to (cond (displaced-to displaced-to)
                             (t (cl:make-array
                                 total-size
                                 :initial-element displaced-vector-initial-element
                                 :element-type element-type))))
         (offsets (if displaced-index-offset
                      (nconc (list displaced-index-offset)
                             (make-list (1- rank) :initial-element 0))
                      offsets))
         (strides (if strides-p
                      strides
                      (dimensions->strides dimensions))))
    (cond (constructor-p
           (let ((row-major-index 0))
             (declare (type size row-major-index))
             ;; To avoid repeated de-allocation of subscripts, we do this convoluted work
             ;; Uncomment the 'print' to see what is happening
             (labels ((construct (r &optional (stride (first strides))
                                  &rest subscripts)
                        (declare (optimize debug)
                                 (type int-index r stride)
                                 (ignorable stride))
                        ;; (print r)
                        ;; (princ (list row-major-index :stride stride subscripts))
                        (if (< r 0)
                            (setf (cl:aref displaced-to row-major-index)
                                  (apply constructor subscripts))
                            (loop :for i :of-type size :below (nth r dimensions)
                                  :with 1-r :of-type int-index := (1- r)
                                  :with s :of-type size := (nth r strides)
                                  :do (apply #'construct
                                             1-r
                                             s
                                             i
                                             subscripts)
                                      (incf row-major-index s)
                                  :finally (decf row-major-index
                                                 (the size (* (the size (nth r dimensions))
                                                              (the size (nth r strides)))))))))
               (construct (1- rank)))))
          (initial-contents-p
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index)
                      (optimize (speed 1)))
             (labels ((set-displaced-to (elt)
                        (typecase elt
                          (list
                           (loop :for e :in elt
                                 :do (set-displaced-to e)))
                          (string
                           (setf (cl:aref displaced-to row-major-index) elt)
                           (incf row-major-index))
                          (cl:vector
                           (loop :for e :across elt
                                 :do (set-displaced-to e)))
                          (t
                           (setf (cl:aref displaced-to row-major-index) elt)
                           (incf row-major-index)))))
               (set-displaced-to initial-contents)))))
    (let* ((array (make-dense-array :displaced-to displaced-to
                                    :element-type (cl:array-element-type displaced-to)
                                    :dim dimensions
                                    :strides strides
                                    :offsets offsets
                                    :contiguous-p t
                                    :total-size (apply #'* dimensions)
                                    :root-array nil
                                    :rank (length dimensions))))
      array)))

(def-test make-array ()
  (is (equalp #(0 1 2 1 2 3)
              (array-displaced-to (make-array '(2 3) :constructor #'+ :element-type 'int32))))
  (is (equalp #(0 1 2 3 1 2 3 4 2 3 4 5 1 2 3 4 2 3 4 5 3 4 5 6)
              (array-displaced-to (make-array '(2 3 4)
                                              :constructor #'+ :element-type 'int32))))

  (symbol-macrolet ((a (make-array 0 :element-type 'int32)))
    (is (typep a '(array int32)))
    (is (typep a '(array (signed-byte 32)))))
  (unless-static-vectors (1)
   (is (equalp #("hello" "goodbye")
               (array-displaced-to (make-array 2 :initial-contents '("hello" "goodbye")))))))

;; trivial function definitions

(declaim (ftype (function (array) list) array-dimensions narray-dimensions))

(defun array-dimensions (array)
  "Returns a copy of the dimensions-list of the ARRAY.
Use NARRAY-DIMENSIONS to avoid the copy."
  (declare (type array array)
           ;; (optimize speed)
           )
  (copy-list (array-dim array)))

(defun narray-dimensions (array)
  "Returns the dimensions-list of the ARRAY. The list is not expected to be modified."
  (declare ;; (optimize speed)
           (type array array))
  (array-dim array))

(defun array-dimension (array axis-number)
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (declare (type array array)
           ;; (optimize speed)
           (type fixnum axis-number))
  (elt (narray-dimensions array) axis-number))

(defun array-stride (array axis-number)
  "Return the length of stride AXIS-NUMBER of ARRAY."
  (declare (type array array)
           ;; (optimize speed)
           (type fixnum axis-number))
  (elt (array-strides array) axis-number))

(defun array-offset (array axis-number)
  "Return the length of offset AXIS-NUMBER of ARRAY."
  (declare (type array array)
           ;; (optimize speed)
           (type size axis-number))
  (elt (array-offsets array) axis-number))

(defun 1d-storage-array (array)
  "Returns the storage-vector underlying the ARRAY. This is equivalent to ARRAY-DISPLACED-TO."
  (declare (type array array))
  (array-displaced-to array))

(defun array-view-p (array)
  "Returns T if the ARRAY is a VIEW, otherwise returns NIL"
  (declare (type array array))
  (if (array-root-array array) t nil))

(defun array-displaced-index-offset (array)
  (declare (type array array))
  (let ((offsets (array-offsets array)))
    (if (every #'zerop (rest offsets))
        (first offsets)
        (error "Supplied array is displaced along multipe axes. Offsets: ~S" offsets))))

(defun collect-reduce-from-end (function list initial-element)
  (declare (type list list)
           (optimize speed))
  (cond ((null list) (list initial-element))
        (t (let ((collect-reduced (collect-reduce-from-end function
                                                           (rest list)
                                                           initial-element)))
             (cons (funcall function
                            (first list)
                            (first collect-reduced))
                   collect-reduced)))))

(define-condition print-lines-exhausted (condition)
  ((axis-number :initarg :axis-number :accessor axis-number)))

(defvar *array-element-print-format* nil
  "The format constrol string used to print the elements of DENSE-ARRAYS:ARRAY.")

(defmethod print-object ((array dense-array) stream)
  ;; (print (type-of array))
  (let ((*print-right-margin* (or *print-right-margin* 80))
        (sv     (array-displaced-to array))
        (rank   (array-rank array))
        (rank-1 (1- (array-rank array)))
        (lines  3)
        (*axis-number* 0)
        (indent 3)
        (fmt-control (or *array-element-print-format*
                         (switch ((array-element-type array) :test #'type=)
                           ('double-float "~,15,3@e")
                           ('single-float "~,7,2@e")
                           (t             "~s")))))
    ;; Do this before just to save some horizontal space
    (declare (special *axis-number*))
    (print-unreadable-object (array stream :identity t)
      (when *print-array*
        ;; header
        (format stream "DENSE-ARRAYS:ARRAY ~A~S ~{~S~^x~}"
                (if (array-view-p array) "(VIEW) " "")
                (array-element-type array)
                (narray-dimensions array))
        ;; elements
        ;; DONE: *print-level*
        ;; DONE: *print-lines*
        ;; DONE: *print-length*
        ;; TODO: Simplify this
        (unless (or (zerop (array-total-size array))
                    (and *print-lines* (< *print-lines* 3))
                    (and *print-level*
                         (< *print-level* 1)
                         (progn
                           (write-string " #" stream)
                           t)))
          (labels
              ((check-lines ()
                 (when (and *print-lines* (> lines *print-lines*))
                   (signal 'print-lines-exhausted :axis-number *axis-number*)))
               (print-respecting-margin (object column)
                 (declare (type fixnum column))
                 (let ((string (format nil fmt-control object)))
                   (cond ((> (+ column (length string))
                             *print-right-margin*)
                          (terpri stream)
                          (check-lines)
                          (incf lines)
                          (write-string string stream)
                          nil)
                         (t
                          (write-string string stream)
                          (length string)))))
               (print-level-reached-p ()
                 (and *print-level* (>= (1+ *axis-number*) *print-level*)))
               ;; TODO: Handle multi-axis offsets
               (print-array (offset start indent &optional column)
                 ;; (print (list offset start))
                 (let ((column (or column indent)))
                   (flet ((newline ()
                            (check-lines)
                            (incf lines)
                            (terpri stream)
                            (dotimes (i indent) (write-char #\space stream))
                            (setq column indent)))
                     (cond ((= *axis-number* rank)
                            (print-respecting-margin (cl:aref sv (+ offset start)) column))
                           (t
                            (let ((dim               (array-dimension array *axis-number*))
                                  (stride            (array-stride array *axis-number*))
                                  (additional-offset (array-offset array *axis-number*)))
                              (when (zerop *axis-number*)
                                (newline))
                              (loop :for i :from 0
                                    :repeat (if *print-length*
                                                (min *print-length*
                                                     dim)
                                                dim)
                                    :with last := (1- dim)
                                    :do (cond
                                          ((print-level-reached-p)
                                           (let ((rv (print-respecting-margin "#" column)))
                                             (if rv
                                                 (incf column rv)
                                                 (setq column indent))
                                             (unless (= i last)
                                               (incf column)
                                               (write-char #\space stream))))
                                          (t
                                           (unless (= *axis-number* rank-1)
                                             (incf column)
                                             (write-char #\( stream))
                                           (let* ((*axis-number* (1+ *axis-number*))
                                                  (rv (print-array (+ offset additional-offset)
                                                                   (+ start (* stride i))
                                                                   (1+ indent)
                                                                   column)))
                                             (declare (special *axis-number*))
                                             (if rv
                                                 (incf column rv)
                                                 (setq column indent)))
                                           (if (= *axis-number* rank-1)
                                               (unless (= i last)
                                                 (incf column)
                                                 (write-char #\space stream))
                                               (progn
                                                 (write-char #\) stream)
                                                 (unless (= i last)
                                                   (newline)))))))
                              (if (and *print-length*
                                       (> dim *print-length*))
                                  (write-string "..." stream)
                                  (write-string "" stream))
                              0)))))))
            (handler-case
                (progn
                  (print-array 0
                               0
                               indent)
                  (terpri stream))
              (print-lines-exhausted (condition)
                (write-string " ..." stream)
                (dotimes (i (axis-number condition)) (write-char #\) stream))
                (terpri stream)))))))))

(defun print-array (array &optional array-element-print-format &key level length
                                                                 (stream nil streamp))
  "Prints ARRAY as if by CL:PRINT.
Format recipes: http://www.gigamonkeys.com/book/a-few-format-recipes.html."
  (let ((*array-element-print-format* (or array-element-print-format
                                          *array-element-print-format*))
        (*print-level*  (or level *print-level*))
        (*print-length* (or length *print-length*)))
    (if streamp
        (print array stream)
        (print array)))
  nil)

(def-test print-array ()
  (unless-static-vectors (7)
   (symbol-macrolet ((array (make-array '(5 5) :initial-element 'hello)))
     (macrolet ((lines (n)
                  `(with-output-to-string (*standard-output*)
                     (let ((*print-lines* ,n))
                       (print-array array "~D"))))
                (len (n)
                  `(with-output-to-string (*standard-output*)
                     (let ((*print-length* ,n))
                       (print-array array "~D"))))
                (level (n)
                  `(with-output-to-string (*standard-output*)
                     (let ((*print-level* ,n))
                       (print-array array "~D")))))
       (is (<= (count #\newline (lines 3))
               3))
       (is (<= (count #\newline (lines 2))
               2))
       (is (>= 3 (count #\newline (len 0))))
       (is (>= 4 (count #\newline (len 1))))
       (is (= 5 (count #\newline (len 2))))
       (is (= 1 (count #\newline (level 0))))
       (is (= 3 (count #\newline (level 1))))))))
