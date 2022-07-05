(in-package :dense-arrays)

(in-suite backend-independent)

(def-test array-type ()
  (is (subtypep '(array single-float) 'array))
  (is (subtypep '(array single-float 2) '(array single-float 2)))
  (is (subtypep '(array single-float 2) 'array)))

(defun dimensions->strides (dimensions layout)
  (ecase layout
    (:column-major (nreverse (dimensions->strides (reverse dimensions) :row-major)))
    ((:row-major nil)
     (cond ((null dimensions) ())
           ((null (rest dimensions))
            (list 1))
           (t
            (let ((strides-so-far (dimensions->strides (rest dimensions) :row-major)))
              (cons (* (first strides-so-far) (second dimensions))
                    strides-so-far)))))))

(def-test dimensions->strides ()
  (is (equalp '()  (dimensions->strides '()  :row-major)))
  (is (equalp '(1) (dimensions->strides '(1) :row-major)))
  (is (equalp '(1) (dimensions->strides '(5) :row-major)))
  (is (equalp '(2 1)
              (dimensions->strides '(3 2) :row-major)))
  (is (equalp '(6 2 1)
              (dimensions->strides '(1 3 2) :row-major))))

(defmacro ensure-single (&rest symbols)
  `(when (rest (remove-if #'null (list ,@symbols)))
     (error "Exactly one of~%  ~{~S~^~%  ~}~%expected but supplied:~%  ~{~S~^~%  ~}"
            ',symbols
            (loop :for sym :in ',symbols
                  :for val :in (list ,@symbols)
                  :if val :collect sym))))

(defun assert-type (value type &optional datum &rest datum-args)
  (if (typep value type)
      value
      (if datum
          (apply #'warn datum datum-args)
          (warn "Expectation unmet for ~S to be of type ~S" value type))))

(declaim (ftype (function * dense-array) make-array))
(defun make-array (dimensions &rest args
                   &key (element-type default-element-type)

                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     (constructor nil constructor-p)

                     (strides nil strides-p)
                     (adjustable nil adjustable-p)
                     (fill-pointer nil fill-pointer-p)
                     (class *dense-array-class*)
                     (layout *array-layout*)

                     (displaced-to nil displaced-to-p)
                     (offsets nil offsets-p)
                     (displaced-index-offset 0 displaced-index-offset-p))
  ;; TODO: Handle adjustable
  ;; TODO: Handle fill-pointer
  ;; TODO: Sanitize displaced-to and perhaps, displaced-index-offset
  ;; TODO: Take displaced-index-offset and strides into account while calculating dimensions
  (declare ;; (optimize speed)
   (ignore args adjustable fill-pointer displaced-to-p)
   (type function-designator constructor)
   (type (member :row-major :column-major) layout))
  (ensure-single initial-element-p
                 initial-contents-p
                 constructor-p)
  (ensure-single offsets-p
                 displaced-index-offset-p)
  (when fill-pointer-p
    (error "FILL-POINTER has not been handled yet in DENSE-ARRAY"))
  (when adjustable-p
    (error "ADJUSTABLE has not been handled yet in DENSE-ARRAY"))

  (let* ((dimensions      (ensure-list dimensions))
         (rank            (length dimensions))
         (total-size      (apply #'* dimensions))
         (strides         (if strides-p
                              strides
                              (dimensions->strides dimensions layout)))

         ;; FIXME: Handle displaced-index-offset correctly
         (offsets         (if displaced-index-offset
                              (if (= rank 0)
                                  ()
                                  (nconc (list displaced-index-offset)
                                         (make-list (1- rank) :initial-element 0)))
                              offsets))

         (class           (typecase class
                            (class class)
                            (t (find-class class))))
         (element-type    (funcall (storage-element-type-upgrader class)
                                   element-type))
         (initial-element (let ((elt (cond (initial-element-p initial-element)
                                           (t (switch (element-type :test #'type=)
                                                ('single-float 0.0f0)
                                                ('double-float 0.0d0)
                                                (t 0))))))
                            (unless (or initial-contents
                                        constructor)
                              (assert-type elt element-type
                                           "The initial element ~S is not of type ~S"
                                           elt element-type))
                            elt))
         (storage         (or displaced-to
                              (funcall (storage-allocator class)
                                       total-size
                                       :initial-element initial-element
                                       :element-type element-type)))

         (dense-array     (make-instance class
                                         :storage storage
                                         :element-type element-type
                                         :dimensions dimensions
                                         :strides strides
                                         :offsets offsets
                                         :total-size (apply #'* dimensions)
                                         :root-array nil
                                         :rank (length dimensions)
                                         :layout layout))
         (storage-accessor (storage-accessor class))
         (storage-deallocator (storage-deallocator class)))
    (when storage-deallocator
      (trivial-garbage:finalize dense-array
                                (lambda () (funcall storage-deallocator storage))))
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
                            (funcall (fdefinition `(setf ,storage-accessor))
                                     (assert-type (apply constructor subscripts)
                                                  element-type)
                                     storage row-major-index)
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
             (labels ((set-displaced-to (elt axis)
                        (typecase elt
                          (list
                           (loop :for e :in elt
                                 :do (set-displaced-to e (1+ axis))
                                     (incf row-major-index (nth axis strides))
                                 :finally (decf row-major-index (* (nth axis dimensions)
                                                                   (nth axis strides)))))
                          (string
                           (funcall (fdefinition `(setf ,storage-accessor))
                                    (assert-type elt element-type)
                                    storage row-major-index))
                          (cl:vector
                           (loop :for e :across elt
                                 :do (set-displaced-to e (1+ axis))
                                     (incf row-major-index (nth axis strides))
                                 :finally (decf row-major-index (* (nth axis dimensions)
                                                                   (nth axis strides)))))
                          (t
                           (funcall (fdefinition `(setf ,storage-accessor))
                                    (assert-type elt element-type)
                                    storage row-major-index)))))
               (set-displaced-to initial-contents 0)))))
    dense-array))

(def-test make-array (:suite backend-dependent)
  (is (equalp #(0 1 2 1 2 3)
              (array-storage (make-array '(2 3) :constructor #'+ :element-type 'int32
                                         :layout :row-major))))
  (is (equalp #(0 1 2 3 1 2 3 4 2 3 4 5 1 2 3 4 2 3 4 5 3 4 5 6)
              (array-storage (make-array '(2 3 4) :layout :row-major
                                                  :constructor #'+ :element-type 'int32))))
  (is (equalp #(0 1 1 2 2 3)
              (array-storage (make-array '(2 3) :constructor #'+ :element-type 'int32
                                         :layout :column-major))))

  (symbol-macrolet ((a (make-array 0 :element-type 'int32)))
    (is (typep a '(%dense-array int32)))
    (is (typep a '(%dense-array (signed-byte 32)))))
  (is (equalp #("hello" "goodbye")
              (array-storage (make-array 2 :initial-contents '("hello" "goodbye")
                                           :class 'standard-dense-array)))))

;; trivial function definitions

(declaim (ftype (function (dense-array) (member :row-major :column-major nil))
                array-layout))
(declaim (inline array-layout))
(defun array-layout (array)
  (declare (type dense-array array))
  (dense-array-layout array))

(declaim (ftype (function (dense-array) list)
                narray-dimensions
                array-strides
                array-offsets))

(declaim (inline array-displaced-to))
(defun array-displaced-to (array)
  (declare (type dense-array array))
  (array-storage array))

(declaim (inline array-displacement))
(defun array-displacement (array)
  "Returns two values:
- ARRAY-STORAGE
- and OFFSET along first axis
Consequences are undefined if ARRAY is displaced along multiple axis."
  (declare (type dense-array array))
  (values (array-storage array)
          (first (array-offsets array))))

(declaim (inline narray-dimensions))
(defun narray-dimensions (array)
  "Returns the dimensions-list of the ARRAY. The list is not expected to be modified."
  (declare (type dense-array array))
  (abstract-array-dimensions array))

(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (declare (type dense-array array)
           (type fixnum axis-number))
  (elt (narray-dimensions array) axis-number))

(declaim (inline array-strides))
(defun array-strides (array)
  (declare (type dense-array array))
  (dense-array-strides array))

(declaim (inline array-stride))
(defun array-stride (array axis-number)
  "Return the length of stride AXIS-NUMBER of ARRAY."
  (declare (type dense-array array)
           (type fixnum axis-number))
  (elt (array-strides array) axis-number))

(declaim (inline array-offsets))
(defun array-offsets (array)
  (declare (type dense-array array))
  (dense-array-offsets array))

(declaim (inline array-offset))
(defun array-offset (array axis-number)
  "Return the length of offset AXIS-NUMBER of ARRAY."
  (declare (type dense-array array)
           ;; (optimize speed)
           (type size axis-number))
  (elt (array-offsets array) axis-number))

(defun 1d-storage-array (array)
  "Returns the storage-vector underlying the ARRAY. This is equivalent to ARRAY-DISPLACED-TO."
  (declare (type dense-array array))
  (array-displaced-to array))

(defun array-view-p (array)
  "Returns T if the ARRAY is a VIEW, otherwise returns NIL"
  (declare (type dense-array array))
  (if (dense-array-root-array array) t nil))

(defun array-displaced-index-offset (array)
  (declare (type dense-array array))
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

(defun pretty-print-number (stream arg colon-modifier-p at-modifier-p)
  (declare (ignore colon-modifier-p at-modifier-p))
  (unless (typep arg 'number)
    (return-from pretty-print-number (print-object arg stream)))
  (let ((number arg))
    (if (or (>= (abs number) 100) (< (abs number) 0.001))
        (format stream
                (typecase number
                  (float (if (< number 0)
                             " ~9,3,2e"
                             " ~10,3,2e"))
                  (t     "~s"))
              arg)
        (format stream
                (typecase number
                  (float (if (< number 0)
                             "~7,3,,,f    "
                             "~7,3,,,f    "))
                  (t      "~s"))
              arg))))

;;; FIXME: Someone read up http://www.lispworks.com/documentation/lw51/CLHS/Body/22_b.htm
;;; to do things better
(defvar *array-element-print-format* "~/DENSE-ARRAYS::PRETTY-PRINT-NUMBER/"
  "The format control string used to print the elements of DENSE-ARRAYS:ARRAY.

It is possible to set this value to \"~/USER-DEFINED-FUNCTION/\" where
USER-DEFINED-FUNCTION should accept at least four arguments.

Also see:
- https://en.wikipedia.org/wiki/Format_(Common_Lisp)
- http://www.gigamonkeys.com/book/a-few-format-recipes.html")

(defmethod print-object ((array dense-array) stream)
  ;; (print (type-of array))
  (let* ((*print-right-margin* (or *print-right-margin* 80))
         (sv      (array-storage array))
         (layout  (array-layout array))
         (rank    (array-rank array))
         (rank-1  (1- (array-rank array)))
         (lines   3)
         (*axis-number* 0)
         (indent  3)
         (fmt-control (or *array-element-print-format*
                          (switch ((array-element-type array) :test #'type=)
                            ('double-float "~,15,3@e")
                            ('single-float "~,7,2@e")
                            (t             "~s")))))
    ;; Do this before just to save some horizontal space
    (declare (special *axis-number*))
    (print-unreadable-object (array stream :identity t :type t)
      ;; header
      (format stream "~S " layout)
      (if (zerop rank)
          (format stream "NIL ~S"
                  (array-element-type array))
          (format stream "~{~S~^x~} ~S"
                  (narray-dimensions array)
                  (array-element-type array)))
      (when *print-array*
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
               (print-respecting-margin (object column indent)
                 (declare (type fixnum column))
                 (let ((string (format nil fmt-control object)))
                   (cond ((> (+ column (length string))
                             *print-right-margin*)
                          (terpri stream)
                          (check-lines)
                          (incf lines)
                          (dotimes (i (1- indent)) (write-char #\space stream))
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
                            (print-respecting-margin (aref sv (+ offset start))
                                                     column
                                                     indent))
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
                                           (let ((rv (print-respecting-margin "#" column indent)))
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
                (if (zerop rank)
                    (format stream fmt-control (aref sv 0))
                    (progn
                      (print-array 0
                                   0
                                   indent)
                      (terpri stream)))
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
  (symbol-macrolet ((array (make-array '(5 5) :initial-element 'hello
                                       :class 'standard-dense-array)))
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
      (is (= 3 (count #\newline (level 1)))))))
