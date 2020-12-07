#.(cl:let ((export-symbols '(:array :arrayp
                             :array-dimensions
                             :narray-dimensions
                             :array-dimension
                             :array-element-type
                             :array-total-size
                             :array-displacement
                             :1d-storage-array
                             :aref
                             :row-major-aref
                             :array-rank
                             :make-array
                             :print-array
                             :copy-array)))
    `(uiop:define-package :dense-arrays
         (:mix :iterate :alexandria :cl :5am :trivial-types)
       ;; TODO: Do away with array-storage-vector for portability by implementing
       ;; initial-contents
       #+sbcl (:import-from :sb-ext :array-storage-vector)
       (:export ,@export-symbols)
       (:shadow ,@export-symbols)))

(in-package :dense-arrays)

(def-suite :dense-arrays)
(in-suite :dense-arrays)

(deftype int32 () `(signed-byte 32))

(defmacro define-struct-with-required-slots (name-and-options &rest slot-descriptions)
  "Like DEFSTRUCT but SLOT-DESCRIPTIONS can also have a `:required t` as an option."
  `(defstruct ,name-and-options
     ,@(loop :for desc :in (if (stringp (first slot-descriptions))
                               (rest slot-descriptions)
                               slot-descriptions)
             :collect (if (getf (cddr desc) :required)
                          `(,(first desc)
                            (cl:error ,(format nil
                                               "~S must be supplied during ~A:~A initialization"
                                               (first desc)
                                               (package-name
                                                (symbol-package
                                                 (first name-and-options)))
                                               (first name-and-options)))
                            ,@(progn (remf (cddr desc) :required)
                                     (cddr desc)))
                          desc))))

(define-struct-with-required-slots (array (:conc-name array-)
                                          (:predicate arrayp)
                                          (:constructor make-dense-array)
                                          (:copier copy-dense-array))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (displaced-to nil :required t)
  (element-type nil :required t)
  (dim          nil :required t :read-only t)
   ;; DIM is actually a read-only slot; however, in TRANSPOSE, for performance reasons,
   ;; we, first copy an existing array, and then write to this slot
  (strides      nil :required t)
  (offsets      nil :required t :type list)
  (contiguous-p nil :required t)
  (total-size   nil :required t :type int32)
  (rank         nil :required t)
  (root-array   nil :required t))

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

(declaim (ftype (function ((or list fixnum) &key
                                            (:element-type t)
                                            (:initial-element *)
                                            (:strides list)
                                            (:offsets list)
                                            (:displaced-index-offset int32)
                                            (:constructor function-designator)
                                            (:initial-contents *)
                                            (:displaced-to simple-array)
                                            (:adjustable *)
                                            (:fill-pointer *))
                          (values array &optional))
                make-array))

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
  ;; (when initial-contents-p
  ;;   (error "INITIAL_CONTENTS has not been handled yet in DENSE-ARRAY"))
  
  (let* ((dimensions (if (listp dimensions)
                         dimensions
                         (list dimensions)))
         (displaced-vector-initial-element
           (cond (constructor-p
                  (ecase element-type
                    ;; TODO: Include more types?
                    (single-float 0.0)
                    (double-float 0.0d0)
                    (t 0)))
                 (initial-element-p initial-element)
                 (initial-contents-p 0)
                 (t 0)))
         (rank (length dimensions))
         (total-size (apply #'* dimensions))
         (displaced-to (cond (displaced-to displaced-to)
                             (t (cl:make-array total-size
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
             (declare (type int32 row-major-index))
             (labels ((construct (r &optional (stride (first strides))
                                  &rest subscripts)
                        (declare (optimize speed)
                                 (type int32 r stride))
                        (if (< r 0)
                            (progn                              
                              (setf (cl:aref displaced-to row-major-index)
                                    (apply constructor subscripts))
                              (incf row-major-index stride))
                            (loop :for i :of-type int32 :below (nth r dimensions)
                                  :with 1-r :of-type int32 := (1- r)
                                  :with s :of-type int32 := (nth r strides)
                                  :do (apply #'construct 1-r
                                             s
                                             i
                                             subscripts)
                                  :finally (decf row-major-index
                                                 (the int32 (* (the int32 (nth r dimensions))
                                                               (the int32 (nth r strides)))))
                                  (incf row-major-index 1)))))
               (construct (1- rank)))))
          (initial-contents-p
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index)
                      (optimize (speed 1)))
             (labels ((set-displaced-to (elt)
                        (if (listp elt)
                            (loop :for e :in elt
                                  :do (set-displaced-to e))
                            (progn
                              (setf (cl:aref displaced-to row-major-index) elt)
                              (incf row-major-index)))))
               (set-displaced-to initial-contents)))))
    (make-dense-array :displaced-to displaced-to
                      :element-type (cl:array-element-type displaced-to)
                      :dim dimensions
                      :strides strides
                      :offsets offsets
                      :contiguous-p t
                      :total-size (apply #'* dimensions)
                      :root-array nil
                      :rank (length dimensions))))

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
           (type int32 axis-number))
  (elt (array-offsets array) axis-number))

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

(defmethod print-object ((array dense-arrays:array) stream)
  ;; (print (type-of array))
  (let ((*print-right-margin* (or *print-right-margin* 80))
        (sv     (array-displaced-to array))
        (rank   (array-rank array))
        (rank-1 (1- (array-rank array)))
        (lines  3)
        (*axis-number* 0)
        (indent 3)
        (fmt-control (cond (*array-element-print-format*
                            *array-element-print-format*)
                           ((type= 'double-float (array-element-type array))
                            "~,15,3e")
                           ((type= 'single-float (array-element-type array))
                            "~,7,2e")
                           (t "~d"))))
    ;; Do this before just to save some horizontal space
    (declare (special *axis-number*))
    (print-unreadable-object (array stream :type t :identity t)
      (when *print-array*
        ;; header
        (format stream "~A~S ~{~S~^x~}"
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
      (is (= 3 (count #\newline (level 1)))))))

(defun broadcast-array (array broadcast-dimensions)
  (unless (arrayp array)
    ;; Should probably warn if ARRAY is not an array
    (setq array (make-array 1 :initial-element array)))
  (with-slots (dim element-type strides offsets displaced-to) array
    (multiple-value-bind (strides offsets)
        (let* ((blen (length broadcast-dimensions))
               (len  (length dim))
               (dim  (append (make-list (- blen len)
                                        :initial-element 1)
                             dim))
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
         :dim broadcast-dimensions
         :element-type element-type
         :strides strides
         :offsets offsets
         :displaced-to displaced-to
         ;; TODO: Raises the question of semantics of array being contiguous
         :contiguous-p (= (first strides)
                          (/ total-size (first broadcast-dimensions)))
         :total-size total-size
         :root-array (or (array-root-array array) array)
         :rank (length broadcast-dimensions))))))

