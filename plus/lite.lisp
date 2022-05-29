(uiop:define-package :dense-arrays-plus-lite
  (:mix :dense-arrays :cl :5am)
  (:import-from
   #:dense-arrays
   #:lm
   #:size
   #:the-size
   #:default-element-type
   #:make-dense-array
   #:storage-accessor
   #:dense-array
   #:simple-dense-array
   #:array-offsets
   #:array-strides
   #:array-root-array)
  (:reexport :dense-arrays)
  (:export
   #:asarray
   #:transpose
   #:zeros
   #:ones
   #:rand
   #:eye
   #:reshape
   #:zeros-like
   #:ones-like
   #:rand-like
   #:as-cl-array
   #:macro-map-array))

(in-package :dense-arrays-plus-lite)

(def-suite :dense-arrays-plus-lite)
(in-suite :dense-arrays-plus-lite)

;; ASARRAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dimensions (array-like)
  "Consequences of ARRAY-LIKE having elements of different dimensions is undefined."
  (typecase array-like
    (string      nil)
    (sequence    (let ((len (length array-like)))
                   (cons len
                         (when (> len 0) (dimensions (elt array-like 0))))))
    (cl:array    (cl:array-dimensions array-like))
    (dense-array (array-dimensions array-like))
    (t           ())))


(defun max-type (type-1 type-2)
  (cond ((subtypep type-1 type-2)
         type-2)
        ((subtypep type-2 type-1)
         type-1)
        ((or (alexandria:type= type-1 'double-float)
             (alexandria:type= type-2 'double-float))
         'double-float)
        ((or (alexandria:type= type-1 'single-float)
             (alexandria:type= type-2 'single-float))
         'single-float)
        ;; At this point, none of the types are floats
        ;; FIXME: Operate better on impl with other float types
        ((and (subtypep type-1 '(unsigned-byte *))
              (subtypep type-2 '(signed-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-1 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        ((and (subtypep type-1 '(signed-byte *))
              (subtypep type-2 '(unsigned-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-2 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        (t
         (error "Don't know how to find MAX-TYPE of ~S and ~S" type-1 type-2))))

(defun element-type (array-like)
  "Consequences of ARRAY-LIKE having elements of different element-type is undefined."
  (typecase array-like
    (string      t)
    (sequence    (if (< 0 (length array-like))
                     (loop :for i :from 1 :below (length array-like)
                           :with max-type := (element-type (elt array-like 0))
                           :do (setq max-type
                                     (max-type max-type
                                               (element-type (elt array-like i))))
                           :finally (return max-type))
                     'null))
    (cl:array    (cl:array-element-type array-like))
    (dense-array (array-element-type (array-displaced-to array-like)))
    (t           (type-of array-like))))

(def-test dimensions ()
  (is (equalp '(3)   (dimensions '(1 2 3))))
  (is (equalp '(1 3) (dimensions '(#(1 2 3)))))
  (is (equalp '(2 3 4)
              (dimensions (list (make-array '(3 4) :element-type 'single-float)
                                (make-array '(3 4) :element-type 'single-float)))))
  (is (equalp '(2 3 4)
              (dimensions (list (cl:make-array '(3 4))
                                (cl:make-array '(3 4)))))))

(defvar *index*)
(defvar *storage*)
(defvar *storage-accessor*)

(defmethod traverse ((object t))
  (funcall (fdefinition `(setf ,*storage-accessor*))
           (trivial-coerce:coerce object (array-element-type *storage*))
            *storage* *index*)
  (incf *index*))

(defmethod traverse ((object string))
  (funcall (fdefinition `(setf ,*storage-accessor*))
           object
            *storage* *index*)
  (incf *index*))

(macrolet ((def-stub (type)
             `(defmethod traverse ((object ,type))
                (map nil #'traverse object))))
  (def-stub vector)
  (def-stub list))

(defmethod traverse ((object cl:array))
  (loop :for i :from 0 :below (cl:array-total-size object)
        :do (traverse (cl:row-major-aref object i))))



(defmethod traverse ((object dense-arrays::dense-array))
  (do-arrays ((a object))
    (traverse a)))

;; - But should the result type be array or dense-arrays::dense-array,
;;   or something else?
(defun asarray (array-like &key (type default-element-type))
  "TYPE can also be :AUTO"
  (let* ((dimensions (dimensions array-like))
         (array      (make-array dimensions
                                 :element-type (if (eq :auto type)
                                                   (element-type array-like)
                                                   type)))
         (*storage*  (array-displaced-to array))
         (*storage-accessor* (storage-accessor (class-of array)))
         (*index*    0))
    (traverse array-like)
    array))

(def-test asarray ()
  (is (array= (make-array '(2 1 3) :initial-contents '(((1 2 3))
                                                     ((1 2 3)))
                                   :element-type '(unsigned-byte 2))
              (asarray '(#2a((1 2 3)) #2a((1 2 3))) :type '(integer 0 3))))
  (is (array= (make-array '(1 3) :initial-contents '((1 2 3))
                                 :element-type '(unsigned-byte 2))
              (asarray '(#(1 2 3)) :type '(integer 0 3))))
  (is (alexandria:type= '(signed-byte 32)
                        (array-element-type
                         (let ((*array-element-type-alist*
                                 (list (cons (find-package :cl)
                                             '(signed-byte 32))))
                               (*package*
                                 (find-package :cl)))
                           (asarray '(1 2 3))))))
  (is (array= (make-array 2 :initial-contents '("hello" "goodbye"))
              (asarray '("hello" "goodbye")))))

;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose (array-like)
  (let ((array (typecase array-like
                 (dense-array array-like)
                 (t (asarray array-like)))))
    (declare (type dense-array array)
             (optimize speed))
    (make-instance (class-of array)
                   :storage (array-displaced-to array)
                   :element-type (array-element-type array)
                   :layout (array-layout array)
                   :dimensions (reverse (narray-dimensions  array))
                   :strides (reverse (array-strides  array))
                   :offsets (reverse (array-offsets  array))
                   :total-size   (array-total-size   array)
                   :rank (array-rank array)
                   :root-array (or (dense-arrays::dense-array-root-array array)
                                   array))))

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) :a 2 :b 3)"
  (if args
      (if (keywordp (car args))
          (cons nil args)
          (destructuring-bind (non-keyword-args &rest keyword-args)
              (split-at-keywords (cdr args))
            (append (list (cons (car args) non-keyword-args))
                    keyword-args)))
      '(nil)))

;; TODO: Add compiler macros to speed things up

(declaim (ftype (function * simple-dense-array)
                ones zeros rand ones-like zeros-like))

(defmacro define-splice-list-fn (name args &body body)
  (multiple-value-bind (doc body)
      (if (and (stringp (first body))
                      (rest body))
          (values (first body) (rest body))
          (values (format nil "LAMBDA-LIST: ~A" args)
                  body))
    `(progn
       (declaim (inline ,name))
       (defun ,name (&rest args)
         ,doc
         (destructuring-bind ,args (split-at-keywords args)
           ,@body))
       (declaim (notinline ,name)))))

(define-splice-list-fn zeros (shape &key (type default-element-type))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (make-array shape :element-type type
                    :initial-element (coerce 0 type)))

(define-splice-list-fn ones (shape &key (type default-element-type))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (make-array shape :element-type type
                    :initial-element (coerce 1 type)))

(define-splice-list-fn rand (shape &key (type default-element-type)
                                   (min (coerce 0 type))
                                   (max (coerce 1 type)))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let ((a     (zeros shape :type type))
        (range (- max min))
        (min   (coerce min type)))
    (declare (type simple-dense-array a))
    (dotimes (index (array-total-size a))
      (funcall #'(setf row-major-aref)
               (+ min (random range))
               a
               index))
    a))

(defun zeros-like (array-like)
  (zeros (dimensions array-like) :type (element-type array-like)))

(defun ones-like (array-like)
  (ones (dimensions array-like) :type (element-type array-like)))

(defun rand-like (array-like)
  (rand (dimensions array-like) :type (element-type array-like)))

(declaim (ftype (function * simple-dense-array) eye))
(defun eye (per-axis-size &key (rank 2) (type default-element-type))
  (declare (type size per-axis-size rank))
  (if (= 1 per-axis-size)
      (ones (make-list rank :initial-element 1)
            :type type)
      (let ((array (zeros (make-list rank :initial-element per-axis-size)
                          :type type))
            (stride (/ (1- (expt per-axis-size rank))
                       (1- per-axis-size)))
            (one (coerce 1 type)))
        (declare (type size stride)
                 (type simple-dense-array array))
        (dotimes (i per-axis-size)
          (declare (type size i))
          (funcall #'(setf row-major-aref)
                   one
                   array
                   (the-size (* i stride))))
        array)))

(define-condition incompatible-reshape-dimensions (error)
  ((array-like :initarg :array-like)
   (new-shape :initarg :new-shape))
  (:report (lambda (c s)
             (with-slots (array-like new-shape) c
               (format s "Cannot reshape~%  ~S~%into shape ~S"
                       array-like new-shape)))))

(declaim (ftype (function * dense-array) reshape))
(defun reshape (array-like new-shape &key (view nil viewp) (layout nil layoutp))
  "VIEW argument is considered only if ARRAY-LIKE is a SIMPLE-DENSE-ARRAY.
If ARRAY-LIKE is a SIMPLE-DENSE-ARRAY, it is guaranteed that when VIEW is supplied,
- :VIEW non-NIL means that no copy of ARRAY-LIKE is created
- :VIEW NIL a copy of the array *will be* created
What is not guaranteed: if ARRAY-LIKE is not a SIMPLE-DENSE-ARRAY,
then a new array is created. In the future, an attempt may be made to avoid
creating the new array and instead return a view instead. "
  (let ((simple-dense-array-p (typep array-like 'simple-dense-array))
        (array (typecase array-like
                 (simple-dense-array (cond ((and layoutp
                                                 (eq layout (array-layout array-like)))
                                            array-like)
                                           (t
                                            array-like)))
                 (dense-array (copy-array array-like :layout (if layoutp
                                                                 layout
                                                                 (array-layout array-like))))
                 (t (asarray array-like))))
        (new-shape (alexandria:ensure-list new-shape)))
    (declare (type simple-dense-array array))
    (assert (= (array-total-size array)
               (reduce #'* new-shape :initial-value 1))
            (new-shape)
            'incompatible-reshape-dimensions
            :array-like array-like
            :new-shape new-shape)
    (let* ((maybe-view-array
             (make-instance (class-of array)
                            :storage (array-displaced-to array)
                            :layout nil
                            :element-type (array-element-type array)
                            :dimensions new-shape
                            :strides
                            (dense-arrays::dimensions->strides new-shape (array-layout array))
                            :offsets (make-list (length new-shape) :initial-element 0)
                            :total-size (array-total-size array)
                            :rank (length new-shape)
                            :root-array (if simple-dense-array-p
                                            ;; In other cases, we are guaranteed
                                            ;; that the ARRAY object created here
                                            ;; will not be accessible from outside
                                            ;; FIXME: Debugger?
                                            (or (dense-arrays::dense-array-root-array
                                                 array)
                                                array)
                                            nil))))
      (cond ((and viewp simple-dense-array-p)
             (if view
                 maybe-view-array
                 (copy-array maybe-view-array :layout (if layoutp
                                                          layout
                                                          (array-layout array-like)))))
            (t maybe-view-array)))))

(def-test reshape ()
  (let ((expected-value (asarray '((1) (2) (3))))
        (actual-value (reshape (asarray '(1 2 3))
                               '(3 1))))
    (is (array= expected-value actual-value)))
  (let ((expected-value (asarray '((1) (2) (3))))
        (actual-value (reshape (asarray '(1 2 3))
                               '(3 1)
                               :view t)))
    (is (array= expected-value actual-value))
    (is-true (dense-arrays::array-view-p actual-value)))
  (let ((expected-value (asarray '((1) (2) (3))))
        (actual-value (reshape (asarray '(1 2 3))
                               '(3 1)
                               :view nil)))
    (is (array= expected-value actual-value))
    (is-false (dense-arrays::array-view-p actual-value)))
  (let ((expected-value (asarray '(1 2 3 4 5 6)))
        (actual-value (reshape (reshape (asarray '((1 2 3)
                                                   (4 5 6)))
                                        '(3 2))
                               6)))
    (is (array= expected-value actual-value))))

(defun as-cl-array (array)
  (declare (type dense-array array))
  (let ((cl-array (cl:make-array (narray-dimensions array)
                                 :element-type (array-element-type array)
                                 :initial-element (coerce 0 (array-element-type array))))
        (index    0))
    (dotimes (index (cl:array-total-size cl-array))
      (setf (cl:row-major-aref cl-array index)
            (row-major-aref array index)))
    cl-array))

;; TODO: Write a functional version of this
;; TODO: Optimize this
(defmacro macro-map-array (result-array function &rest arrays)
  (alexandria:with-gensyms (result i result-type)
    (let ((array-syms (alexandria:make-gensym-list (length arrays) "ARRAY"))
          (function   (cond ((eq 'quote (first function)) (second function))
                            ((eq 'function (first function)) (second function))
                            (t (error "Unexpected")))))
      `(let (,@(loop :for sym :in array-syms
                     :for array-expr :in arrays
                     :collect `(,sym ,array-expr)))
         (declare (type dense-array ,@array-syms))
         ;; TODO: Optimize this
         (let* ((,result (or ,result-array (zeros-like ,(first array-syms))))
                (,result-type (array-element-type ,result)))
           (dotimes (,i (array-total-size ,(first array-syms)))
             (funcall #'(setf row-major-aref)
                      (trivial-coerce:coerce
                       (,function ,@(mapcar (lm array-sym `(row-major-aref ,array-sym ,i))
                                            array-syms))
                       ,result-type)
                      ,result ,i))
           ,result)))))
