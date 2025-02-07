(uiop:define-package :dense-arrays-plus-lite
  (:mix :dense-arrays :peltadot :5am :alexandria)
  (:use)
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
   #:array-offset
   #:array-strides
   #:array-root-array
   #:dense-array-metadata
   #:dam-dense-array-constructor)
  (:import-from
   #:peltadot-traits-library
   #:array-like
   #:dimensions-and-strides
   #:dimensions
   #:element-type
   #:row-major-iterator)
  (:reexport :dense-arrays)
  (:export
   #:asarray
   #:transpose
   #:zeros
   #:ones
   #:rand
   #:full
   #:eye
   #:reshape
   #:zeros-like
   #:ones-like
   #:rand-like
   #:full-like
   #:as-cl-array
   #:macro-map-array))

(in-package :dense-arrays-plus-lite)

(def-suite :dense-arrays-plus-lite)
(in-suite :dense-arrays-plus-lite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname
   :coerce :peltadot/coerce))

(define-trait-implementation array-like dense-array ()
  (defun dimensions-and-strides (array)
    (values (array-dimensions array)
            (array-strides array)))
  (defun element-type (array)
    (array-element-type array))
  (defun row-major-iterator (array)
    (let ((total-size-1 (1- (array-total-size array)))
          (row-major-index -1))
      (lambda ()
        (incf row-major-index)
        (values (row-major-aref array row-major-index)
                (not (cl:= total-size-1 row-major-index)))))))

;; - But should the result type be array or dense-arrays::dense-array,
;;   or something else?
(defun asarray (array-like &key (out nil outp)
                             (type default-element-type typep)
                             (layout *array-layout*))
  "TYPE can also be :AUTO"
  (let* ((dimensions (dimensions-and-strides array-like)))
    (when outp
      (check-type out dense-array)
      (assert (equalp dimensions (narray-dimensions out)) (out))
      (if typep
          (assert (type= type (array-element-type out)) (out))
          (setq type (array-element-type out))))
    (let* ((array      (or out
                           (make-array dimensions
                                       :element-type (if (eq :auto type)
                                                         (element-type array-like)
                                                         type)
                                       :layout layout)))
           (type     (array-element-type array))
           (iterator (row-major-iterator array-like)))
      (if (functionp iterator)
          (do-arrays ((x array))
            (setf x (coerce:coerce (funcall iterator) type)))
          (setf (aref array) (coerce:coerce iterator type)))
      array)))

(def-test asarray ()
  (is (array= (make-array '(2 1 3) :initial-contents '(((1 2 3))
                                                       ((1 2 3)))
                                   :element-type '(unsigned-byte 2))
              (asarray '(#2a((1 2 3)) #2a((1 2 3))) :type '(integer 0 3))))
  (is (array= (make-array '(2 1 3) :initial-contents '(((1 2 3))
                                                       ((1 2 3)))
                                   :element-type '(unsigned-byte 2))
              (asarray '(#2a((1 2 3)) #2a((1 2 3))) :type '(integer 0 3)
                                                    :layout :column-major)))
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
              (asarray '("hello" "goodbye"))))
  (is (array= (asarray '((1 2 3) (4 5 6)) :layout :row-major)
              (asarray '((1 2 3) (4 5 6)) :layout :column-major))))

;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose (array-like &key axes)
  (let* ((array (typecase array-like
                  (dense-array array-like)
                  (t (asarray array-like))))
         (metadata (dense-array-metadata (the dense-array array))))
    (declare (type dense-array array)
             (optimize speed))
    (multiple-value-bind (dimensions strides)
        (if axes
            (loop :for i :of-type size :below (array-rank array)
                  :for axis :in axes
                  :collect (array-dimension array axis) :into dimensions
                  :collect (array-stride array axis) :into strides
                  :finally (return (values dimensions strides)))
            (values (reverse (narray-dimensions array))
                    (reverse (array-strides    array))))
      (funcall (dam-dense-array-constructor metadata)
               :storage (array-displaced-to array)
               :element-type (array-element-type array)
               :layout (array-layout array)
               :dimensions dimensions
               :strides strides
               :offset (array-offset array)
               :total-size (array-total-size array)
               :rank (array-rank array)
               :root-array (or (dense-arrays::dense-array-root-array array)
                               array)
               :metadata metadata))))

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
       (declaim (notinline ,name))

       (defmethod custom-form-type ((op (eql ',name)) args env)
         (let* ((type-position (position :type args))
                (elt-type-form (if type-position
                                   (nth (1+ type-position) args)
                                   (return-from custom-form-type 'simple-array)))
                (elt-type (if (constantp elt-type-form env)
                              (introspect-environment:constant-form-value elt-type-form env)
                              (return-from custom-form-type 'simple-array)))
                (dim-form/s (subseq args 0 type-position))
                (rank (cond ((< 1 (length dim-form/s))
                             (length dim-form/s))
                            (t
                             (let ((dim-form-type (form-type (first dim-form/s) env)))
                               (if (type= t dim-form-type)
                                   'cl:*
                                   (multiple-value-bind (subtypep knownp)
                                       (subtypep dim-form-type 'cons env)
                                     (if knownp
                                         (if subtypep 'cl:* 1)
                                         'cl:*))))))))
           `(simple-array ,elt-type ,rank))))))

(define-splice-list-fn zeros (shape &key (type default-element-type) (layout *array-layout*))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (make-array shape :element-type type
                    :initial-element (coerce 0 type)
                    :layout layout))

(define-splice-list-fn ones (shape &key (type default-element-type) (layout *array-layout*))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (make-array shape :element-type type
                    :initial-element (coerce 1 type)
                    :layout layout))

(define-splice-list-fn rand (shape &key (type default-element-type)
                                   (layout *array-layout*)
                                   (min (coerce 0 type))
                                   (max (coerce 1 type)))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let ((a     (zeros shape :type type :layout layout))
        (range (- max min))
        (min   (coerce min type)))
    (do-arrays ((a-elt a))
      (setf a-elt (+ min (random range))))
    a))

(define-splice-list-fn full (shape &key (type default-element-type)
                                   (layout *array-layout*)
                                   value)
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (make-array shape :element-type type
                    :initial-element (coerce value type)
                    :layout layout))

;;; Doesn't result in any boost at all :(!
;; (define-compiler-macro rand (&whole form &rest args &environment env)
;;   (declare (optimize debug))
;;   ;; Escape if there are no keywords
;;   (unless (some #'keywordp args) (return-from rand form))
;;   (destructuring-bind (shape &key (type nil typep)
;;                                (layout *array-layout*)
;;                                (min nil minp)
;;                                (max nil maxp))
;;       (split-at-keywords args)
;;     (let* ((type (if (and typep
;;                           (constantp type env))
;;                      (introspect-environment:constant-form-value type env)
;;                      (return-from rand form))))
;;       (alexandria:with-gensyms (shape-var min-var max-var a sv range size i)
;;         (print
;;          `(let ((,shape-var (list ,@shape))
;;                 (,min-var ,(if minp min `(coerce 0 ',type)))
;;                 (,max-var ,(if maxp max `(coerce 1 ',type))))
;;             (declare (type ,type ,min-var ,max-var)
;;                      (type list ,shape-var))
;;             (when (listp (first ,shape-var))
;;               (assert (null (rest ,shape-var)))
;;               (setq ,shape-var (first ,shape-var)))
;;             (let* ((,a       (zeros ,shape-var :type ',type :layout ,layout))
;;                    (,range   (- ,max-var ,min-var))
;;                    (,sv      (array-storage (cl:the (simple-array ,type) ,a)))
;;                    (,size    (array-total-size (cl:the (simple-array ,type) ,a))))
;;               (declare (type (cl:simple-array ,type 1) ,sv)
;;                        (type ,type ,range)
;;                        (type size ,size))
;;               (dotimes (,i ,size)
;;                 (funcall #'(setf row-major-aref)
;;                          (cl:the ,type (+ ,min-var
;;                                           (cl:the ,type (random ,range))))
;;                          (cl:the (cl:simple-array ,type 1) ,sv)
;;                          ,i))
;;               ,a)))))))

(defun zeros-like (array-like)
  (zeros (dimensions array-like) :type (element-type array-like)))

(defun ones-like (array-like)
  (ones (dimensions array-like) :type (element-type array-like)))

(defun rand-like (array-like)
  (rand (dimensions array-like) :type (element-type array-like)))

(defun full-like (array-like value)
  (full (dimensions array-like) :value value :type (element-type array-like)))

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
  (let* ((simple-dense-array-p (typep array-like 'simple-dense-array))
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
         (new-shape (alexandria:ensure-list new-shape))
         (metadata (dense-array-metadata array)))
    (declare (type simple-dense-array array))
    (assert (= (array-total-size array)
               (reduce #'* new-shape :initial-value 1))
            (new-shape)
            'incompatible-reshape-dimensions
            :array-like array-like
            :new-shape new-shape)
    (let* ((maybe-view-array
             (funcall (dam-dense-array-constructor metadata)
                      :storage (array-displaced-to array)
                      :layout nil
                      :element-type (array-element-type array)
                      :dimensions new-shape
                      :strides
                      (dense-arrays::dimensions->strides new-shape (array-layout array))
                      :offset 0
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
                                      nil)
                      :metadata metadata)))
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
                                                   (4 5 6))
                                                 :layout :row-major)
                                        '(3 2))
                               6
                               :layout :row-major)))
    (is (array= expected-value actual-value)))
  (let ((expected-value (asarray '(1 4 2 5 3 6)))
        (actual-value (reshape (reshape (asarray '((1 2 3)
                                                   (4 5 6))
                                                 :layout :column-major)
                                        '(3 2))
                               6
                               :layout :column-major)))
    (is (array= expected-value actual-value))))

(defun as-cl-array (array)
  (declare (type dense-array array))
  (let ((cl-array (cl:make-array (narray-dimensions array)
                                 :element-type (array-element-type array)
                                 :initial-element (coerce 0 (array-element-type array)))))
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
                            ((eq 'cl:function (first function)) (second function))
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
                      (coerce:coerce
                       (,function ,@(mapcar (lm array-sym `(row-major-aref ,array-sym ,i))
                                            array-syms))
                       ,result-type)
                      ,result ,i))
           ,result)))))
