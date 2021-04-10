(uiop:define-package :dense-arrays-plus-lite
  (:mix :dense-arrays :cl :5am)
  (:import-from
   :dense-arrays
   :make-dense-array
   :dense-array-backend
   :backend-storage-accessor
   :dense-array
   :array-offsets
   :array-strides
   :array-root-array)
  (:reexport :dense-arrays)
  (:export
   :asarray
   :*element-type-alist*
   :transpose
   :zeros
   :ones
   :rand
   :zeros-like
   :ones-like
   :rand-like
   :as-cl-array
   :macro-map-array))

(in-package :dense-arrays-plus-lite)

(def-suite :dense-arrays-plus-lite)
(in-suite :dense-arrays-plus-lite)

(defvar *element-type-alist* nil
  "An ALIST mapping package to the default element-type used in that package.
See the definition of ASARRAY for an example of usage.")

(define-symbol-macro package-local-element-type
    (cdr (assoc *package* *element-type-alist*)))

(define-symbol-macro default-element-type
  (or package-local-element-type 'double-float))

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

(defun element-type (array-like)
  "Consequences of ARRAY-LIKE having elements of different element-type is undefined."
  (typecase array-like
    (string      t)
    (sequence    (if (> (length array-like) 0)
                     (element-type (elt array-like 0))
                     'null))
    (cl:array    (cl:array-element-type array-like))
    (dense-array (array-element-type (array-displaced-to array-like)))
    (t           t)))

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
(defun asarray (array-like &optional
                             (element-type (or package-local-element-type
                                               :auto)))
  (let* ((dimensions (dimensions array-like))
         (array      (make-array dimensions
                                 :element-type (if (eq :auto element-type)
                                                   (element-type array-like)
                                                   element-type)))
         (*storage*  (array-displaced-to array))
         (*storage-accessor* (backend-storage-accessor
                              (find-backend (dense-array-backend array))))
         (*index*    0))
    (traverse array-like)
    array))

(def-test asarray ()
  (is (array= (make-array '(2 1 3) :initial-contents '(((1 2 3))
                                                     ((1 2 3)))
                                   :element-type '(unsigned-byte 2))
              (asarray '(#2a((1 2 3)) #2a((1 2 3))) '(integer 0 3))))
  (is (array= (make-array '(1 3) :initial-contents '((1 2 3))
                                 :element-type '(unsigned-byte 2))
              (asarray '(#(1 2 3)) '(integer 0 3))))
  (is (alexandria:type= '(signed-byte 32)
                        (array-element-type
                         (let ((*element-type-alist* (list (cons (find-package :cl)
                                                                 '(signed-byte 32))))
                               (*package*            (find-package :cl)))
                           (asarray '(1 2 3))))))
  (is (array= (make-array 2 :initial-contents '("hello" "goodbye"))
              (asarray '("hello" "goodbye")))))

;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose (array)
  (declare (optimize speed)
           (type dense-array array))
  (make-dense-array :displaced-to (array-displaced-to array)
                    :storage (array-displaced-to array)
                    :element-type (array-element-type array)
                    :dimensions (reverse (narray-dimensions  array))
                    :strides (reverse (array-strides  array))
                    :offsets (reverse (array-offsets  array))
                    :contiguous-p nil
                    :total-size   (array-total-size   array)
                    :rank (array-rank array)
                    :backend (dense-array-backend array)
                    :root-array (or (dense-arrays::dense-array-root-array array)
                                    array)))

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) (:a 2 :b 3))"
  (if args
      (if (keywordp (car args))
          (cons nil args)
          (destructuring-bind (non-keyword-args &rest keyword-args)
              (split-at-keywords (cdr args))
            (append (list (cons (car args) non-keyword-args))
                    keyword-args)))
      '(nil)))

;; TODO: Add compiler macros to speed things up

(defmacro define-splice-list-fn (name args &body body)
  `(defun ,name (&rest args)
     ,(format nil "LAMBDA-LIST: ~A" args)
     (destructuring-bind ,args (split-at-keywords args)
       ,@body)))

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

(define-splice-list-fn rand (shape &key (type default-element-type))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let ((a   (zeros shape :type type))
        (lim (random (alexandria:switch (type :test #'alexandria:type=)
                       ('single-float 1.0e0)
                       ('double-float 1.0d0)
                       ('short-float  1.0s0)
                       ('long-float   1.0l0)
                       (t            (random 1.0))))))
    (do-arrays ((a-elt a))
      (setf a-elt (random lim)))
    a))

(defun zeros-like (array-like)
  (zeros (dimensions array-like) :type (element-type array-like)))

(defun ones-like (array-like)
  (ones (dimensions array-like) :type (element-type array-like)))

(defun rand-like (array-like)
  (rand (dimensions array-like) :type (element-type array-like)))

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
(defmacro macro-map-array (function &rest arrays)
  (alexandria:with-gensyms (first r result)
    (let ((array-syms (alexandria:make-gensym-list (length arrays) "ARRAY"))
          (function   (cond ((eq 'quote (first function)) (second function))
                            ((eq 'function (first function)) (second function))
                            (t (error "Unexpected")))))
      `(let ((,first ,(first arrays)))
         (let ((,result (zeros-like ,first)))
           (do-arrays ((,r ,result)
                       (,(first array-syms) ,first)
                       ,@(loop :for sym :in (rest array-syms)
                               :for array-expr :in (rest arrays)
                               :collect `(,sym ,array-expr)))
             (setf ,r (,function ,@array-syms)))
           ,result)))))
