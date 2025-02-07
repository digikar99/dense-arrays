(in-package :dense-arrays)

(in-suite :dense-arrays)

;; TODO: This should be rewritten using type-propagating functionality
(defun simple-dense-array-p (object)
  (declare (optimize speed))
  (and (typep object 'dense-array)
       (locally (declare (type dense-array object))
         (and (null (dense-array-root-array object))
              (not (eq (class-of object)
                       (class-of (array-storage object))))
              (dense-array-layout object)))))

(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
(define-array-specialization-type simple-array (and standard-dense-array
                                                    (satisfies simple-dense-array-p)))

(define-orthogonally-specializing-type
    (standard-dense-array :class standard-dense-array)
    (&optional (element-type '*) (dimensions '*) (rank '*) (layout '*)
               (simple-p '*) (metadata-name 'standard-dense-array))
  ((element-type :accessor array-element-type)
   (dimensions :accessor array-dimensions :nested t)
   (rank :accessor array-rank) (layout :accessor dense-array-layout)
   (simple-p :accessor (cl:lambda (o) (null (dense-array-root-array o))))
   (metadata-name :accessor (cl:lambda (o) (dam-name (dense-array-metadata o)))))
  :to-cl-type
  (cl:lambda (type-spec)
    (let ((type-spec (ensure-list type-spec)))
      (optima:ematch (append type-spec
                             (make-list (- 7 (length type-spec))
                                        :initial-element '*))
        ((list 'standard-dense-array element-type dimensions _ _ simple-p metadata-name)
         ;; FIXME: This needs more refinement
         (let ((metadata-object (dam-object metadata-name)))
           (if (eq t simple-p)
               (list (dam-simple-array-type metadata-object)  element-type dimensions)
               (list (dam-array-type metadata-object) element-type dimensions))))))))

(defmacro define-dense-array-types
    (type-name metadata-name array-name &optional simple-array-name)

  `(eval-when (:compile-toplevel :load-toplevel :execute)

     (setf (dam-array-type (dam-object ',metadata-name)) ',array-name)
     (setf (dam-simple-array-type (dam-object ',metadata-name)) ',simple-array-name)

     (define-type ,array-name (&optional (element-type 'cl:*) (dim/rank 'cl:*))
       `(,',type-name ,(cond ((eq 'cl:* element-type)
                              'cl:*)
                             ((cl-type-specifier-p element-type)
                              (funcall (dam-storage-element-type-upgrader
                                        (dam-object ',metadata-name))
                                       element-type))
                             (t
                              element-type))
                      ,(if (numberp dim/rank)
                           (make-list dim/rank
                                      :initial-element 'cl:*)
                           dim/rank)
                      ,(if (listp dim/rank)
                           (length dim/rank)
                           dim/rank)
                      cl:*
                      cl:*
                      ,',metadata-name))



     ,(when simple-array-name
        `(define-type ,simple-array-name (&optional (element-type 'cl:*) (dim/rank 'cl:*))
           `(,',type-name ,(cond ((eq 'cl:* element-type)
                                  'cl:*)
                                 ((cl-type-specifier-p element-type)
                                  (funcall (dam-storage-element-type-upgrader
                                            (dam-object ',metadata-name))
                                           element-type))
                                 (t
                                  element-type))
                          ,(if (numberp dim/rank)
                               (make-list dim/rank
                                          :initial-element 'cl:*)
                               dim/rank)
                          ,(if (listp dim/rank)
                               (length dim/rank)
                               dim/rank)
                          cl:*
                          t
                          ,',metadata-name)))))

(define-dense-array-types standard-dense-array standard-dense-array
  array simple-array)

(setf (assoc-value *array-class-type-alist* 'standard-dense-array
                   :test #'equal)
      (lambda (array)
        (if (null (dense-array-root-array array))
            'simple-array
            'array)))


(define-trait-implementation dense-array standard-dense-array ()

  (defun dense-array-strides (array) (standard-dense-array-strides array))
  (defun dense-array-offset (array) (standard-dense-array-offset array))
  (defun dense-array-layout (array) (standard-dense-array-layout array))
  (defun dense-array-root-array (array) (standard-dense-array-root-array array))
  (defun dense-array-metadata (array) (standard-dense-array-metadata array))

  (defun array-dimensions (array) (copy-list (standard-dense-array-dimensions array)))
  (defun array-dimension (array axis-number)
    (nth axis-number (standard-dense-array-dimensions array)))
  (defun array-rank (array) (standard-dense-array-rank array))
  (defun array-element-type (array) (standard-dense-array-element-type array))
  (defun array-total-size (array) (standard-dense-array-total-size array))
  (defun array-storage (array) (standard-dense-array-storage array)))

;; For internal usage

(defun dense-array-type-element-type (type-spec)
  (assert (subtypep type-spec 'dense-array))
  (optima:match (typexpand type-spec)
    ((list* 'specializing _ element-type _)
     element-type)
    (_
     (array-type-element-type type-spec))))

(defun dense-array-type-rank (type-spec)
  (assert (subtypep type-spec 'dense-array))
  (optima:match (typexpand type-spec)
    ((list* 'specializing _ _ _ rank _)
     rank)
    (_
     (array-type-rank type-spec))))
