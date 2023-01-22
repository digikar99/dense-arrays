(in-package :dense-arrays)

(in-suite :dense-arrays)

(defun simple-dense-array-p (object)
  (declare (optimize speed))
  (and (typep object 'dense-array)
       (locally (declare (type dense-array object))
         (and (null (dense-array-root-array object))
              (not (eq (class-of object)
                       (class-of (array-storage object))))
              (dense-array-layout object)))))

;; (deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(defmacro define-dense-array-types
    (type-name class-name array-name &optional simple-array-name)

  `(progn
     (define-array-specialization-type ,array-name ,class-name)
     ,(when simple-array-name
        `(define-array-specialization-type ,simple-array-name
             (and ,class-name
                  (satisfies simple-dense-array-p))))

     (define-orthogonally-specializing-type
         (,type-name :class ,class-name)
         (&optional (element-type 'cl:*) (dimensions 'cl:*)
                    (rank 'cl:*) (layout 'cl:*) (simple-p 'cl:*))
       ((element-type :accessor array-element-type)
        (dimensions   :accessor array-dimensions :nested t)
        (rank         :accessor array-rank)
        (layout       :accessor dense-array-layout)
        (simple-p     :accessor (cl:lambda (o) (null (dense-array-root-array o)))))

       :to-cl-type (cl:lambda (type-spec)
                     (let ((type-spec (ensure-list type-spec)))
                       (optima:ematch (append type-spec
                                              (make-list (- 6 (length type-spec))
                                                         :initial-element 'cl:*))
                         ((list class-name element-type dimensions
                                _ _ simple-p)
                          ,(if simple-array-name
                               `(if (eq t simple-p)
                                    (list ',simple-array-name element-type dimensions)
                                    (list ',array-name element-type dimensions))
                               `(list ',array-name element-type dimensions)))))))

     (define-type ,array-name (&optional (element-type 'cl:*) (dim/rank 'cl:*))
       `(,',type-name ,(cond ((eq 'cl:* element-type)
                              'cl:*)
                             ((cl-type-specifier-p element-type)
                              (funcall (storage-element-type-upgrader
                                        (find-class ',class-name))
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
                      cl:*))



     ,(when simple-array-name
        `(define-type ,simple-array-name (&optional (element-type 'cl:*) (dim/rank 'cl:*))
           `(,',type-name ,(cond ((eq 'cl:* element-type)
                                  'cl:*)
                                 ((cl-type-specifier-p element-type)
                                  (funcall (storage-element-type-upgrader
                                            (find-class ',class-name))
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
                          t)))))

(define-dense-array-types standard-dense-array
  standard-dense-array array simple-array)

(pushnew (cons 'standard-dense-array (lambda (array)
                                       (if (dense-array-root-array array)
                                           'simple-array
                                           'array)))
         *array-class-type-alist*
         :test #'equal :key #'car)

;; For internal usage

(define-dense-array-types dense-array
  dense-array %dense-array simple-dense-array)

(defun dense-array-type-element-type (type-spec)
  (assert (subtypep type-spec 'dense-array))
  (optima:match (typexpand type-spec)
    ((list 'specializing _)
     'cl:*)
    ((list* 'specializing _ element-type _)
     element-type)
    (_
     (array-type-element-type type-spec))))
