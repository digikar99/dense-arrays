(in-package :dense-arrays)

(in-suite :dense-arrays)

(defun simple-dense-array-p (object)
  (declare (optimize speed))
  (and (typep object 'dense-array)
       (locally (declare (type dense-array object))
         (and (null (dense-array-root-array object))
              (not (type= (class-of object)
                          (class-of (array-storage object))))
              (dense-array-layout object)))))

(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
(define-array-specialization-type simple-array
    (and standard-dense-array simple-dense-array))

;; For internal usage
(define-array-specialization-type %dense-array dense-array)

#+extensible-compound-types
(progn
  (defmethod %subtypep ((n1 (eql '%dense-array)) (n2 (eql 'abstract-array))
                        t1 t2 &optional env)
    (declare (ignore n1 n2 t1 env))
    (if (or (symbolp t2)
            (and (listp t2)
                 (first t2)
                 (null (cdr t2))))
        (values t t)
        (values nil nil)))

  (defmethod %subtypep ((t1 (eql 'array)) (t2 (eql 'simple-array)) type1 type2 &optional env)
    (declare (ignore t1 t2 type1 type2 env))
    (values nil t))

  (defmethod %subtypep ((t1 (eql 'simple-array)) (t2 (eql 'array)) type1 type2 &optional env)
    (declare (ignore t1 t2))
    (%subtypep 'array 'array
               `(array ,@(rest (ensure-list type1)))
               `(array ,@(rest (ensure-list type2))) env))

  (defmethod %intersect-type-p
    ((t1 (eql 'array)) (t2 (eql 'simple-array)) type1 type2 &optional env)
    (%intersect-type-p t1 t1 type1 `(array ,@(rest (ensure-list type2))) env))
  (defmethod %intersect-type-p
      ((t1 (eql 'simple-array)) (t2 (eql 'array)) type1 type2 &optional env)
    (%intersect-type-p t2 t2 `(array ,@(rest (ensure-list type1))) type2 env))

  (define-mutually-exclusive-types array cl:array)
  (define-mutually-exclusive-types array cl:simple-array)
  (define-mutually-exclusive-types simple-array cl:simple-array)
  (define-mutually-exclusive-types simple-array cl:array))
