(cl:in-package :dense-arrays)

(defmacro with-speed-optimization ((env form) &body body)
  `(if (= 3 (cm:policy-quality 'speed ,env))
       (progn
         ,@body)
       ,form))

(defun compile-time-array-type (array env)  
  (let ((array-type
          (cdr (when-let (type-information
                          (assoc 'type
                                 (nth-value 2
                                            (env:variable-information array env))))
                 type-information))))
    (when (and (listp array-type)
               (eq 'satisfies (first array-type)))
      (setq array-type (second array-type)))
    (destructuring-bind (&optional elt-type rank)
        (gethash array-type *checker-fn->element-and-rank*)
      (cond ((eq 'dense-array array-type)
             (values t '* '*))
            ((null elt-type)
             (values nil nil nil))            
            (t
             (values t elt-type rank))))))
