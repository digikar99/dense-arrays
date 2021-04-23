(cl:in-package :dense-arrays)

(defpolymorph-compiler-macro aref (dense-array &rest)
    (&whole form array &rest subscripts &environment env)
  (with-speed-optimization (env form)
    (if (not (or (symbolp array)
                 (and (listp array)
                      (eq 'the (first array)))))
        (progn
          (optim-failure)
          form)
        (multiple-value-bind (backend-name elt-type rank)
            (compile-time-array-type array env)
          (unless (symbolp array) (setq array (third array)))
          ;; TODO: Check if the types are declared!
          (let*
              ((backend         (handler-case (find-backend backend-name)
                                  (no-existing-backend (c)
                                    (declare (ignore c))
                                    ;; The optimization benefit would be
                                    ;; totally insignificant in this case
                                    (unknown-backend backend-name)
                                    (return-from aref form))))
               (storage-accessor (backend-storage-accessor backend))
               (storage-type    (funcall (backend-storage-type-inferrer-from-array-type
                                          backend)
                                         `(%dense-array ,elt-type ,rank)))
               (subscript-types (mapcar (lm form (primary-form-type form env)) subscripts))
               (os              (make-gensym-list (length subscripts) "OFFSET"))
               (ss              (make-gensym-list (length subscripts) "STRIDE"))
               (ds              (make-gensym-list (length subscripts) "DIMENSION"))
               (full-expansion
                 (once-only (array)
                   `(locally (declare (type dense-array ,array))
                      (cond ((and (= (array-rank ,array) ,(length subscripts))
                                  ,@(mapcar (lm ss `(integerp ,ss)) subscripts))
                             (destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (,storage-accessor
                                (the ,storage-type (array-storage ,array))
                                (the size (+ ,@os
                                             ,@(mapcar (lm ss ds sub
                                                           `(the size
                                                                 (* ,ss
                                                                    (normalize-index
                                                                     ,sub
                                                                     ,ds))))
                                                       ss ds subscripts))))))
                            ((or ,@(mapcar (lm ss `(cl:arrayp ,ss)) subscripts)
                                 ,@(mapcar (lm ss `(arrayp ,ss)) subscripts))
                             (%aref ,array ,@subscripts))
                            (t
                             (%aref-view ,array ,@subscripts))))))
               (optim-expansion
                 `(the ,elt-type
                       ,(once-only (array)
                          `(locally (declare (type dense-array ,array))
                             (destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (,storage-accessor
                                (the ,storage-type (array-storage ,array))
                                (the-size (+ ,@os
                                             ,@(mapcar (lm ss ds sub
                                                           `(the-size
                                                             (* ,ss
                                                                (normalize-index
                                                                 ,sub
                                                                 ,ds))))
                                                       ss ds subscripts))))))))))
            (return-from aref
              (cond ((eq '* elt-type)
                     (insufficient-declarations "ELEMENT-TYPE of ~S is not declared"
                                                array)
                     form)
                    ((not (integerp rank))
                     (insufficient-declarations
                      "Rank of array ~S is not declared" array)
                     full-expansion)
                    ((not (= rank (length subscripts)))
                     (insufficient-declarations
                      "Number of subscripts does not match array rank ~D" rank)
                     full-expansion)
                    ((not (every (lm type (subtypep type 'integer)) subscript-types))
                     (insufficient-declarations
                      "Type of subscripts ~S were derived to be non-integers ~S"
                      subscripts
                      subscript-types)
                     full-expansion)
                    (t
                     optim-expansion))))))))



(defpolymorph-compiler-macro (setf aref) (t dense-array &rest)
    (&whole form new-value array &rest subscripts &environment env)

  (with-speed-optimization (env form)
    (if (not (or (symbolp array)
                 (and (listp array)
                      (eq 'the (first array)))))
        (progn
          (optim-failure)
          form)
        (multiple-value-bind (backend-name elt-type rank)
            (compile-time-array-type array env)
          (unless (symbolp array) (setq array (third array)))
          ;; TODO: Check if the types are declared!
          (let*
              ((backend         (handler-case (find-backend backend-name)
                                  (no-existing-backend (c)
                                    (declare (ignore c))
                                    ;; The optimization benefit would be
                                    ;; totally insignificant in this case
                                    (unknown-backend backend-name)
                                    (return-from aref form))))
               (storage-accessor (backend-storage-accessor backend))
               (storage-type    (funcall (backend-storage-type-inferrer-from-array-type
                                          backend)
                                         `(%dense-array ,elt-type ,rank)))
               (subscript-types (mapcar (lm form (primary-form-type form env)) subscripts))
               (new-value-type  (primary-form-type new-value env))
               (os              (make-gensym-list (length subscripts) "OFFSET"))
               (ss              (make-gensym-list (length subscripts) "STRIDE"))
               (ds              (make-gensym-list (length subscripts) "DIMENSION"))
               (full-expansion
                 (once-only (array new-value)
                   `(locally (declare (type dense-array ,array))
                      (cond ((and (= (array-rank ,array) ,(length subscripts))
                                  ,@(mapcar (lm ss `(integerp ,ss)) subscripts))
                             (destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (setf (,storage-accessor
                                      (the ,storage-type (array-storage ,array))
                                      (the size (+ ,@os
                                                   ,@(mapcar (lm ss ds sub
                                                                 `(the size
                                                                       (* ,ss
                                                                          (normalize-index
                                                                           ,sub
                                                                           ,ds))))
                                                             ss ds subscripts))))
                                     ,new-value)))
                            ((or ,@(mapcar (lm ss `(cl:arrayp ,ss)) subscripts)
                                 ,@(mapcar (lm ss `(arrayp ,ss)) subscripts))
                             (%aref ,array ,@subscripts))
                            (t
                             (%aref-view ,array ,@subscripts))))))
               (optim-expansion
                 `(the ,elt-type
                       ,(once-only (array)
                          `(locally (declare (type dense-array ,array))
                             (destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (setf (,storage-accessor
                                      (the ,storage-type (array-storage ,array))
                                      (the-size (+ ,@os
                                                   ,@(mapcar (lm ss ds sub
                                                                 `(the-size
                                                                   (* ,ss
                                                                      (normalize-index
                                                                       ,sub
                                                                       ,ds))))
                                                             ss ds subscripts))))
                                     ,new-value)))))))
            (return-from aref
              (cond ((eq '* elt-type)
                     (insufficient-declarations "ELEMENT-TYPE of ~S is not declared"
                                                array)
                     form)
                    ((not (integerp rank))
                     (insufficient-declarations
                      "Rank of array ~S is not declared" array)
                     full-expansion)
                    ((not (= rank (length subscripts)))
                     (insufficient-declarations
                      "Number of subscripts does not match array rank ~D" rank)
                     full-expansion)
                    ((not (every (lm type (subtypep type 'integer)) subscript-types))
                     (insufficient-declarations
                      "Type of subscripts ~S were derived to be non-integers ~S"
                      subscripts
                      subscript-types)
                     full-expansion)
                    ((not (subtypep new-value-type elt-type))
                     (insufficient-declarations
                      "Type of the new-value form ~S was derived to be ~S not of type ~S"
                      new-value new-value-type elt-type)
                     full-expansion)
                    (t
                     optim-expansion))))))))
