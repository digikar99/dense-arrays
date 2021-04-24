(cl:in-package :dense-arrays)

(defpolymorph-compiler-macro aref (dense-array &rest)
    (&whole form array &rest subscripts &environment env)
  (compiler-macro-notes:with-notes (form
                                    :name (find-polymorph 'aref '(dense-array &rest))
                                    :unwind-on-signal nil
                                    :optimization-note-condition optim-speed)
    ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
    ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
    (let* ((array-type   (primary-form-type array env))
           (backend-name (dense-array-type-backend array-type env))
           (elt-type     (array-type-element-type array-type env))
           (rank         (array-type-rank array-type env))
           (simple-p     (subtypep array-type 'simple-dense-array)))
      (declare (ignore simple-p))
      (when (eq 'cl:* backend-name)
        ;; Don't have much hope of optimization
        (signal 'backend-failure :form array :form-type array-type)
        (return-from aref form))
      (let*
          ((backend         (handler-case (find-backend backend-name)
                              (no-existing-backend (c)
                                (declare (ignore c))
                                ;; The optimization benefit would be
                                ;; totally insignificant in this case
                                (signal 'no-existing-backend/note :name backend-name)
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
                 (signal 'element-type-failure :form array :form-type array-type)
                 form)
                ((not (integerp rank))
                 (signal 'rank-failure :form array :form-type array-type)
                 full-expansion)
                ((not (= rank (length subscripts)))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Number of subscripts does not match array rank ~D"
                         :args (list rank))
                 full-expansion)
                ((not (every (lm type (subtypep type 'integer)) subscript-types))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Type of subscripts ~S were derived to be non-integers ~S"
                         :args (list subscripts subscript-types))
                 full-expansion)
                (t
                 optim-expansion)))))))



(defpolymorph-compiler-macro (setf aref) (t dense-array &rest)
    (&whole form new-value array &rest subscripts &environment env)
  (compiler-macro-notes:with-notes (form
                                    :name (find-polymorph 'aref '(dense-array &rest))
                                    :unwind-on-signal nil
                                    :optimization-note-condition optim-speed)
    ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
    ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
    (let* ((array-type   (primary-form-type array env))
           (backend-name (dense-array-type-backend array-type env))
           (elt-type     (array-type-element-type array-type env))
           (rank         (array-type-rank array-type env))
           (simple-p     (subtypep array-type 'simple-dense-array)))
      (declare (ignore simple-p))
      (when (eq 'cl:* backend-name)
        ;; Don't have much hope of optimization
        (signal 'backend-failure :form array :form-type array-type)
        (return-from aref form))
      (let*
          ((backend         (handler-case (find-backend backend-name)
                              (no-existing-backend (c)
                                (declare (ignore c))
                                ;; The optimization benefit would be
                                ;; totally insignificant in this case
                                (signal 'no-existing-backend/note :name backend-name)
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
                 (signal 'element-type-failure :form array :form-type array-type)
                 form)
                ((not (integerp rank))
                 (signal 'rank-failure :form array :form-type array-type)
                 full-expansion)
                ((not (= rank (length subscripts)))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Number of subscripts does not match array rank ~D"
                         :args (list rank))
                 full-expansion)
                ((not (every (lm type (subtypep type 'integer)) subscript-types))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Type of subscripts ~S were derived to be non-integers ~S"
                         :args (list subscripts subscript-types))
                 full-expansion)
                ((not (subtypep new-value-type elt-type env))
                 (signal 'compiler-macro-notes:note
                         :datum "Type of the new-value form~%  ~S~%was derived to be ~S not of type ~S"
                         :args (list new-value new-value-type elt-type))
                 full-expansion)
                (t
                 optim-expansion)))))))
