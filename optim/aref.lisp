(cl:in-package :dense-arrays)

(define-compiler-macro array= (array1 array2 &key (test '(function equalp)) &environment env)
  ;; Turns out merely inlining functions is not sufficient to "preserve"
  ;; the type declarations; so, we use a compiler macro. See
  ;;   https://stackoverflow.com/questions/67149358/portable-type-propagation-in-common-lisp-inlined-functions-without-compiler-macr
  ;; for some details
  (with-gensyms (array1-sym array2-sym)
    `(let ((,array1-sym ,array1)
           (,array2-sym ,array2))
       (declare (type ,(primary-form-type array1 env) ,array1-sym)
                (type ,(primary-form-type array2 env) ,array2-sym))
       (and (equalp (narray-dimensions ,array1-sym)
                    (narray-dimensions ,array2-sym))
            (loop :for i :below (array-total-size ,array1-sym)
                  :always (funcall ,test
                                   (row-major-aref ,array1-sym i)
                                   (row-major-aref ,array2-sym i)))))))

(defpolymorph-compiler-macro aref (dense-array &rest)
    (&whole form array &rest subscripts &environment env)
  (compiler-macro-notes:with-notes (form env
                                    :name (find-polymorph 'aref '(dense-array &rest))
                                    :unwind-on-signal nil
                                    :optimization-note-condition optim-speed)
    ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
    ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
    (let* ((array-type   (primary-form-type array env))
           (class        (dense-array-type-class array-type env))
           (elt-type     (array-type-element-type array-type env))
           (rank         (array-type-rank array-type env))
           ;; FIXME on extensible-compound-types: This should work with simple-dense-array
           (simple-p     (subtypep array-type 'simple-array)))
      (when (eq 'cl:* class)
        ;; Don't have much hope of optimization
        (signal 'backend-failure :form array :form-type array-type)
        (return-from aref form))
      (let*
          ((storage-accessor (storage-accessor class))
           (storage-type     (funcall (storage-type-inferrer-from-array-type
                                       class)
                                      `(%dense-array ,elt-type ,rank)))
           (subscript-types (mapcar (lm form (primary-form-type form env)) subscripts))
           (os              (make-gensym-list (length subscripts) "OFFSET"))
           (ss              (make-gensym-list (length subscripts) "STRIDE"))
           (optim-expansion
             (once-only (array)
               (if simple-p
                   `(locally (declare (type dense-array ,array))
                      (destructuring-lists ((int-index ,ss (array-strides ,array)
                                                       :dynamic-extent nil))
                        (,storage-accessor
                         (the ,storage-type (array-storage ,array))
                         (the-size (+ ,@(mapcar (lm ss sub
                                                    `(the-size
                                                      (* ,ss ,sub)))
                                                ss subscripts))))))
                   `(locally (declare (type dense-array ,array))
                      (destructuring-lists ((size      ,os (array-offsets ,array)
                                                       :dynamic-extent nil)
                                            (int-index ,ss (array-strides ,array)
                                                       :dynamic-extent nil))
                        (,storage-accessor
                         (the ,storage-type (array-storage ,array))
                         (the-size (+ ,@os
                                      ,@(mapcar (lm ss sub
                                                    `(the-size
                                                      (* ,ss ,sub)))
                                                ss subscripts))))))))))
        (return-from aref
          (cond ((eq '* elt-type)
                 (signal 'element-type-failure :form array :form-type array-type)
                 optim-expansion)
                ((not (integerp rank))
                 (signal 'rank-failure :form array :form-type array-type)
                 form)
                ((not (= rank (length subscripts)))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Number of subscripts does not match array rank ~D"
                         :args (list rank))
                 form)
                ((not (every (lm type (subtypep type '(integer 0))) subscript-types))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Type of subscripts ~S were derived to be non-integers ~S"
                         :args (list subscripts subscript-types))
                 form)
                (t
                 `(the ,elt-type ,optim-expansion))))))))



(defpolymorph-compiler-macro (setf aref) (t dense-array &rest)
    (&whole form new-value array &rest subscripts &environment env)
  (compiler-macro-notes:with-notes (form env
                                    :name (find-polymorph 'aref '(dense-array &rest))
                                    :unwind-on-signal nil
                                    :optimization-note-condition optim-speed)
    ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
    ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
    (let* ((array-type (primary-form-type array env))
           (class      (dense-array-type-class array-type env))
           (elt-type   (array-type-element-type array-type env))
           (rank       (array-type-rank array-type env))
           (simple-p   (subtypep array-type 'simple-array)))
      (when (eq 'cl:* class)
        ;; Don't have much hope of optimization
        (signal 'backend-failure :form array :form-type array-type)
        (return-from aref form))
      (let*
          ((storage-accessor (storage-accessor class))
           (storage-type    (funcall (storage-type-inferrer-from-array-type
                                      class)
                                     `(%dense-array ,elt-type ,rank)))
           (subscript-types (mapcar (lm form (primary-form-type form env)) subscripts))
           (new-value-type  (primary-form-type new-value env))
           (os              (make-gensym-list (length subscripts) "OFFSET"))
           (ss              (make-gensym-list (length subscripts) "STRIDE"))
           (optim-expansion
             (once-only (array)
               (if simple-p
                   `(locally (declare (type dense-array ,array))
                      (destructuring-lists ((int-index ,ss (array-strides ,array)
                                                       :dynamic-extent nil))
                        (setf (,storage-accessor
                               (the ,storage-type (array-storage ,array))
                               (the-size (+ ,@(mapcar (lm ss sub
                                                          `(the-size
                                                            (* ,ss ,sub)))
                                                      ss subscripts))))
                              ,new-value)))
                   `(locally (declare (type dense-array ,array))
                      (destructuring-lists ((size      ,os (array-offsets ,array)
                                                       :dynamic-extent nil)
                                            (int-index ,ss (array-strides ,array)
                                                       :dynamic-extent nil))
                        (setf (,storage-accessor
                               (the ,storage-type (array-storage ,array))
                               (the-size (+ ,@os
                                            ,@(mapcar (lm ss sub
                                                          `(the-size
                                                            (* ,ss ,sub)))
                                                      ss subscripts))))
                              ,new-value)))))))

        (return-from aref
          (cond ((eq '* elt-type)
                 (signal 'element-type-failure :form array :form-type array-type)
                 optim-expansion)
                ((not (integerp rank))
                 (signal 'rank-failure :form array :form-type array-type)
                 form)
                ((not (= rank (length subscripts)))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Number of subscripts does not match array rank ~D"
                         :args (list rank))
                 form)
                ((not (every (lm type (subtypep type '(integer 0))) subscript-types))
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Type of subscripts ~S were derived to be non-integers ~S"
                         :args (list subscripts subscript-types))
                 form)
                ((not (subtypep new-value-type elt-type env))
                 (signal 'compiler-macro-notes:note
                         :datum "Type of the new-value form~%  ~S~%was derived to be ~S not of type ~S"
                         :args (list new-value new-value-type elt-type))
                 form)
                (t
                 `(the ,elt-type ,optim-expansion))))))))
