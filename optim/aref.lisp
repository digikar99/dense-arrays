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

;; TODO: Consider the per-axis information to further optimize without (SAFETY 0)

(defpolymorph-compiler-macro aref (dense-array &rest)
    (&whole form array &rest subscripts &environment env)
  (let ((original-form `(aref ,@(rest form))))
    (compiler-macro-notes:with-notes (original-form
                                      env
                                      :name (find-polymorph 'aref '(dense-array &rest))
                                      :unwind-on-signal nil
                                      :optimization-note-condition optim-speed)
      ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
      ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
      (let* ((array-type   (simplify-and-type `(and ,(primary-form-type array env) t)
                                              env))
             (class        (dense-array-type-class array-type env))
             (elt-type     (dense-array-type-element-type array-type))
             (rank         (dense-array-type-rank array-type))
             ;; FIXME on extensible-compound-types: This should work with simple-dense-array
             (simple-p     (if (member :extensible-compound-types cl:*features*)
                               (subtypep array-type 'simple-array)
                               (subtypep array-type 'simple-dense-array)))
             (safety-zero-p (zerop (policy-quality 'safety env))))
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
             (ds              (make-gensym-list (length subscripts) "DIMENSION"))
             (ss              (make-gensym-list (length subscripts) "STRIDE"))
             (optim-expansion
               (once-only (array)
                 (cond
                   (simple-p
                    `(locally (declare (type dense-array ,array))
                       (destructuring-lists ((int-index ,ss (array-strides ,array)
                                                        :dynamic-extent nil)
                                             (size      ,ds (narray-dimensions ,array)
                                                        :dynamic-extent nil))
                         ,(when (not safety-zero-p)
                            `(when (not (and ,@(mapcar (lm d s `(<= 0 ,s (1- ,d)))
                                                       ds subscripts)))
                               (error 'invalid-array-index
                                      :array ,array :index (list ,@subscripts)
                                      :suggestion
                                      "Did you mean to use DENSE-ARRAYS:AREF* ?")))
                         (,storage-accessor
                          (the ,storage-type (array-storage ,array))
                          (the-int-index (+ ,@(mapcar (lm ss sub
                                                          `(the-int-index
                                                            (* ,ss ,sub)))
                                                      ss subscripts)))))))
                   (t
                    `(locally (declare (type dense-array ,array))
                       (destructuring-lists ((size      ,ds (narray-dimensions ,array)
                                                        :dynamic-extent nil)
                                             (int-index ,ss (array-strides ,array)
                                                        :dynamic-extent nil))
                         ,(when (not safety-zero-p)
                            `(when (not (and ,@(mapcar (lm d s `(<= 0 ,s (1- ,d)))
                                                       ds subscripts)))
                               (error 'invalid-array-index
                                      :array ,array :index (list ,@subscripts)
                                      :suggestion
                                      "Did you mean to use DENSE-ARRAYS:AREF* ?")))
                         (,storage-accessor
                          (the ,storage-type (array-storage ,array))
                          (the-size (+ (the-size (array-offset ,array))
                                       ,@(mapcar (lm ss sub
                                                     `(the-int-index
                                                       (* ,ss ,sub)))
                                                 ss subscripts)))))))))))
          (return-from aref
            (cond ((eq '* elt-type)
                   (signal 'element-type-failure :form array :form-type array-type)
                   optim-expansion)
                  ((not (integerp rank))
                   (signal 'rank-failure :form array :form-type array-type)
                   form)
                  ((not (= rank (length subscripts)))
                   (signal 'compiler-macro-notes:optimization-failure-note
                           :datum "Number of subscripts (~D) does not match array rank ~D"
                           :args (list (length subscripts) rank))
                   form)
                  ((not (every (lm type (subtypep type 'integer)) subscript-types))
                   (signal 'compiler-macro-notes:optimization-failure-note
                           :datum "Type of subscripts~%  ~S~%could not be derived to be integers~%  ~S"
                           :args (list subscripts subscript-types))
                   form)
                  (t
                   ;; ELT-TYPE is supplied
                   ;; rank matches the number of subscripts
                   ;; subscripts are declared/derived to be positive integers
                   `(the ,elt-type ,optim-expansion)))))))))



(defpolymorph-compiler-macro (setf aref) (t dense-array &rest)
    (&whole form new-value array &rest subscripts &environment env)
  (let ((original-form `(funcall #'(setf aref) ,@(rest form))))
    (compiler-macro-notes:with-notes (original-form
                                      env
                                      :name (find-polymorph 'aref '(dense-array &rest))
                                      :unwind-on-signal nil
                                      :optimization-note-condition optim-speed)
      ;; The fact that we are here means the type of ARRAY is at least DENSE-ARRAY
      ;; Therefore, we ignore the second return value of PRIMARY-FORM-TYPE
      (let* ((array-type (simplify-and-type `(and ,(primary-form-type array env) t)
                                            env))
             (class      (dense-array-type-class array-type env))
             (elt-type   (dense-array-type-element-type array-type))
             (rank       (dense-array-type-rank array-type))
             (simple-p   (if (member :extensible-compound-types cl:*features*)
                             (subtypep array-type 'simple-array)
                             (subtypep array-type 'simple-dense-array))))
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
             (ds              (make-gensym-list (length subscripts) "DIMENSION"))
             (ss              (make-gensym-list (length subscripts) "STRIDE"))
             (safety-zero-p (zerop (policy-quality 'safety env)))
             (optim-expansion
               (once-only (array)
                 (cond
                   (simple-p
                    `(locally (declare (type dense-array ,array))
                       (destructuring-lists ((int-index ,ss (array-strides ,array)
                                                        :dynamic-extent nil)
                                             (size      ,ds (narray-dimensions ,array)
                                                        :dynamic-extent nil))
                         ,(when (not safety-zero-p)
                            `(when (not (and ,@(mapcar (lm d s `(<= 0 ,s (1- ,d)))
                                                       ds subscripts)))
                               (error 'invalid-array-index
                                      :array ,array :index (list ,@subscripts)
                                      :suggestion
                                      "Did you mean to use (SETF DENSE-ARRAYS:AREF*) ?")))
                         (setf (,storage-accessor
                                (the ,storage-type (array-storage ,array))
                                (the-int-index (+ ,@(mapcar (lm ss sub
                                                                `(the-int-index
                                                                  (* ,ss ,sub)))
                                                            ss subscripts))))
                               (the ,elt-type ,new-value)))))
                   (t
                    `(locally (declare (type dense-array ,array))
                       (destructuring-lists ((size      ,ds (narray-dimensions ,array)
                                                        :dynamic-extent nil)
                                             (int-index ,ss (array-strides ,array)
                                                        :dynamic-extent nil))
                         ,(when (not safety-zero-p)
                            `(when (not (and ,@(mapcar (lm d s `(<= 0 ,s (1- ,d)))
                                                       ds subscripts)))
                               (error 'invalid-array-index
                                      :array ,array :index (list ,@subscripts)
                                      :suggestion
                                      "Did you mean to use (SETF DENSE-ARRAYS:AREF*) ?")))
                         (setf (,storage-accessor
                                (the ,storage-type (array-storage ,array))
                                (the-size (+ (the-size (array-offset ,array))
                                             ,@(mapcar (lm ss sub
                                                           `(the-int-index
                                                             (* ,ss ,sub)))
                                                       ss subscripts))))
                               (the ,elt-type ,new-value)))))))))

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
                  ((not (every (lm type (subtypep type 'integer)) subscript-types))
                   (signal 'compiler-macro-notes:optimization-failure-note
                           :datum "Type of subscripts~%  ~S~%could not be derived to be integers~%  ~S"
                           :args (list subscripts subscript-types))
                   form)
                  ((not (intersect-type-p new-value-type elt-type env))
                   (signal 'compiler-macro-notes:note
                           :datum "Type of the new-value form~%  ~S~%was derived to be ~S which does not intersect type ~S"
                           :args (list new-value new-value-type elt-type))
                   form)
                  (t
                   `(the ,elt-type ,optim-expansion)))))))))
