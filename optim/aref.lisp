(cl:in-package :dense-arrays)

(define-compiler-macro aref (&whole form array &rest subscripts &environment env)

  (flet ((optim-failure ()
           (format
            *error-output*
            "~&Unable to optimize~%  ~S~%because:~%  ~A~%but is:~%  ~S~%"
            form
            "Can only optimize when ARRAY is a symbol or a list of the form (THE TYPE FORM)"
            array))
         (insufficient-declarations (reason &rest reason-args)
           (format
            *error-output*
            "~&Unable to (fully) optimize ~S because:~%  ~A"
            form
            (apply #'format nil reason reason-args))))

    (with-speed-optimization (env form)
      (if (not (or (symbolp array)
                   (and (listp array)
                        (eq 'the (first array)))))
          (progn
            (optim-failure)
            form)
          (multiple-value-bind (exists elt-type rank)
              (if (symbolp array)
                  (compile-time-array-type array env)
                  (values t (second (second array)) (third (second array))))
            ;; (print (list exists elt-type rank))
            (unless (symbolp array) (setq array (third array)))
            ;; TODO: Check if the types are declared!
            (let*
                ((subscript-types (mapcar (lm form (cm:form-type form env)) subscripts))
                 (os              (make-gensym-list (length subscripts) "OFFSET"))
                 (ss              (make-gensym-list (length subscripts) "STRIDE"))
                 (ds              (make-gensym-list (length subscripts) "DIMENSION"))
                 (full-expansion
                   (once-only (array)
                     `(cond ((and (= (array-rank ,array) ,(length subscripts))
                                  ,@(mapcar (lm ss `(integerp ,ss)) subscripts))
                             (destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (cl:aref (the (cl:simple-array ,elt-type)
                                             (array-displaced-to ,array))
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
                             (%aref-view ,array ,@subscripts)))))
                 (optim-expansion
                   `(the ,elt-type
                         ,(once-only (array)
                            `(destructuring-lists ((size      ,os (array-offsets ,array)
                                                              :dynamic-extent nil)
                                                   (int-index ,ss (array-strides ,array)
                                                              :dynamic-extent nil)
                                                   (size      ,ds (narray-dimensions ,array)
                                                              :dynamic-extent nil))
                               (cl:aref (the (cl:simple-array ,elt-type 1)
                                             (array-displaced-to ,array))
                                        (the-size (+ ,@os
                                                     ,@(mapcar (lm ss ds sub
                                                                   `(the-size
                                                                     (* ,ss
                                                                        (normalize-index
                                                                         ,sub
                                                                         ,ds))))
                                                               ss ds subscripts)))))))))
              (return-from aref
                (cond ((null exists)
                       (insufficient-declarations "Type of ~S is not declared" array)
                       ;; No hope of optimizing
                       form)
                      ((eq '* elt-type)
                       (insufficient-declarations "ELEMENT-TYPE of ~S is not declared" array)
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
                       optim-expansion)))))))))
