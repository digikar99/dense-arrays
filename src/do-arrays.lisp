(in-package :dense-arrays)

(in-suite :dense-arrays)

(defmacro lm (&rest body-vars)
  `(lambda ,(butlast body-vars)
     ,@(last body-vars)))

;;; We are not using let-plus, because for something like
;;
;; (let ((array (make-array '(1000000 1))))
;;   (declare (optimize speed))
;;   (time (loop repeat 1000 do
;;     (do-arrays 2 ((elt array t))
;;       (setf elt 1)))))
;;
;;; the form was found to be about 5-10% slower if let-plus:let+ was used to expand
;;; rather than the bare destructuring-bind
;;; This is probably due to where the type declarations occur.

(defmacro destructuring-lists (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((type variables values &key (dynamic-extent t))
                           &rest bindings) bindings
        (let ((values-sym (gensym "VALUES")))
          `(let (,@(loop :for var :in variables
                         :collect `(,var 0)))
             (declare (type ,type ,@variables))
             (let ((,values-sym ,values))
               (declare ,@(if dynamic-extent `((dynamic-extent ,values-sym)) nil)
                        (ignorable ,values-sym))
               ;; Using destructuring-bind here is slower
               ,@(loop :for var :in variables
                       :for i :from 0
                       :collect `(setq ,var (nth ,i ,values-sym))))
             (destructuring-lists ,bindings ,@body))))))

(defmacro map-collect (format &rest list-vars)
  "(map-collect `(c ,%1) '(1 2 3)) ;=> ((C 1) (C 2) (C 3))"
  ;; TODO: Replace this using mapcar and lm
  (let ((list-syms (loop :for i :from 1 :to (length list-vars)
                         :collect (intern (concatenate 'string "%" (write-to-string i))))))
    `(mapcar (lambda (,@list-syms)
               ,format)
             ,@list-vars)))

(defun expand-do-arrays-with-rank (elt-vars array-vars element-types rank body)
  ;; TODO Add rank correctness checks
  (let ((num-arrays (length array-vars)))
    (let ((dimensions  (make-gensym-list rank "DIMENSION"))
          (all-strides (loop :repeat num-arrays
                             :collect (make-gensym-list rank "STRIDE")))
          (all-offsets (loop :repeat num-arrays
                             :collect (make-gensym-list rank "OFFSET")))
          (is          (make-gensym-list num-arrays "I"))
          (svs         (make-gensym-list num-arrays "SV")))
      (labels ((nest-loop (dimensions all-strides all-offsets)
                 (let ((d       (first dimensions))
                       (strides (mapcar #'first all-strides))
                       (offsets (mapcar #'first all-offsets)))
                   `(loop :initially ,@(map-collect `(incf ,%1 ,%2)
                                                    is offsets)
                          :repeat ,d
                          :do ,(if (and (null (rest dimensions))
                                        (every (compose #'null #'rest) all-strides))
                                   `(locally ,@body)
                                   (nest-loop (rest dimensions)
                                              (mapcar #'rest all-strides)
                                              (mapcar #'rest all-offsets)))
                          ,@(map-collect `(incf ,%1 ,%2) is strides)
                          :finally ,@(map-collect `(decf ,%1 (+ ,%2 (the-int-index (* ,d ,%3))))
                                                  is offsets strides)))))
        `(let (,@(map-collect `(,%1 (array-displaced-to ,%2))
                              svs array-vars)
               ,@(map-collect `(,%1 0) is))
           (declare (type int-index ,@is)
                    ;; (optimize speed) ; check before finalizing
                    ,@(map-collect `(type (cl:simple-array ,%1 1) ,%2)
                                   element-types svs))
           (symbol-macrolet (,@(map-collect `(,%1 (cl:aref ,%2 ,%3))
                                            elt-vars svs is))
             (destructuring-bind ,dimensions (narray-dimensions ,(first array-vars))
               (declare (type size ,@dimensions))
               (destructuring-lists
                   (,@(mapcar (lm strides array-var
                                  `(int-index ,strides (array-strides ,array-var)))
                              all-strides array-vars)
                    ,@(mapcar (lm offsets array-var
                                 `(size ,offsets (array-offsets ,array-var)))
                             all-offsets array-vars))
                 ,(nest-loop dimensions all-strides all-offsets)))))))))

(defun expand-do-arrays-without-rank (elt-vars array-vars element-types body)
  ;; TODO Add rank correctness checks
  (let ((num-arrays (length array-vars)))
    (let ((offsets      (make-gensym-list num-arrays "OFFSETS"))
          (dimensions   (gensym "DIMENSIONS"))
          (strides      (make-gensym-list num-arrays "STRIDES"))
          (is           (make-gensym-list num-arrays "I"))
          (svs          (make-gensym-list num-arrays "SV"))
          (d            (gensym "D"))
          (ss           (make-gensym-list num-arrays "SS"))
          (os           (make-gensym-list num-arrays "OS")))
      `(let (,@(map-collect `(,%1 (array-displaced-to ,%2))
                            svs array-vars)
             (,dimensions (narray-dimensions ,(first array-vars)))
             ,@(map-collect `(,%1 (array-strides ,%2))
                            strides array-vars)
             ,@(map-collect `(,%1 (array-offsets ,%2))
                            offsets array-vars)
             ,@(map-collect `(,%1 0) is))
         (declare (type int-index ,@is)
                  ;; (optimize speed) ; check before finalizing
                  ,@(map-collect `(type (cl:simple-array ,%1 1) ,%2)
                                 element-types svs))
         ;; TODO: A proper let form would aid debugging, but doesn't allow setf-ing
         (symbol-macrolet (,@(map-collect `(,%1 (cl:aref ,%2 ,%3))
                                          elt-vars svs is))
           (labels ((nest-loop (,dimensions ,@strides ,@offsets)
                      (let ((,d  (first ,dimensions))
                            ,@(map-collect `(,%1 (first ,%2)) ss strides)
                            ,@(map-collect `(,%1 (first ,%2)) os offsets))
                        (declare (type int-index ,@os ,@ss)
                                 (type size ,d))
                        (if (null (rest ,dimensions))
                            (loop :initially ,@(map-collect `(incf ,%1 ,%2)
                                                            is os)
                                  :repeat ,d
                                  :do (locally ,@body)
                                  ,@(map-collect `(incf ,%1 ,%2) is ss)
                                  :finally ,@(map-collect `(decf ,%1
                                                               (+ ,%2
                                                                  (the-int-index (* ,d ,%3))))
                                                          is os ss))
                            (loop :initially ,@(map-collect `(incf ,%1 ,%2)
                                                            is os)
                                  :repeat ,d
                                  :do (nest-loop (rest ,dimensions)
                                                 ,@(map-collect `(rest ,%1) strides)
                                                 ,@(map-collect `(rest ,%1) offsets))
                                  ,@(map-collect `(incf ,%1 ,%2) is ss)
                                  :finally ,@(map-collect `(decf ,%1
                                                               (+ ,%2
                                                                  (the-int-index (* ,d ,%3))))
                                                          is os ss))))))
             (nest-loop ,dimensions ,@strides ,@offsets)))))))

(defmacro do-arrays (&whole form rank/bindings &body body &environment env)
  "  If the argument is of type SIZE, it'd be treated as the rank of the arrays. Then,
the BINDINGS are assumed to be the first element of the BODY.
  Otherwise, the first argument is treated as if they are BINDINGS.
  Each BINDING is of the form (ELT-VAR ARRAY &OPTIONAL ELEMENT-TYPE).

Examples

    (let ((a (make-array '(2 3)))
          (b (make-array '(2 3))))
      (do-arrays ((c a t)
                  (d b t))
        (print (list c d))))

    (let ((a (make-array '(2 3)))
          (b (make-array '(2 3))))
      (do-arrays 2 ((c a t) ; The 2 indicates the rank of the arrays
                    (d b t))
        (print (list c d))))

Either of the two cases might be faster depending on the number of dimensions."
  (let* ((rankp    (typep rank/bindings 'size))
         (rank     (when rankp rank/bindings))
         (bindings (if rankp (first body) rank/bindings))
         (body     (if rankp (rest body) body)))
    (multiple-value-bind (elt-vars arrays element-types)
        (let (elt-vars arrays element-types)
          (loop :for binding :in bindings
                :do (destructuring-bind (elt-var array &optional (element-type '*))
                        binding
                      (when (and (= 3 (env:policy-quality 'speed env))
                                 (eq element-type '*))
                        (format
                         *error-output*
                         "~&Unable to optimize~%  ~S~%because element-type (third argument) is not provided in~%  ~S~%"
                         form
                         (list elt-var array)))
                      (push  elt-var      elt-vars)
                      (push array        arrays)
                      (push element-type element-types)))
          ;; Reverse - so same as given order - because, see the test below
          (values (nreverse elt-vars)
                  (nreverse arrays)
                  (nreverse element-types)))
      (let ((array-vars (make-gensym-list (length arrays) "ARRAY")))
        `(let (,@(mapcar (lm var arr `(,var ,arr)) array-vars arrays))
           ,(unless (zerop (env:policy-quality 'safety env))
              `(assert (every (lm a (equalp (narray-dimensions ,(first array-vars))
                                            (narray-dimensions a)))
                              (list ,@(rest array-vars)))
                       ()
                       "~&Expected arrays to have the same dimensions but they are:~%  ~{~S~^~%  ~}"
                       (mapcar (lm array-var (narray-dimensions array-var))
                               (list ,@array-vars))))
           ,(if rankp
                (expand-do-arrays-with-rank elt-vars array-vars element-types rank body)
                (expand-do-arrays-without-rank elt-vars array-vars element-types body)))))))

(def-test do-arrays ()
  (is (equalp '((2 3) (1 2) (0 1) 2 1)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32))
                    (elt))
                (do-arrays 1 ((c (progn
                                   (push 1 elt)
                                   (aref a 0)))
                              (d (progn
                                   (push 2 elt)
                                   (aref a 1))))
                  (push (list c d) elt))
                elt)))
  (is (equalp '((1 2) (0 1) 2 1)
              (let ((a (make-array '(2 3) :constructor #'+ :element-type 'int32))
                    (elt))
                (do-arrays 1 ((c (progn
                                   (push 1 elt)
                                   (aref a nil 0)))
                              (d (progn
                                   (push 2 elt)
                                   (aref a nil 1))))
                  (push (list c d) elt))
                elt)))
  (is (array= (make-array '(2 3) :initial-element 1 :element-type 'int32)
              (let ((a (make-array '(2 3) :element-type 'int32)))
                (do-arrays ((a a))
                  (setf a 1))
                a))))
