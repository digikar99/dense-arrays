(in-package :dense-arrays)

(in-suite :dense-arrays)

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

(defmacro destructuring-int32-lists (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(destructuring-bind ,@(first bindings)
           (declare (type int32 ,@(first (first bindings))))
         (destructuring-int32-lists ,(rest bindings) ,@body))))

;; (defun map-collect (format)
;;   "(MAP-COLLECT '(C (1 2 3) (4 5 6))) ;=> ((C 1 4) (C 2 5) (C 3 6))"
;;   (let ((lists (remove-if-not #'listp format)))
;;     (apply #'mapcar
;;            (lambda (&rest items)
;;              (loop :for obj :in format
;;                    :with idx := 0
;;                    :collect (if (listp obj)
;;                                 (progn
;;                                   (incf idx)
;;                                   (nth (1- idx) items))
;;                                 obj)))
;;            lists)))

(defmacro map-collect (format &rest list-vars)
  "(map-collect `(c ,%1) '(1 2 3)) ;=> ((C 1) (C 2) (C 3))"
  (let ((list-syms (loop :for i :from 1 :to (length list-vars)
                         :collect (intern (concatenate 'string "%" (write-to-string i))))))
    `(mapcar (lambda (,@list-syms)
               ,format)
             ,@list-vars)))

(defun expand-do-arrays-with-rank (elt-vars arrays element-types rank body)
  ;; TODO Add rank correctness checks
  (let ((num-arrays (length arrays)))
    (let ((array-vars  (make-gensym-list num-arrays "ARRAY"))
          (dimensions  (make-gensym-list rank "DIMENSION"))
          (all-strides (loop :repeat (length arrays)
                             :collect (make-gensym-list rank "STRIDE")))
          (all-offsets (loop :repeat (length arrays)
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
                                   `(progn ,@body)
                                   (nest-loop (rest dimensions)
                                              (mapcar #'rest all-strides)
                                              (mapcar #'rest all-offsets)))
                          ,@(map-collect `(incf ,%1 ,%2) is strides)
                          :finally ,@(map-collect `(decf ,%1 (+ ,%2 (the int32 (* ,d ,%3))))
                                                  is offsets strides)))))
        `(let (,@(map-collect `(,%1 ,%2) array-vars arrays))
           (let (,@(map-collect `(,%1 (array-displaced-to ,%2))
                                svs array-vars)
                 ,@(map-collect `(,%1 0) is))
             (declare (type int32 ,@is)
                      ;; (optimize speed) ; check before finalizing
                      ,@(map-collect `(type (cl:simple-array ,%1) ,%2)
                                     element-types svs))
             (symbol-macrolet (,@(map-collect `(,%1 (cl:aref ,%2 ,%3))
                                              elt-vars svs is))
               ;; TODO: Add dimension correctness checks             
               (destructuring-bind ,dimensions (narray-dimensions ,(first array-vars))
                 (declare (type int32 ,@dimensions))
                 (destructuring-int32-lists
                     (,@(map-collect `(,%1 (array-strides ,%2))
                                     all-strides array-vars)
                      ,@(map-collect `(,%1 (array-offsets ,%2))
                                     all-offsets array-vars))
                   ,(nest-loop dimensions all-strides all-offsets))))))))))

(defun expand-do-arrays-without-rank (elt-vars arrays element-types body)
  ;; TODO Add rank correctness checks
  (let ((num-arrays (length arrays)))
    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (offsets      (make-gensym-list num-arrays "OFFSETS"))
          (dimensions   (gensym "DIMENSIONS"))
          (strides      (make-gensym-list num-arrays "STRIDES"))
          (is           (make-gensym-list num-arrays "I"))
          (svs          (make-gensym-list num-arrays "SV"))
          (d            (gensym "D"))
          (ss           (make-gensym-list num-arrays "SS"))
          (os           (make-gensym-list num-arrays "OS")))
      `(let (,@(map-collect `(,%1 ,%2) array-vars arrays))
         (let (,@(map-collect `(,%1 (array-displaced-to ,%2))
                              svs array-vars)
               (,dimensions (narray-dimensions ,(first array-vars)))
               ,@(map-collect `(,%1 (array-strides ,%2))
                              strides array-vars)
               ,@(map-collect `(,%1 (array-offsets ,%2))
                              offsets array-vars)
               ,@(map-collect `(,%1 0) is))
           (declare (type int32 ,@is)
                    ;; (optimize speed) ; check before finalizing
                    ,@(map-collect `(type (cl:simple-array ,%1) ,%2)
                                   element-types svs))
           (symbol-macrolet (,@(map-collect `(,%1 (cl:aref ,%2 ,%3))
                                            elt-vars svs is))
             ;; TODO: Add dimension correctness checks                 
             (labels ((nest-loop (,dimensions ,@strides ,@offsets)
                        (let ((,d  (first ,dimensions))
                              ,@(map-collect `(,%1 (first ,%2)) ss strides)
                              ,@(map-collect `(,%1 (first ,%2)) os offsets))
                          (declare (type int32 ,@ss ,@os ,d))
                          (if (null (rest ,dimensions))
                              (loop :initially ,@(map-collect `(incf ,%1 ,%2)
                                                              is os)
                                    :repeat ,d
                                    :do ,@body
                                    ,@(map-collect `(incf ,%1 ,%2) is ss)
                                    :finally ,@(map-collect `(decf ,%1
                                                                   (+ ,%2
                                                                      (the int32 (* ,d ,%3))))
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
                                                                      (the int32 (* ,d ,%3))))
                                                            is os ss))))))
               (nest-loop ,dimensions ,@strides ,@offsets))))))))

(defmacro do-arrays (rank/bindings &body body &environment env)
  "  If the argument is an INT32, it'd be treated as the rank of the arrays. Then,
the BINDINGS are assumed to be the first element of the BODY.
  Otherwise, the first argument is treated as if they are BINDINGS.
  Each BINDING is of the form (ELT-VAR ARRAY &OPTIONAL ELEMENT-TYPE)."
  (declare (ignore env)) ; TODO: Emit compiler-notes using ENV
  (let* ((rankp    (typep rank/bindings 'int32))
         (rank     (when rankp rank/bindings))
         (bindings (if rankp (first body) rank/bindings))
         (body     (if rankp (rest body) body)))
    (multiple-value-bind (elt-vars arrays element-types)
        (let (elt-vars arrays element-types)
          (loop :for binding :in bindings
                :do (destructuring-bind (elt-var array &optional (element-type '*))
                        binding
                      ;; TODO: Handle quoted element-type
                      (push elt-var      elt-vars)
                      (push array        arrays)
                      (push element-type element-types)))
          ;; Reverse because, see the test below 
          (values (nreverse elt-vars)
                  (nreverse arrays)
                  (nreverse element-types)))
      (if rankp
          (expand-do-arrays-with-rank elt-vars arrays element-types rank body)
          (expand-do-arrays-without-rank elt-vars arrays element-types body)))))

(def-test do-arrays ()
  (is (equalp '((2 3) (1 2) (0 1) 2 1)
              (let ((a (make-array '(2 3) :constructor #'+))
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
              (let ((a (make-array '(2 3) :constructor #'+))
                    (elt))
                (do-arrays 1 ((c (progn
                                   (push 1 elt)
                                   (aref a nil 0)))
                              (d (progn
                                   (push 2 elt)
                                   (aref a nil 1))))
                  (push (list c d) elt))
                elt)))
  (is (array= (make-array '(2 3) :initial-element 1)
              (let ((a (make-array '(2 3))))
                (do-arrays ((a a))
                  (setf a 1))
                a))))
