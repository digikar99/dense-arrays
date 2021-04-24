(cl:in-package :dense-arrays)

(defun dense-array-type-backend (array-type &optional env) ; backend-name
  (let ((array-type (introspect-environment:typexpand array-type env)))
    (assert (and (subtypep array-type 'abstract-array)
                 (eq 'and (first array-type)))
            ()
            "Expected ARRAY-TYPE to be of the form (AND ABSTRACT-ARRAY ...) but is~%  ~S"
            array-type)
    (if (and (listp array-type)
             (eq 'and (first array-type)))
        (loop :for clause :in (intersection-type-types array-type)
              :if (and (listp clause)
                       (eq 'satisfies (first clause)))
                :do (let* ((fn-name (symbol-name (second clause)))
                           (prefix  "DENSE-ARRAY-BACKEND-")
                           (elt-pos (search prefix fn-name))
                           (start-pos (length prefix))
                           (end-pos (search "-P" fn-name)))
                      (when (and elt-pos end-pos)
                        (return-from dense-array-type-backend
                          (let ((*read-eval* nil))
                            (read-from-string (subseq fn-name start-pos end-pos))))))
              :finally (return-from dense-array-type-backend 'cl:*))
        'cl:*)))

(define-symbol-macro optim-speed (and (/= 3 (env:policy-quality 'debug env))
                                      (= 3 (env:policy-quality 'speed env))))

(define-condition no-existing-backend/note (no-existing-backend compiler-macro-notes:note)
  ())

(define-condition slot-type-failure (compiler-macro-notes:optimization-failure-note)
  ((form :initarg :form :reader form)
   (form-type :initarg :form-type :reader form-type)
   (slot :initarg :slot :reader slot))
  (:report (lambda (c s)
             (format s "Unable to identify the ~S of FORM~%  ~S~%derived to be of type ~S"
                     (slot c) (form c) (form-type c)))))

(define-condition backend-failure (slot-type-failure)
  ((slot :initform 'backend :allocation :class)))

(define-condition element-type-failure (slot-type-failure)
  ((slot :initform 'element-type :allocation :class)))

(define-condition rank-failure (slot-type-failure)
  ((slot :initform 'rank :allocation :class)))
