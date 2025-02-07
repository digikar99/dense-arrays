(cl:in-package :dense-arrays)

(defun primary-form-type (form env)
  (nth-form-type form env 0 t t))

(defun dense-array-type-metadata (array-type &optional env) ; backend-name
  (let ((array-type (peltadot:typexpand array-type env)))
    (assert (subtypep array-type 'dense-array)
            ()
            "Expected ARRAY-TYPE to be a subtype of DENSE-ARRAY but is~%  ~S"
            array-type)
    (optima:ematch array-type
      ((list 'specializing _ _ _ _ _ _ metadata-name)
       (or (dam-object metadata-name nil)
           'cl:*)))))

(define-symbol-macro optim-speed (and (/= 3 (policy-quality 'debug env))
                                      (= 3 (policy-quality 'speed env))))

(define-condition slot-type-failure (compiler-macro-notes:optimization-failure-note)
  ((form :initarg :form)
   (form-type :initarg :form-type)
   (slot :initarg :slot))
  (:report (lambda (c s)
             (with-slots (form form-type slot) c
               (format s "Unable to identify the DENSE-ARRAY ~S of FORM~%  ~S~%derived to be of type ~S"
                       slot form form-type)))))

(define-condition backend-failure (slot-type-failure)
  ((slot :initform 'backend :allocation :class)))

(define-condition element-type-failure (slot-type-failure)
  ((slot :initform 'element-type :allocation :class)))

(define-condition rank-failure (slot-type-failure)
  ((slot :initform 'rank :allocation :class)))
