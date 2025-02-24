(cl:in-package :dense-arrays)

(defun primary-form-type (form env)
  (nth-form-type form env 0 t t))

(defun dense-array-type-class (array-type &optional env) ; backend-name
  (let ((array-type (introspect-environment:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-dense-array)
            ()
            "Expected ARRAY-TYPE to be a subtype of ABSTRACT-DENSE-ARRAY but is~%  ~S"
            array-type)
    (loop :for class :in (closer-mop:class-direct-subclasses
                          (find-class 'abstract-dense-array))
          :if (subtypep array-type (class-name class))
            :do (return-from dense-array-type-class class)
          :finally (return-from dense-array-type-class 'cl:*))))

(define-symbol-macro optim-speed (and (/= 3 (policy-quality 'debug env))
                                      (= 3 (policy-quality 'speed env))))

(define-condition slot-type-failure (compiler-macro-notes:optimization-failure-note)
  ((form :initarg :form)
   (form-type :initarg :form-type)
   (slot :initarg :slot))
  (:report (lambda (c s)
             (with-slots (form form-type slot) c
               (format s "Unable to identify the ABSTRACT-DENSE-ARRAY ~S of FORM~%  ~S~%derived to be of type ~S"
                       slot form form-type)))))

(define-condition backend-failure (slot-type-failure)
  ((slot :initform 'backend :allocation :class)))

(define-condition element-type-failure (slot-type-failure)
  ((slot :initform 'element-type :allocation :class)))

(define-condition rank-failure (slot-type-failure)
  ((slot :initform 'rank :allocation :class)))
