#.(cl:let ((export-symbols '(:array :arrayp
                             :array-dimensions
                             :narray-dimensions
                             :array-dimension
                             :array-element-type
                             :array-total-size
                             :array-displacement
                             :array-displaced-to
                             :array=
                             :aref
                             :row-major-aref
                             :array-rank
                             :make-array
                             :*array-element-print-format*
                             :print-array
                             :copy-array
                             :do-arrays
                             :reset-array-types-cache)))
    `(uiop:define-package :dense-arrays
         (:mix :iterate :alexandria :cl :5am :trivial-types)
       (:export ,@export-symbols)
       (:shadow ,@export-symbols)))

(in-package :dense-arrays)

(trivial-package-local-nicknames:add-package-local-nickname :cm :sandalphon.compiler-macro)
(trivial-package-local-nicknames:add-package-local-nickname :env :introspect-environment)

(def-suite :dense-arrays)
(in-suite :dense-arrays)


(defvar *use-static-vectors-alist* nil
  "An ALIST mapping package to a boolean. If the boolean corresponding to *PACKAGE* is true,
dense-arrays:make-array uses static-vectors to allocate the storage/displaced vector.
Can be overriden by both
  - binding *USE-STATIC-VECTORS*
  - providing keyword arg :STATIC to DENSE-ARRAYS:MAKE-ARRAY")

(defvar *use-static-vectors*)
(setf (documentation '*use-static-vectors* 'variable)
      "If T dense-arrays:make-array uses static-vectors to allocate the underlying
storage/displaced vector. Can be overriden by providing keyword arg :STATIC
to DENSE-ARRAYS:MAKE-ARRAY")

(define-symbol-macro use-static-vectors-p
    (if (boundp '*use-static-vectors*)
        *use-static-vectors*
        (cdr (assoc *package* *use-static-vectors-alist*))))

(defvar *use-static-vectors*)

(defmacro unless-static-vectors ((num-passes) &body body)
  `(if use-static-vectors-p
       (dotimes (i ,num-passes) (pass "Skipping for static vectors"))
       (locally ,@body)))

(deftype size () `(unsigned-byte 62))
(deftype int-index () `(signed-byte 62))

(defmacro the-size (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    size ,form))

(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    int-index ,form))

(deftype int32 () `(signed-byte 32))
(deftype uint32 () `(unsigned-byte 32))
(deftype int64 () `(signed-byte 64))
(deftype uint64 () `(unsigned-byte 64))

(defmacro define-struct-with-required-slots (name-and-options &rest slot-descriptions)
  "Like DEFSTRUCT but SLOT-DESCRIPTIONS can also have a `:required t` as an option."
  `(defstruct ,name-and-options
     ,@(loop :for desc :in (if (stringp (first slot-descriptions))
                               (rest slot-descriptions)
                               slot-descriptions)
             :collect (if (getf (cddr desc) :required)
                          `(,(first desc)
                            (cl:error ,(format nil
                                               "~S must be supplied during ~A:~A initialization"
                                               (first desc)
                                               (package-name
                                                (symbol-package
                                                 (first name-and-options)))
                                               (first name-and-options)))
                            ,@(progn (remf (cddr desc) :required)
                                     (cddr desc)))
                          desc))))

(define-struct-with-required-slots (dense-array (:conc-name array-)
                                                (:predicate arrayp)
                                                (:constructor make-dense-array)
                                                (:copier copy-dense-array))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (displaced-to nil :required t :type (cl:simple-array * 1))
  (element-type nil :required t)
  (dim          nil :required t :read-only t)
   ;; DIM is actually a read-only slot; however, in TRANSPOSE, for performance reasons,
   ;; we, first copy an existing array, and then write to this slot
  (strides      nil :required t)
  (offsets      nil :required t :type list)
  (contiguous-p nil :required t)
  (total-size   nil :required t :type size)
  (rank         nil :required t :type size)
  (root-array   nil :required t))

(defparameter *element-type->checker-fn-ht* (make-hash-table))
(defparameter *checker-fn->element-and-rank* (make-hash-table))

(defun checker-fn-sym (element-type rank)
  (when-let (inner-ht (gethash element-type *element-type->checker-fn-ht*))
    (gethash rank inner-ht)))
(defun (setf checker-fn-sym) (fn-sym element-type rank)
  (when (null (gethash element-type *element-type->checker-fn-ht*))
    (setf (gethash element-type *element-type->checker-fn-ht*) (make-hash-table)))
  (setf (gethash rank (gethash element-type *element-type->checker-fn-ht*))
        fn-sym)
  (setf (gethash fn-sym *checker-fn->element-and-rank*) (list element-type rank)))

(deftype array (&optional (element-type '* elt-supplied-p) (rank '* rankp))
  (check-type rank (or (eql *) size))
  (let* ((element-type (if elt-supplied-p
                           (introspect-environment:typexpand
                            (upgraded-array-element-type element-type))
                           element-type))
         (elt-sym (intern (concatenate 'string
                                       "%ARRAY-"
                                       (let ((*package* (find-package :cl)))
                                         (write-to-string element-type))
                                       "-P")
                          :dense-arrays))
         (rank-sym (intern (concatenate 'string
                                        "%ARRAY-"
                                        (write-to-string rank)
                                        "-P")
                           :dense-arrays)))
    ;; The above separation is required to better pass subtypep test in dense-arrays.lisp.
    (unless (checker-fn-sym element-type rank)
      ;; This portion is used by compiler macros
      ;; TODO: Determine if things are simpler without this
      (let ((fn-sym (intern (concatenate 'string
                                         "%ARRAY-"
                                         (let ((*package* (find-package :cl)))
                                           (write-to-string element-type))
                                         "-" (write-to-string rank)
                                         "-P")
                            :dense-arrays)))
        (compile fn-sym
                 `(lambda (array)
                    (and (arrayp array)
                         (type= ',element-type (array-element-type array))
                         ,(if (eq rank '*)
                              t
                              `(= ',rank (array-rank array))))))
        (setf (checker-fn-sym element-type rank) fn-sym)))
    (when (and elt-supplied-p (not (fboundp elt-sym)))
      (compile elt-sym
               `(lambda (array)
                  (declare (type dense-array array))
                  (type= ',element-type (array-element-type array)))))
    (when (and rankp (not (fboundp rank-sym)))
      (compile rank-sym
               `(lambda (array)
                  (declare (type dense-array array))
                  (= ',rank (array-rank array)))))
    (cond ((and rankp elt-supplied-p)
           (add-to-array-types-cache `(array ,element-type ,rank))
           `(and dense-array
                 (satisfies ,(checker-fn-sym element-type rank))
                 (satisfies ,elt-sym)
                 (satisfies ,rank-sym)))
          (elt-supplied-p
           (add-to-array-types-cache `(array ,element-type))
           `(and dense-array
                 (satisfies ,(checker-fn-sym element-type rank))
                 (satisfies ,elt-sym)))
          (rankp ; never invoked though
           (add-to-array-types-cache `(array * ,rank))
           `(and dense-array
                 (satisfies ,(checker-fn-sym element-type rank))
                 (satisfies ,rank-sym)))
          (t
           'dense-array))))

(define-constant +array-types-cache-doc+
  ";;; This file is automatically managed by (DEFTYPE ARRAY ...) and
;;; ENSURE-ARRAY-TYPE-DURING-LOAD. Please do not modify this file manually."
  :test #'string=)

(defvar *array-types-cache* ())

;;; FIXME: The caching system will break the day we have custom array types
;;; Perhaps, then, we could just add a check to see if the element-type
;;; is predefined.
(defun add-to-array-types-cache (array-type)
  ;; The types would be present in a form canonicalized by DEFTYPE body above.
  ;; FIXME: array-types-cache file should depend on implementation?
  (unless (member array-type *array-types-cache* :test #'equalp)
    (let ((*package* (find-package :dense-arrays)))
      (with-open-file (f (asdf:component-pathname
                          (asdf:find-component
                           (asdf:find-component
                            (asdf:find-system "dense-arrays")
                            "src")
                           "array-types-cache"))
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :append)
        (unless *array-types-cache*
          (write '(cl:in-package :dense-arrays) :stream f)
          (terpri f)
          (write-string +array-types-cache-doc+ f)
          (terpri f)
          (terpri f))
        ;; We use FORMAT instead of WRITE because on compilers like SBCL,
        ;; ENV or INTROSPECT-ENVIRONMENT translate to sbcl-specific symbols.
        (format f "(PROGN~%  (PUSH '~S *ARRAY-TYPES-CACHE*)~%  (ENV:TYPEXPAND '~S))~%"
                array-type array-type)
        (terpri f)
        (push array-type *array-types-cache*)))))

(defun reset-array-types-cache ()
  (with-open-file (f (asdf:component-pathname
                      (asdf:find-component
                       (asdf:find-component
                        (asdf:find-system "dense-arrays")
                        "src")
                       "array-types-cache"))
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede))
  (setq *array-types-cache* nil))
