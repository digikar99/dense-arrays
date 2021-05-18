(in-package :dense-arrays)

(in-suite :dense-arrays)

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

(defun simple-dense-array-p (object)
  (and (typep object 'dense-array)
       (or (null (dense-array-root-array object))
           (and (not (type= (class-of object)
                            (class-of (array-storage object))))
                (loop :for o :of-type size :in (array-offsets object)
                      :always (zerop o))
                (let ((total-size (array-total-size object)))
                  (loop :for s :in (array-strides object)
                        :for d :in (narray-dimensions object)
                        :always (= s (/ total-size d))
                        :do (setq total-size (floor total-size d))))))))

(deftype simple-dense-array   () `(and dense-array (satisfies simple-dense-array-p)))

(define-array-specialization-type array standard-dense-array)
;;; TODO: Put simple-array type to use
(define-array-specialization-type simple-array (and standard-dense-array
                                                    simple-dense-array))

;; For internal usage
(define-array-specialization-type %dense-array dense-array)
