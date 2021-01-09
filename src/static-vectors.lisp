(in-package :dense-arrays)

(export '(*use-static-vectors-alist*
          *use-static-vectors*)
        :dense-arrays)

(in-suite :dense-arrays)
;; TODO: No separate test because no way to check if a vector is static-vector
;; https://github.com/sionescu/static-vectors/issues/13
;; In theory, can use a dynamic-variable inside the finalizer, but
;; performance plus lack of guarantees.

(defun make-array (dimensions &rest args
                   &key (element-type t)

                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     (constructor nil constructor-p)

                     (strides nil strides-p)
                     (adjustable nil adjustable-p)
                     (fill-pointer nil fill-pointer-p)

                     (static use-static-vectors-p)

                     (displaced-to nil displaced-to-p)
                     (offsets nil offsets-p)
                     (displaced-index-offset 0 displaced-index-offset-p))
  ;; TODO: Handle adjustable
  ;; TODO: Handle fill-pointer
  ;; TODO: Sanitize displaced-to and perhaps, displaced-index-offset
  ;; TODO: Take displaced-index-offset and strides into account while calculating dimensions
  (declare ;; (optimize speed)
   (ignore args adjustable fill-pointer displaced-to-p)
   (type function-designator constructor))
  (ensure-single initial-element-p
                 initial-contents-p
                 constructor-p)
  (ensure-single offsets-p
                 displaced-index-offset-p)
  (when fill-pointer-p
    (error "FILL-POINTER has not been handled yet in DENSE-ARRAY"))
  (when adjustable-p
    (error "ADJUSTABLE has not been handled yet in DENSE-ARRAY"))

  (let* ((dimensions (if (listp dimensions)
                         dimensions
                         (list dimensions)))
         (displaced-vector-initial-element
           (cond (initial-element-p initial-element)
                 (t
                  (case element-type
                    ;; TODO: Include more types?
                    (single-float 0.0s0)
                    (double-float 0.0d0)
                    (t 0)))))
         (rank (length dimensions))
         (total-size (apply #'* dimensions))
         (displaced-to (cond (displaced-to displaced-to)
                             (t (funcall (if static
                                             'static-vectors:make-static-vector
                                             'cl:make-array)
                                 total-size
                                 :initial-element displaced-vector-initial-element
                                 :element-type element-type))))
         (offsets (if displaced-index-offset
                      (nconc (list displaced-index-offset)
                             (make-list (1- rank) :initial-element 0))
                      offsets))
         (strides (if strides-p
                      strides
                      (dimensions->strides dimensions))))
    (cond (constructor-p
           (let ((row-major-index 0))
             (declare (type size row-major-index))
             ;; To avoid repeated de-allocation of subscripts, we do this convoluted work
             ;; Uncomment the 'print' to see what is happening
             (labels ((construct (r &optional (stride (first strides))
                                  &rest subscripts)
                        (declare (type int-index r stride)
                                 (ignorable stride))
                        ;; (print r)
                        ;; (princ (list row-major-index :stride stride subscripts))
                        (if (< r 0)
                            (setf (cl:aref displaced-to row-major-index)
                                  (apply constructor subscripts))
                            (loop :for i :of-type size :below (nth r dimensions)
                                  :with 1-r :of-type int-index := (1- r)
                                  :with s :of-type size := (nth r strides)
                                  :do (apply #'construct
                                             1-r
                                             s
                                             i
                                             subscripts)
                                      (incf row-major-index s)
                                  :finally (decf row-major-index
                                                 (the size (* (the size (nth r dimensions))
                                                              (the size (nth r strides)))))))))
               (construct (1- rank)))))
          (initial-contents-p
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index)
                      (optimize (speed 1)))
             (labels ((set-displaced-to (elt)
                        (typecase elt
                          (list
                           (loop :for e :in elt
                                 :do (set-displaced-to e)))
                          (string
                           (setf (cl:aref displaced-to row-major-index) elt)
                           (incf row-major-index))
                          (cl:vector
                           (loop :for e :across elt
                                 :do (set-displaced-to e)))
                          (t
                           (setf (cl:aref displaced-to row-major-index) elt)
                           (incf row-major-index)))))
               (set-displaced-to initial-contents)))))
    (let* ((array (make-dense-array :displaced-to displaced-to
                                    :element-type (cl:array-element-type displaced-to)
                                    :dim dimensions
                                    :strides strides
                                    :offsets offsets
                                    :contiguous-p t
                                    :total-size (apply #'* dimensions)
                                    :root-array nil
                                    :rank (length dimensions))))
      ;; TODO: Abstract this out for other backends?
      (when static
        (trivial-garbage:finalize array
                                  (lambda ()
                                    (static-vectors:free-static-vector displaced-to))))
      array)))
