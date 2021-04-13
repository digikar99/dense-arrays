(in-package :cl-cuda)

;;; FIXME: We probably shouldn't do such a global initialization
;;; None-the-less, this lets us play in REPL,
;;; albeit, sometimes, one can run into issues:
;;; https://github.com/takagi/cl-cuda/issues/99

(init-cuda)
(setq *cuda-device* (get-cuda-device 0)
      *cuda-context* (create-cuda-context *cuda-device*)
      *nvcc-options*
      (if (cl-cuda.api.context::arch-exists-p *nvcc-options*)
          *nvcc-options*
          (cl-cuda.api.context::append-arch *nvcc-options* 0)))

(in-package :dense-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :cu :cl-cuda))

(define-struct-with-required-slots (lisp-cuda-type)
  (lisp nil  :required t)
  (cuda nil  :required t)
  (size nil  :required t)
  (c    nil :required t))

(declaim (inline lisp-type cuda-type type-size c-type))
(defun lisp-type (lisp-cuda-type)
  (declare (type lisp-cuda-type lisp-cuda-type))
  (lisp-cuda-type-lisp lisp-cuda-type))
(defun cuda-type (lisp-cuda-type)
  (declare (type lisp-cuda-type lisp-cuda-type))
  (lisp-cuda-type-cuda lisp-cuda-type))
(defun type-size (lisp-cuda-type)
  (declare (type lisp-cuda-type lisp-cuda-type))
  (lisp-cuda-type-size lisp-cuda-type))
(defun c-type (lisp-cuda-type)
  (declare (type lisp-cuda-type lisp-cuda-type))
  (lisp-cuda-type-c lisp-cuda-type))

(defmethod print-object ((o lisp-cuda-type) stream)
  (format stream "<~S | ~S>" (lisp-type o) (cuda-type o)))

(defparameter *lisp-cuda-types*
  (list (make-lisp-cuda-type :lisp 'single-float
                             :cuda 'cu:float
                             :size 4
                             :c :float)
        (make-lisp-cuda-type :lisp 'double-float
                             :cuda 'cu:double
                             :size 8
                             :c :double)
        (make-lisp-cuda-type :lisp '(signed-byte 32)
                             :cuda 'cu:int
                             :size 4
                             :c :int)
        (make-lisp-cuda-type :lisp 'bit
                             :cuda 'cu:bool
                             ;; well the size is 1 bit, not 1 byte!
                             :size 1
                             :c :bool)))

(defun make-cuda-vector (size &key element-type initial-element)
  (let* ((lisp-cuda-type (find element-type *lisp-cuda-types*
                               :key #'lisp-type :test #'type=))
         (block (cu:alloc-memory-block (cuda-type lisp-cuda-type) size)))
    (loop :for i :below size
          :do (setf (cuda-memory-block-aref block i) initial-element))
    ;; TODO: A faster option; but there are better things to optimize than this
    (cu:sync-memory-block block :host-to-device)
    block))

(defun free-cuda-vector (memory-block)
  (cu:free-memory-block memory-block))

(defun upgraded-cuda-array-element-type (element-type)
  (if (lisp-cuda-type-p element-type)
      element-type
      (if-let (lisp-cuda-type (find element-type *lisp-cuda-types*
                                    :key #'lisp-type :test #'type=))
        (lisp-type lisp-cuda-type)
       ;; TODO: Some default type
        'single-float)))

(declaim (inline cuda-memory-block-aref (setf cuda-memory-block-aref)))
(defun cuda-memory-block-aref (memory-block index)
  (declare (optimize speed))
  (let ((value (cu:memory-block-aref memory-block index)))
    (case value
      ((t) 1)
      ((nil) 0)
      (t value))))
(defun (setf cuda-memory-block-aref) (new memory-block index)
  (declare (optimize speed))
  (if (eq 'cu:bool (cu:memory-block-type memory-block))
      (setf (cu:memory-block-aref memory-block index)
            (= 1 (the bit new)))
      (setf (cu:memory-block-aref memory-block index) new)))

(define-dense-array-backend-specialization :cuda)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype cuda-dense-array () `(and dense-array (satisfies ,(backend-p-fn-name :cuda))))
  (define-array-specialization-type cuda-array cuda-dense-array))

(make-backend :cuda
              :storage-accessor 'cuda-memory-block-aref
              :storage-allocator 'make-cuda-vector
              :storage-deallocator 'free-cuda-vector
              :element-type-upgrader 'upgraded-cuda-array-element-type
              ;; TODO: Would additional types help in optimization?
              :storage-type-inferrer-from-array
              (lambda (array)
                (declare (ignore array))
                'cl-cuda.api.memory::memory-block)
              :storage-type-inferrer-from-array-type
              (lambda (array)
                (declare (ignore array))
                'cl-cuda.api.memory::memory-block))

(defpolymorph array-element-type ((memory-block cl-cuda.api.memory::memory-block)) t
  (lisp-type (find (cl-cuda:memory-block-type memory-block)
                   *lisp-cuda-types*
                   :key #'cuda-type)))

