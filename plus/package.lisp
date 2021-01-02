(uiop:define-package :dense-arrays-plus
  (:mix :dense-arrays-plus-lite :cl :5am)
  (:import-from
   :dense-arrays
   :array-strides
   :array-stride)
  (:reexport :dense-arrays-plus-lite)
  (:export
   :shape
   :size
   :uint8
   :int32
   :uint32))

(in-package :dense-arrays-plus)

(5am:def-suite :dense-arrays-plus)

(defun shape (array-like &optional axis)
  (if axis
      (nth axis (dense-arrays-plus-lite::dimensions array-like))
      (dense-arrays-plus-lite::dimensions array-like)))

(defun size (array-like)
  (etypecase array-like
    (array    (array-total-size array-like))
    (cl:array (cl:array-total-size array-like))
    (t        (reduce #'* (shape array-like)
                      :initial-value 1))))

(deftype uint8 () '(unsigned-byte 8))
(deftype int32 () '(signed-byte 32))
(deftype uint32 () '(unsigned-byte 32))


