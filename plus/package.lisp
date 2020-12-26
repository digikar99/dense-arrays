(uiop:define-package :dense-arrays-plus
  (:mix :dense-arrays-plus-lite :cl :5am)
  (:import-from
   :dense-arrays
   :array-strides
   :array-stride)
  (:reexport :dense-arrays-plus-lite)
  (:export
   :shape
   :uint8
   :int32
   :uint32))

(in-package :dense-arrays-plus)

(5am:def-suite :dense-arrays-plus)

(defun shape (array-like)
  (dense-arrays-plus-lite::dimensions array-like))

(deftype uint8 () '(unsigned-byte 8))
(deftype int32 () '(signed-byte 32))
(deftype uint32 () '(unsigned-byte 32))


