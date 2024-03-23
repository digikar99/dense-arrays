(uiop:define-package #:dense-arrays/py4cl2-cffi
  (:mix :dense-arrays :py4cl2-cffi :cffi :cl)
  (:import-from :py4cl2-cffi
                #:py-module-dict
                #:+npy-array-c-contiguous+
                #:+npy-array-writeable+
                #:array-element-typecode
                #:array-element-type-num-bytes
                #:cl-array-offset
                #:numpy-funcall
                #:pyforeign-funcall
                #:pytrack)
  (:import-from :alexandria
                #:type=))

(cl:in-package #:dense-arrays/py4cl2-cffi)

(defmethod pythonize ((array standard-dense-array))
  (pytrack
   (let* ((descr        (pyforeign-funcall "PyArray_Descr_from_element_type_code"
                                           :string (array-element-typecode
                                                    (array-storage array))
                                           :pointer))
          (ndarray-type (pyforeign-funcall "PyDict_GetItemString"
                                           :pointer (py-module-dict "numpy")
                                           :string "ndarray"
                                           :pointer))
          (ndims        (array-rank array)))
     (with-foreign-objects ((dims :long ndims))
       (dotimes (i ndims)
         (setf (mem-aref dims :long i) (array-dimension array i)))
       (cond ((type= t (array-element-type array))
              (let* ((numpy-array
                       (numpy-funcall "PyArray_NewFromDescr"
                                      :pointer ndarray-type
                                      :pointer descr
                                      :int ndims
                                      :pointer dims
                                      :pointer (null-pointer)
                                      :pointer (null-pointer)
                                      :int 0 ; non-zero flag indicates a fortran-style array
                                      :pointer (null-pointer)
                                      :pointer))
                     (array-data (pyforeign-funcall "PyArray_Data"
                                                    :pointer numpy-array :pointer)))
                (loop :for idx :below (array-total-size array)
                      :do (pyforeign-funcall "PyArray_SetItem"
                                             :pointer numpy-array
                                             :pointer (inc-pointer array-data (cl:* idx 8))
                                             :pointer (pythonize (row-major-aref array idx))
                                             :int))
                numpy-array))
             (t
              (let ((array (copy-array array)))
                (with-pointer-to-vector-data (array-data (array-storage array))
                  (numpy-funcall "PyArray_NewFromDescr"
                                 :pointer ndarray-type
                                 :pointer descr
                                 :int ndims
                                 :pointer dims
                                 :pointer (null-pointer)
                                 :pointer array-data
                                 :int (logior +npy-array-c-contiguous+ +npy-array-writeable+) ; flags
                                 :pointer (null-pointer)
                                 :pointer)))))))))
