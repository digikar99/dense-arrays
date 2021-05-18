# dense-arrays

[Last README update: 18th May 2021]

This system provides

- a metaclass `dense-array-class`
- a class `dense-array` with
  - multidimensional strides and offsets: CL arrays can only have a single offset
  - customizable behavior, especially storage slot: depending on the exact class of `dense-array`, the storage object and associated meta-information could correspond to usual `(cl:simple-array element-type 1)` or [static-vectors](https://github.com/sionescu/static-vectors) or even [cl-cuda](https://github.com/takagi/cl-cuda)!
- [a rich aref](#basic-demonstration)
- a nicer default print-object that respects `*print-array* *print-length* *print-level* *print-lines*` and is customizable via `*array-element-print-format*`; this could be improved and integrated further with builtins once someone wraps their head around [The Lisp Pretty Printer](http://www.lispworks.com/documentation/lw51/CLHS/Body/22_b.htm).
- a `unupgraded-array` and `simple-unupgraded-array` types that use `(cl:simple-array * 1)` for storage but do not upgrade the types; this can be helpful for better type checking

The multidimensional strides and offsets enable copy-free slicing and broadcasting.


```lisp
CL-USER> (let ((a (make-array '(1000 1000))))
           (time (loop for i below 1000 do (select:select a t i))))
Evaluation took:
  0.159 seconds of real time
  0.159541 seconds of total run time (0.159541 user, 0.000000 system)
  100.63% CPU
  42 lambdas converted
  352,387,000 processor cycles
  10,863,248 bytes consed

NIL
CL-USER> (let ((a (dense-arrays:make-array '(1000 1000))))
           (time (loop for i below 1000 do (dense-arrays:aref a nil i))))
Evaluation took:
  0.000 seconds of real time
  0.000910 seconds of total run time (0.000878 user, 0.000032 system)
  100.00% CPU
  2,001,298 processor cycles
  261,904 bytes consed

NIL
```

**Limitations:**

- adjustable-arrays are not handled yet
- `aref` can be 5-6 times slower than `cl:aref` even after optimization; the work-around for this is `do-arrays` which can be up to 25% slower. See [perf.org](./perf.org) for example optimizations.
- cannot be a drop-in replacement for built-in arrays because `cl:array` is both a class and a specializing type-specifier; IIUC, non-builtins can only either be one of class or specializing type-specifier.
- `(setf aref)` and `(setf row-major-aref)` may need to be used using `(funcall #'(setf aref) ...)` since some implementations like SBCL "lose" the type information from the environment in an attempt to use `once-only` 


### Included Systems

- `dense-arrays`: the super bare essentials
- `dense-arrays-plus-lite`: some utilities
- `dense-arrays+static-vectors`: provides and exports a `static-array` type that is essentially a wrapper around [static-vectors](https://github.com/sionescu/static-vectors)
- `dense-arrays+cuda`: provides and export an array using `cl-cuda`
- `dense-arrays-plus`: more utilities as well as static-vectors

Minimalists would want to stick to the first four. The last one also introduces

- `shape` as an alias for `array-dimensions`
- `int32 uint32 uint8` types as aliases for their common lisp counterparts
- integration with [py4cl2](#py4cl2): simply set `py4cl2:*array-type*` to `:dense-arrays` when you want to use py4cl2 with dense-arrays.
- and perhaps more things!


# Basic Demonstration

```lisp
CL-USER> (uiop:define-package :dense-arrays-demo
           (:mix :dense-arrays :cl))
#<PACKAGE "DENSE-ARRAYS-DEMO">
CL-USER> (in-package :dense-arrays-demo)
#<PACKAGE "DENSE-ARRAYS-DEMO">
CL-USER> (setq *print-length* 10) ; also intends to respect (*print-level* *print-lines* *print-array*)
10
```

**A cleaner look**

```lisp
DENSE-ARRAYS-DEMO> (make-array '(2 3 4) :constructor #'+)
#<STANDARD-DENSE-ARRAY 2x3x4 T
   ((0 1 2 3)
    (1 2 3 4)
    (2 3 4 5))
   ((1 2 3 4)
    (2 3 4 5)
    (3 4 5 6))
 {102F298243}>
DENSE-ARRAYS-DEMO> (aref * 1 '(0 :step 2))
#<STANDARD-DENSE-ARRAY (VIEW) 2x4 T
   (1 2 3 4)
   (3 4 5 6)
 {102F299973}>
DENSE-ARRAYS-DEMO> (aref ** 1 '(-1 :step -2))
#<STANDARD-DENSE-ARRAY (VIEW) 2x4 T
   (3 4 5 6)
   (1 2 3 4)
 {102F29AAF3}>
DENSE-ARRAYS-DEMO> (make-array '(2 10))
#<STANDARD-DENSE-ARRAY 2x10 T
   (0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0)
 {102F29B933}>
DENSE-ARRAYS-DEMO> (make-array '(2 100))
#<STANDARD-DENSE-ARRAY 2x100 T
   (0 0 0 0 0 0 0 0 0 0 ...)
   (0 0 0 0 0 0 0 0 0 0 ...)
 {102F29CEA3}>
DENSE-ARRAYS-DEMO> (describe (make-array '(2 10)))
#<STANDARD-DENSE-ARRAY 2x10 T  {102F29E383}>
  [standard-object]

Slots with :INSTANCE allocation:
  STORAGE                        = #(0 0 0 0 0 0 0 0 0 0 ...)
  DIMENSIONS                     = (2 10)
  ELEMENT-TYPE                   = T
  RANK                           = 2
  TOTAL-SIZE                     = 20
  STRIDES                        = (10 1)
  OFFSETS                        = (0 0)
  CONTIGUOUS-P                   = T
  ROOT-ARRAY                     = NIL
; No value
DENSE-ARRAYS-DEMO> (defparameter a (make-array '(4 10) :constructor #'+))
A
DENSE-ARRAYS-DEMO> (print-array a "~2d")

#<STANDARD-DENSE-ARRAY 4x10 T
   ( 0  1  2  3  4  5  6  7  8  9)
   ( 1  2  3  4  5  6  7  8  9 10)
   ( 2  3  4  5  6  7  8  9 10 11)
   ( 3  4  5  6  7  8  9 10 11 12)
 {102F3D3503}>
NIL
 ```

**Slicing facilities**

```lisp
DENSE-ARRAYS-DEMO> (aref a nil 1)
#<STANDARD-DENSE-ARRAY (VIEW) 4 T
   1 2 3 4
 {102F3E05E3}>
DENSE-ARRAYS-DEMO> (aref a nil -1)
#<STANDARD-DENSE-ARRAY (VIEW) 4 T
   9 10 11 12
 {102F3E1163}>
DENSE-ARRAYS-DEMO> (aref a 1)
#<STANDARD-DENSE-ARRAY (VIEW) 10 T
   1 2 3 4 5 6 7 8 9 10
 {102F3E1D73}>
DENSE-ARRAYS-DEMO> (aref a '(1 :end 3) '(1 :end 3))
#<STANDARD-DENSE-ARRAY (VIEW) 2x2 T
   (2 3)
   (3 4)
 {102F3E2B63}>
DENSE-ARRAYS-DEMO> (defparameter b (aref a '(1 :end 3) '(1 :end 8 :step 2)))
B
DENSE-ARRAYS-DEMO> b
#<STANDARD-DENSE-ARRAY (VIEW) 2x4 T
   (2 4 6 8)
   (3 5 7 9)
 {102F3E4113}>
DENSE-ARRAYS-DEMO> (aref b (make-array '(2 4) :initial-contents '((0 1 0 0) (1 1 0 0))
                                       :element-type 'bit))
#<STANDARD-DENSE-ARRAY 3 T
   4 3 5
 {102F3E6903}>
DENSE-ARRAYS-DEMO> (setf (aref b (make-array '(2 4)
                                             :initial-contents '((0 1 0 0) (1 1 0 0))
                                             :element-type 'bit))
                         0)
0
DENSE-ARRAYS-DEMO> b
#<STANDARD-DENSE-ARRAY (VIEW) 2x4 T
   (2 0 6 8)
   (0 0 7 9)
 {102F3E4113}>
DENSE-ARRAYS-DEMO> a
#<STANDARD-DENSE-ARRAY 4x10 T
   (0 1 2 3 4 5 6 7 8 9)
   (1 2 3 0 5 6 7 8 9 10)
   (2 0 4 0 6 7 8 9 10 11)
   (3 4 5 6 7 8 9 10 11 12)
 {102F3D3503}>
```

Tests are also littered throughout out the system and may serve as examples, for instance [plus/py4cl2.lisp](plus/py4cl2.lisp).

# Usage

### Using Ultralisp

- See the instructions for installing the `digikar99/specialized-array-dispatch` dist at https://github.com/digikar99/adhoc-polymorphic-functions/#getting-it-from-ultralisp
- This library and others hasn't yet been added to quicklisp because one, it is not yet stable,
and two, the version of trivial-types in quicklisp needs an update

### Without using Ultralisp

0. Obtain
   - [adhoc-polymorphic-functions](https://github.com/digikar99/adhoc-polymorphic-functions/)
   - my copy of [trivial-types](https://github.com/digikar99/trivial-types/) (original has been archived by the author)
   - [abstract-arrays](https://github.com/digikar99/abstract-arrays)
   - [compiler-macro-notes](https://github.com/digikar99/compiler-macro-notes)
   - perhaps a few other alpha-stage things not added to quicklisp yet (raise an issue!)
1. Clone into `$QUICKLISP_HOME/local-projects`. (See `ql:*local-project-directories*`.)
2. `(ql:quickload "dense-arrays")` - or dense-arrays-plus or dense-arrays-plus-lite
3. Optionally: `(asdf:test-system "dense-arrays")`- or dense-arrays-plus or dense-arrays-plus-lite

Feel free to raise an issue!
