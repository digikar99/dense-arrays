# dense-arrays

[Last README update: 5th June 2022. To skip rant, click [here](#introduction).]

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [dense-arrays](#dense-arrays)
    - [Rant](#rant)
    - [Introduction](#introduction)
        - [Included Systems](#included-systems)
    - [Basic Demonstration](#basic-demonstration)
    - [Usage](#usage)
        - [Using Ultralisp](#using-ultralisp)
        - [Without using Ultralisp](#without-using-ultralisp)
    - [API Reference: dense-arrays-plus-lite](#api-reference-dense-arrays-plus-lite)
        - [\*array-element-print-format\*](#array-element-print-format)
        - [\*array-element-type\*](#array-element-type)
        - [\*array-element-type-alist\*](#array-element-type-alist)
        - [\*array-layout\*](#array-layout)
        - [\*dense-array-class\*](#dense-array-class)
        - [aref](#aref)
            - [Polymorph: `((common-lisp:array common-lisp:array) &rest abstract-arrays::subscripts)`](#polymorph-common-lisparray-common-lisparray-rest-abstract-arrayssubscripts)
            - [Polymorph: `((array dense-array) &rest dense-arrays::subscripts)`](#polymorph-array-dense-array-rest-dense-arrayssubscripts)
        - [array](#array)
        - [array-dimension](#array-dimension)
        - [array-dimensions](#array-dimensions)
            - [Polymorph: `((common-lisp:array common-lisp:array))`](#polymorph-common-lisparray-common-lisparray)
            - [Polymorph: `((common-lisp:array abstract-arrays:abstract-array))`](#polymorph-common-lisparray-abstract-arraysabstract-array)
        - [array-displaced-to](#array-displaced-to)
        - [array-displacement](#array-displacement)
        - [array-element-type](#array-element-type)
            - [Polymorph: `((common-lisp:array common-lisp:array))`](#polymorph-common-lisparray-common-lisparray-1)
            - [Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`](#polymorph-abstract-arraysabstract-array-abstract-arraysabstract-array)
        - [array-layout](#array-layout)
        - [array-offset](#array-offset)
        - [array-offsets](#array-offsets)
        - [array-rank](#array-rank)
            - [Polymorph: `((common-lisp:array common-lisp:array))`](#polymorph-common-lisparray-common-lisparray-2)
            - [Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`](#polymorph-abstract-arraysabstract-array-abstract-arraysabstract-array-1)
        - [array-storage](#array-storage)
            - [Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`](#polymorph-abstract-arraysabstract-array-abstract-arraysabstract-array-2)
            - [Polymorph: `((common-lisp:array common-lisp:array))`](#polymorph-common-lisparray-common-lisparray-3)
        - [array-storage-allocator](#array-storage-allocator)
        - [array-storage-deallocator](#array-storage-deallocator)
        - [array-stride](#array-stride)
        - [array-strides](#array-strides)
        - [array-total-size](#array-total-size)
            - [Polymorph: `((common-lisp:array common-lisp:array))`](#polymorph-common-lisparray-common-lisparray-4)
            - [Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`](#polymorph-abstract-arraysabstract-array-abstract-arraysabstract-array-3)
        - [array=](#array)
        - [arrayp](#arrayp)
        - [as-cl-array](#as-cl-array)
        - [asarray](#asarray)
        - [broadcast-array](#broadcast-array)
        - [broadcast-arrays](#broadcast-arrays)
        - [broadcast-compatible-p](#broadcast-compatible-p)
        - [copy-array](#copy-array)
        - [copy-dense-array](#copy-dense-array)
        - [define-array-class](#define-array-class)
        - [dense-array-type-class](#dense-array-type-class)
        - [do-arrays](#do-arrays)
        - [eye](#eye)
        - [full](#full)
        - [full-like](#full-like)
        - [macro-map-array](#macro-map-array)
        - [make-array](#make-array)
        - [narray-dimensions](#narray-dimensions)
            - [Polymorph: `((common-lisp:array abstract-arrays:abstract-array))`](#polymorph-common-lisparray-abstract-arraysabstract-array-1)
        - [ones](#ones)
        - [ones-like](#ones-like)
        - [print-array](#print-array)
        - [rand](#rand)
        - [rand-like](#rand-like)
        - [reshape](#reshape)
        - [row-major-aref](#row-major-aref)
            - [Polymorph: `((common-lisp:array common-lisp:array) (abstract-arrays::index t))`](#polymorph-common-lisparray-common-lisparray-abstract-arraysindex-t)
            - [Polymorph: `((array dense-array) (dense-arrays::index t))`](#polymorph-array-dense-array-dense-arraysindex-t)
        - [simple-array](#simple-array)
        - [simple-unupgraded-array](#simple-unupgraded-array)
        - [standard-dense-array](#standard-dense-array)
        - [standard-dense-array-class](#standard-dense-array-class)
        - [storage-accessor](#storage-accessor)
        - [storage-allocator](#storage-allocator)
        - [storage-deallocator](#storage-deallocator)
        - [storage-element-type-upgrader](#storage-element-type-upgrader)
        - [storage-type-inferrer-from-array-type](#storage-type-inferrer-from-array-type)
        - [transpose](#transpose)
        - [unupgraded-array](#unupgraded-array)
        - [unupgraded-dense-array](#unupgraded-dense-array)
        - [zeros](#zeros)
        - [zeros-like](#zeros-like)

<!-- markdown-toc end -->


## Rant

Quick! Tell me the dimensions of

```lisp
#2A((0 4 2 1 9 1 5 9 8 8 6 8 2 0 4 0 2 3 6 8 2 6 7 2 6 7 2 7 8 9 6 8 5 6 3 1 5
     5 3 5 0 4 1 4 5 4 3 2 6 1 0 5 2 5 1 6 2 2 3 3 7 9 5 8 2 5 0 5 5 3 8 6 3 7
     9 4 7 3 3 1 3 0 0 0 8 8 4 8 2 5 7 2 5 6 6 1 5 1 5 4 3 1 3 9 7 0 9 8)
    (2 0 0 7 0 5 8 7 2 0 3 1 9 3 3 2 8 5 0 3 1 3 5 6 9 2 9 8 5 2 0 6 4 7 7 2 5
     6 7 8 3 3 6 3 1 7 3 2 8 0 6 5 9 8 7 7 4 5 8 5 4 8 8 1 6 8 0 7 8 7 4 6 3 9
     3 3 0 9 4 9 7 1 0 0 9 6 3 7 4 4 7 7 5 9 5 0 9 2 2 5 1 6 6 6 5 9 8 2)
    (6 5 0 3 7 2 4 0 3 4 9 3 1 9 8 2 4 4 7 2 3 8 3 9 4 4 8 8 7 0 6 2 0 1 9 9 2
     4 7 4 1 4 2 1 4 1 9 7 7 2 9 0 0 6 0 5 1 5 5 2 8 5 8 9 0 1 7 0 8 9 5 0 2 3
     8 5 8 5 2 4 0 9 3 9 4 8 4 8 7 1 8 5 5 4 3 4 0 3 5 7 9 4 2 5 4 7 0 0)
    (8 8 9 6 9 3 6 4 4 2 3 9 4 8 4 1 2 1 0 5 3 0 5 9 4 3 9 7 9 8 8 4 5 3 6 9 9
     0 9 1 8 1 7 5 7 1 8 1 7 2 4 2 4 9 0 9 9 3 8 5 8 8 1 4 9 0 5 8 9 7 5 4 3 4
     1 4 2 9 1 9 8 2 4 5 6 2 0 2 0 0 5 8 5 9 9 4 2 7 7 1 7 5 7 6 2 9 6 2)
    (2 4 9 8 2 7 1 6 0 8 4 2 9 4 6 9 9 4 1 5 1 4 0 1 9 9 0 1 3 8 0 9 7 9 2 3 9
     5 8 0 8 8 1 2 4 9 1 0 5 4 5 4 8 5 7 2 8 1 6 5 5 3 0 9 0 8 8 8 5 0 7 6 7 1
     6 3 8 8 6 9 7 9 9 5 2 1 6 7 5 3 3 9 7 2 4 0 3 8 4 9 7 8 1 5 1 9 1 6)
    (7 7 0 7 9 6 1 5 8 4 2 9 4 9 9 6 6 9 3 3 7 6 1 6 6 6 3 5 5 1 6 7 3 7 6 2 8
     7 3 4 2 4 8 1 3 0 8 3 5 4 7 6 5 6 0 9 5 0 0 7 6 0 9 1 0 0 5 3 1 2 1 0 5 5
     6 2 4 7 5 5 7 7 6 5 1 6 6 1 4 0 9 9 0 2 9 5 4 0 7 1 2 1 3 9 9 7 7 9))
```

and also the element-type!

```lisp
CL-USER> (progn
           (print (array-dimensions *))
           (print (array-element-type *)))
(6 108)
T
T
```

Why do I need to do this every f*cking time? This can't be so annoying. I mean, why do I need to do all that to get something as basic as dimensions and element-type? Like magicl you mean?

```lisp
#<MAGICL:MATRIX/SINGLE-FLOAT (6x108):
  .
  .
  .
  . >
```

See how pretty? Well not sarcastically. It is prettier than cl:array indeed, in that I can at least tell the element-type and dimensions at a glance, what more can you need?

```lisp
(setq *print-length* 10)
```

```lisp
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 6x108 T
   (7 6 1 3 7 1 6 5 2 5 ...)
   (8 7 6 9 7 5 5 1 0 0 ...)
   (8 1 7 8 9 0 2 9 5 5 ...)
   (0 5 8 6 2 6 9 2 5 3 ...)
   (4 8 3 4 2 3 2 2 2 5 ...)
   (7 8 2 5 5 3 2 8 6 1 ...)
 {103F4CE453}>
```

Oh wait!

```lisp
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 6x108 SINGLE-FLOAT
   (  0.054       7.363       8.527       8.561       7.399       1.875
      0.513       6.690       0.398       0.745     ...)
   (  6.766       7.421       2.419       7.857       8.914       6.637
      8.464       1.052       1.400       3.602     ...)
   (  7.384       6.489       9.426       9.044       6.571       9.431
      6.177       9.389       3.526       4.850     ...)
   (  7.873       6.541       2.272       7.863       7.271       8.398
      7.759       3.428       3.482       7.050     ...)
   (  4.954       9.796       7.450       9.431       2.254       7.741
      5.725       1.729       4.947       4.176     ...)
   (  6.553       4.481       0.159       5.915       4.391       5.940
      1.160       2.071       4.158       8.746     ...)
 {103F81F7B3}>
```

No, wait!

So that first one is a 6x108 array with element-type T while the second has element-type single-float. And both have row-major layouts. Oh, and we even have their identity in case we want to check whether two objects are the same or just their elements are same!

We could have simply written a nice print-array function, couldn't we? Well, perhaps yes, but perhaps not! What do you mean?

```lisp
(describe *)
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 6x108 SINGLE-FLOAT {103F81F7B3}>
  [standard-object]

Slots with :INSTANCE allocation:
  STORAGE                        = #(0.05400777 7.363088 8.526753 8.5613365 7.398881 1.8748772 0.5133271..
  DIMENSIONS                     = (6 108)
  ELEMENT-TYPE                   = SINGLE-FLOAT
  RANK                           = 2
  TOTAL-SIZE                     = 648
  STRIDES                        = (108 1)
  OFFSETS                        = (0 0)
  LAYOUT                         = :ROW-MAJOR
  ROOT-ARRAY                     = NIL
```

Storage, dimensions, element-type, rank, total-size, pretty obvious stuff.

Hmm, strides, offsets, layout, and root array: what... are these? Offsets, I think I know, it sounds like displaced-index-offset of cl:array, but wait, why is it a list and not a single number? Welcome to dense-arrays :)

But does that mean all the effort spent on magicl *all these years* will go to waste? Thankfully not! Turns out both magicl and dense-arrays - specifically standard-dense-array - are using cl:vector under the hood for storage:

```
(describe (magicl:rand '(6 108) :type 'single-float))
#<MAGICL:MATRIX/DOUBLE-FLOAT (6x108):..
  [structure-object]

Slots with :INSTANCE allocation:
  NROWS                          = 6
  NCOLS                          = 108
  SIZE                           = 648
  LAYOUT                         = :COLUMN-MAJOR
  STORAGE                        = #(0.23684204 0.7401872 0.8467122 0.17149377 0.5106092 0.10455426..
```

What that means is:

```lisp
(ql:quickload "dense-arrays+magicl")
```

```lisp
(rand 5 5 :type 'single-float)
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 5x5 SINGLE-FLOAT
   (  0.058       0.553       0.918       0.653       0.080    )
   (  0.283       0.289       0.843       0.236       0.333    )
   (  0.434       0.701       0.826       0.339       0.306    )
   (  0.275       0.021       0.774       0.248       0.841    )
   (  0.426       0.913       0.662       0.058       0.395    )
 {10438D6D13}>
```

```lisp
(magicl-funcall #'magicl:svd *)
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 5x5 SINGLE-FLOAT
   ( -0.460      -0.292       0.740       0.354       0.174    )
   ( -0.401       0.179       0.112      -0.801       0.391    )
   ( -0.497      -0.221      -0.107      -0.209      -0.806    )
   ( -0.401       0.842      -0.068       0.348      -0.065    )
   ( -0.469      -0.352      -0.651       0.262       0.404    )
 {10476B6C93}>
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 5x5 SINGLE-FLOAT
   (  2.486       0.000e+00   0.000e+00   0.000e+00   0.000e+00)
   (  0.000e+00   0.793       0.000e+00   0.000e+00   0.000e+00)
   (  0.000e+00   0.000e+00   0.635       0.000e+00   0.000e+00)
   (  0.000e+00   0.000e+00   0.000e+00   0.181       0.000e+00)
   (  0.000e+00   0.000e+00   0.000e+00   0.000e+00   0.105    )
 {10476B87F3}>
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 5x5 SINGLE-FLOAT
   ( -0.268      -0.465      -0.721      -0.278      -0.340    )
   (  0.025      -0.717       0.150      -0.044       0.678    )
   ( -0.421      -0.360       0.320       0.660      -0.393    )
   ( -0.496       0.356      -0.444       0.402       0.519    )
   ( -0.710       0.115       0.398      -0.569       0.023    )
 {10476B8D03}>
```

Welcome again to dense-arrays :D!

## Introduction

This system provides

- a metaclass `dense-array-class`
- a class `dense-array` with
  - multidimensional strides and offsets: CL arrays can only have a single offset
  - an option for layout: row-major or column-major to play nice with libraries like or [magicl](https://github.com/quil-lang/magicl). NIL layout implies either non-contiguous arrays or an unknown layout.
  - customizable behavior, especially storage slot: depending on the exact class of `dense-array`, the storage object and associated meta-information could correspond to usual `(cl:simple-array element-type 1)` or [static-vectors](https://github.com/sionescu/static-vectors) or [cl-cuda](https://github.com/takagi/cl-cuda)[DOC.org](./DOC.org) for more details about customization.
- [a rich aref](#basic-demonstration)
- a nicer default print-object that respects `*print-array* *print-length* *print-level* *print-lines*` and is customizable via `*array-element-print-format*`; this could be improved and integrated further with builtins once someone wraps their head around [The Lisp Pretty Printer](http://www.lispworks.com/documentation/lw51/CLHS/Body/22_b.htm).
- a `unupgraded-array` and `simple-unupgraded-array` types that use `(cl:simple-array * 1)` for storage but do not upgrade the types; this can be helpful for better type checking
- dynamic variables: `*array-element-type* *array-element-type-alist* *dense-array-class* *array-layout*`

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
- cannot be a drop-in replacement for built-in arrays because `cl:array` is both a class and a specializing type-specifier; IIUC, non-builtins can only either be one of class or specializing type-specifier. However, this can now be rectified using [extensible-compound-types](https://github.com/digikar99/extensible-compound-types), however one needs to also think about playing nice in the presence or absence of extensible-compound-types.
- `(setf aref)` and `(setf row-major-aref)` may need to be used using `(funcall #'(setf aref) ...)` since some implementations like SBCL "lose" the type information from the environment in an attempt to use `once-only`
- Although `:layout` has been provided recently, support for it can be buggy. Bug reports will be appreciated!


### Included Systems

- `dense-arrays`: the super bare essentials
- `dense-arrays-plus-lite`: some utilities
- `dense-arrays+static-vectors`: provides and exports a `static-array` type that is essentially a wrapper around [static-vectors](https://github.com/sionescu/static-vectors)
- `dense-arrays+cuda`: provides and export an array using `cl-cuda`
- `dense-arrays+magicl`: provides helper functions `magicl-funcall from-magicl-tensor as-magicl-tensor` for interoperating between `standard-dense-array` and `magicl:vector magicl:matrix magicl:tensor`
- `dense-arrays-plus`: more utilities as well as static-vectors

Minimalists would want to stick to the first four. The last one also introduces

- `shape` as an alias for `array-dimensions`
- `int32 uint32 uint8` types as aliases for their common lisp counterparts
- integration with [py4cl2](#py4cl2): simply set `py4cl2:*array-type*` to `:dense-arrays` when you want to use py4cl2 with dense-arrays.
- and perhaps more things!


## Basic Demonstration

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
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3x4 T
   ((0 1 2 3)
    (1 2 3 4)
    (2 3 4 5))
   ((1 2 3 4)
    (2 3 4 5)
    (3 4 5 6))
 {100980B593}>
DENSE-ARRAYS-DEMO> (aref * 1 '(0 :step 2))
#<STANDARD-DENSE-ARRAY NIL 2x4 T
   (1 2 3 4)
   (3 4 5 6)
 {1009810073}>
DENSE-ARRAYS-DEMO> (aref ** 1 '(-1 :step -2))
#<STANDARD-DENSE-ARRAY NIL 2x4 T
   (3 4 5 6)
   (1 2 3 4)
 {1009811E73}>
DENSE-ARRAYS-DEMO> (make-array '(2 10))
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x10 T
   (0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0)
 {1009813D63}>
DENSE-ARRAYS-DEMO> (make-array '(2 100))
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x100 T
   (0 0 0 0 0 0 0 0 0 0 ...)
   (0 0 0 0 0 0 0 0 0 0 ...)
 {10098286B3}>
DENSE-ARRAYS-DEMO> (describe (make-array '(2 10)))
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x10 T {100982C243}>
  [standard-object]

Slots with :INSTANCE allocation:
  STORAGE                        = #(0 0 0 0 0 0 0 0 0 0 ...)
  DIMENSIONS                     = (2 10)
  ELEMENT-TYPE                   = T
  RANK                           = 2
  TOTAL-SIZE                     = 20
  STRIDES                        = (10 1)
  OFFSETS                        = (0 0)
  LAYOUT                         = :ROW-MAJOR
  ROOT-ARRAY                     = NIL
; No value
DENSE-ARRAYS-DEMO> (defparameter a (make-array '(4 10) :constructor #'+))
A
DENSE-ARRAYS-DEMO> (print-array a "~2d")

#<STANDARD-DENSE-ARRAY :ROW-MAJOR 4x10 T
   ( 0  1  2  3  4  5  6  7  8  9)
   ( 1  2  3  4  5  6  7  8  9 10)
   ( 2  3  4  5  6  7  8  9 10 11)
   ( 3  4  5  6  7  8  9 10 11 12)
 {1009837073}>
NIL
 ```

**Slicing facilities**

```lisp
DENSE-ARRAYS-DEMO> (aref a nil 1)
#<STANDARD-DENSE-ARRAY NIL 4 T
   1
   2
   3
   4
 {100984D933}>
DENSE-ARRAYS-DEMO> (aref a nil -1)
#<STANDARD-DENSE-ARRAY NIL 4 T
   9
   10
   11
   12
 {100984ECC3}>
DENSE-ARRAYS-DEMO> (aref a 1)
#<STANDARD-DENSE-ARRAY NIL 10 T
   1
   2
   3
   4
   5
   6
   7
   8
   9
   10
 {100984FFA3}>
DENSE-ARRAYS-DEMO> (aref a '(1 :end 3) '(1 :end 3))
#<STANDARD-DENSE-ARRAY NIL 2x2 T
   (2 3)
   (3 4)
 {10098622A3}>
DENSE-ARRAYS-DEMO> (defparameter b (aref a '(1 :end 3) '(1 :end 8 :step 2)))
B
DENSE-ARRAYS-DEMO> b
#<STANDARD-DENSE-ARRAY NIL 2x4 T
   (2 4 6 8)
   (3 5 7 9)
 {1009863FF3}>
DENSE-ARRAYS-DEMO> (aref b (make-array '(2 4) :initial-contents '((0 1 0 0) (1 1 0 0))
                                       :element-type 'bit))
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 3 T
   4
   3
   5
 {1009867F13}>
DENSE-ARRAYS-DEMO> (setf (aref b (make-array '(2 4)
                                             :initial-contents '((0 1 0 0) (1 1 0 0))
                                             :element-type 'bit))
                         0)
0
DENSE-ARRAYS-DEMO> b
#<STANDARD-DENSE-ARRAY NIL 2x4 T
   (2 0 6 8)
   (0 0 7 9)
 {1009863FF3}>
DENSE-ARRAYS-DEMO> a
#<STANDARD-DENSE-ARRAY :ROW-MAJOR 4x10 T
   (0 1 2 3 4 5 6 7 8 9)
   (1 2 3 0 5 6 7 8 9 10)
   (2 0 4 0 6 7 8 9 10 11)
   (3 4 5 6 7 8 9 10 11 12)
 {1009837073}>
```

Tests are also littered throughout out the system and may serve as examples, for instance [plus/py4cl2.lisp](plus/py4cl2.lisp).

## Usage

### Using Ultralisp

- See the instructions for installing the `digikar99/specialized-array-dispatch` dist at https://github.com/digikar99/adhoc-polymorphic-functions/#getting-it-from-ultralisp
- This library and others hasn't yet been added to quicklisp because one, it is not yet stable,
and two, the version of trivial-types in quicklisp needs an update

### Without using Ultralisp

0. Obtain
   - [polymorphic-functions](https://github.com/digikar99/polymorphic-functions/)
   - my copy of [trivial-types](https://github.com/digikar99/trivial-types/) (original has been archived by the author)
   - [abstract-arrays](https://github.com/digikar99/abstract-arrays)
   - [compiler-macro-notes](https://github.com/digikar99/compiler-macro-notes)
   - optionally [extensible-compound-types](https://github.com/digikar99/extensible-compound-types)
   - perhaps a few other alpha-stage things not added to quicklisp yet (raise an issue!)
1. Clone into `$QUICKLISP_HOME/local-projects`. (See `ql:*local-project-directories*`.)
2. `(ql:quickload "dense-arrays")` - or dense-arrays-plus or dense-arrays-plus-lite
3. Optionally: `(asdf:test-system "dense-arrays")`- or dense-arrays-plus or dense-arrays-plus-lite

Feel free to raise an issue!

## API Reference: dense-arrays-plus-lite

### \*array-element-print-format\*

```lisp
Variable
Default Value: "~/DENSE-ARRAYS::PRETTY-PRINT-NUMBER/"
```

The format control string used to print the elements of [dense-arrays:array](#array).

It is possible to set this value to "~/USER-DEFINED-FUNCTION/" where
USER-DEFINED-FUNCTION should accept at least four arguments.

Also see:
- https://en.wikipedia.org/wiki/Format_(Common_Lisp)
- http://www.gigamonkeys.com/book/a-few-format-recipes.html

### \*array-element-type\*

```lisp
Variable
Default Unbound
```

If BOUND, this is the default value of the ELEMENT-TYPE or TYPE argument.
Overrides [\*array-element-type-alist\*](#array-element-type-alist).
Is overriden by explicitly passing an ELEMENT-TYPE or TYPE argument.

### \*array-element-type-alist\*

```lisp
Variable
Default Value: ((#<PACKAGE "DENSE-NUMERICALS.IMPL"> . SINGLE-FLOAT))
```

An ALIST mapping package to the default element-type used in that package.
(Inspired from SWANK:*READTABLE-ALIST*)
Overrides none.
Is overriden by [\*array-element-type\*](#array-element-type) when bound, or by explicitly passing an
  ELEMENT-TYPE or TYPE argument.

### \*array-layout\*

```lisp
Variable
Default Value: :ROW-MAJOR
```

Specifies the default layout constructed by [dense-arrays:make-array](#make-array) and
constructor functions like [asarray](#asarray), [zeros](#zeros), [ones](#ones), etc in the
DENSE-ARRAYS-PLUS-LITE package.

### \*dense-array-class\*

```lisp
Variable
Default Value: #<DENSE-ARRAYS:STANDARD-DENSE-ARRAY-CLASS DENSE-ARRAYS:STANDARD-DENSE-ARRAY>
```

Specifies the default value of CLASS in [dense-arrays:make-array](#make-array)
and other functions. (TODO: Specify these other functions.)

### aref

```lisp
Polymorphic Function: (aref array &rest subscripts)
```

This is SETF-able.

#### Polymorph: `((common-lisp:array common-lisp:array) &rest abstract-arrays::subscripts)`

A wrapper around CL:AREF.

Return the element of the `array` specified by the `subscripts`.

#### Polymorph: `((array dense-array) &rest dense-arrays::subscripts)`

Accessor function for DENSE-ARRAYS::DENSE-ARRAY.
The semantics are intended to be similar to numpy's indexing semantics.
See https://numpy.org/doc/stable/user/basics.indexing.html

Each element of `subscripts` can be
- either an integer denoting the position within the axis which is to be indexed
- or a list of the form (&OPTIONAL START &KEY END STEP) with each of START END
  STEP being integers if supplied. START denotes the start position within the
  axis, END denotes the ending position within the axis, STEP denotes at what
  distance within the axis the next element should come after the previous,
  starting from START

Each of the `subscripts`, START, END, STEP can also be negative integers, in which
case the last element along the axis is given the index -1, the second last is
given the index -2 and so on. Thus, `(aref ... '(-1 :step -1))` can reverse a one
dimensional array.

Like, CL:AREF, returns the element corresponding to `subscripts`
if all the subscripts are integers and there as many subscripts
as the rank of the array.

If the number (aka length) of `subscripts` were less than the array's rank, or
if some of the `subscripts` were lists described above, then returns a VIEW
of the arrays. A VIEW is a window into the original array and thus
avoids copying the elements of the original array.

Examples illustrating the numpy-equivalent indexes:

    a[::]       (aref a nil)
    a[::2]      (aref a '(0 :step 2))
    a[3, ::-1]  (aref a 3 '(-1 :step -1))
    a[3::, -1]  (aref a '(3) -1)

The `subscripts` can also be integer or boolean arrays, denoting which elements
to select from each of the axes. But in this case the corresponding elements
of the array are copied over into a new array.

### array

```lisp
Type: (ARRAY &OPTIONAL (ABSTRACT-ARRAYS::ELEMENT-TYPE '*)
       (ABSTRACT-ARRAYS::DIM/RANK '*))
```

A wrapper around STANDARD-DENSE-ARRAY with support for specifying ELEMENT-TYPE and DIMENSIONS or RANK.
These specializers are the same like the CL:ARRAY compound type.


### array-dimension

```lisp
Function: (array-dimension array axis-number)
```

Return the length of dimension `axis-number` of `array`.

### array-dimensions

```lisp
Polymorphic Function: (array-dimensions array)
```

#### Polymorph: `((common-lisp:array common-lisp:array))`

No documentation found.

#### Polymorph: `((common-lisp:array abstract-arrays:abstract-array))`

Returns a COPY of the dimensions of `array`. The copy may then be modified.

See [narray-dimensions](#narray-dimensions) or equivalent of a copy is to be avoided, and destructive
use is not intended.

### array-displaced-to

```lisp
Function: (array-displaced-to array)
```

### array-displacement

```lisp
Function: (array-displacement array)
```

Returns two values:
- [array-storage](#array-storage)
- and OFFSET along first axis
Consequences are undefined if `array` is displaced along multiple axis.

### array-element-type

```lisp
Polymorphic Function: (array-element-type array)
```

#### Polymorph: `((common-lisp:array common-lisp:array))`

No documentation found.

#### Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`

No documentation found.

### array-layout

```lisp
Function: (array-layout array)
```

### array-offset

```lisp
Function: (array-offset array axis-number)
```

Return the length of offset corresponding to `axis-number` of `array`.

### array-offsets

```lisp
Function: (array-offsets array)
```

### array-rank

```lisp
Polymorphic Function: (array-rank array)
```

#### Polymorph: `((common-lisp:array common-lisp:array))`

No documentation found.

#### Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`

No documentation found.

### array-storage

```lisp
Polymorphic Function: (array-storage array)
```

#### Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`

No documentation found.

#### Polymorph: `((common-lisp:array common-lisp:array))`

No documentation found.

### array-storage-allocator

No documentation found for `array-storage-allocator`

### array-storage-deallocator

No documentation found for `array-storage-deallocator`

### array-stride

```lisp
Function: (array-stride array axis-number)
```

Return the length of stride corresponding to `axis-number` of `array`.

### array-strides

```lisp
Function: (array-strides array)
```

### array-total-size

```lisp
Polymorphic Function: (array-total-size array)
```

#### Polymorph: `((common-lisp:array common-lisp:array))`

No documentation found.

#### Polymorph: `((abstract-arrays:abstract-array abstract-arrays:abstract-array))`

No documentation found.

### array=

```lisp
Function: (array= array1 array2 &key (test (function equalp)))
```

Returns non-NIL if each element of `array1` is equal to each corresponding
element of `array2` using `test`, which should be a two-argument function that takes
the one element of the first array and the corresponding element of the second
and tests for their equality.

### arrayp

```lisp
Function: (arrayp object)
```

### as-cl-array

```lisp
Function: (as-cl-array array)
```

### asarray

```lisp
Function: (asarray array-like &key (out NIL outp) (type default-element-type)
           (layout *array-layout*))
```

`type` can also be :AUTO

### broadcast-array

```lisp
Function: (broadcast-array array broadcast-dimensions)
```

### broadcast-arrays

```lisp
Function: (broadcast-arrays &rest arrays)
```

Returns two values. The first value is the list of broadcasted arrays
  if the second value is non-NIL.

### broadcast-compatible-p

```lisp
Function: (broadcast-compatible-p &rest arrays)
```

Returns two values:
- The first value is a generalized boolean indicating whether the arrays can be broadcasted.
- The second value is the dimension of the array resulting from the broadcast.

The broadcasting semantics are equivalent to numpy semantics. Two arrays are broadcast
compatible, if
- they have the same dimensions, or
- if, for the dimensions they differ, one of the dimension is of length 1, or
- the dimensions of the lower-ranked array matches the rightmost dimensions
  of the higher-ranked array

Thus, arrays with the following dimensions are broadcast-compatible:
- (3) (3)
- (3 1) (3 3)
- (3 3) (1 3)
- (3 3) (3)

Arrays with the following dimensions are not compatible:
- (3 1) (3)

See https://numpy.org/doc/stable/user/basics.broadcasting.html for an elaborate discussion.

### copy-array

```lisp
Function: (copy-array array &key (layout (array-layout array)))
```

Returns a copy of `array`. Creates a completely new array even if `array`
is a VIEW (see ARRAY-VIEW-P).

### copy-dense-array

No documentation found for `copy-dense-array`

### define-array-class

```lisp
Macro: (define-array-class name &body (direct-slots &rest slot-options))
```

Defines `name` as a CLASS with DIRECT-SUPERCLASS ABSTRACT-ARRAY and metaclass
as ABSTRACT-ARRAY-CLASS. Also defines the appropriate order using `direct-slots`.

### dense-array-type-class

```lisp
Function: (dense-array-type-class array-type &optional env)
```

### do-arrays

```lisp
Macro: (do-arrays rank/bindings &body body)
```

  Traverses the arrays in row-major order.

  If the first argument `rank/bindings` is of type SIZE, it'd be treated as the rank
  of the arrays. Then, the BINDINGS are assumed to be the first element of the `body`.

  Otherwise, the first argument is treated as if they are BINDINGS.
  Each BINDING is of the form
    (ELT-VAR [array](#array) &OPTIONAL (ELEMENT-TYPE *) &KEY (CLASS-NAME [\*dense-array-class\*](#dense-array-class)))
  Here, only [array](#array) is evaluated.

Examples

    (let ((a (make-array '(2 3)))
          (b (make-array '(2 3))))
      (do-arrays ((c a t)
                  (d b t))
        (print (list c d))))

    (let ((a (make-array '(2 3)))
          (b (make-array '(2 3))))
      (do-arrays 2 ((c a t) ; The 2 indicates the rank of the arrays
                    (d b t))
        (print (list c d))))

Either of the two cases might be faster depending on the number of dimensions.

### eye

```lisp
Function: (eye per-axis-size &key (rank 2) (type default-element-type))
```

### full

```lisp
Function: (full &rest args)
```

LAMBDA-LIST: (SHAPE &KEY (TYPE DEFAULT-ELEMENT-TYPE) (LAYOUT [\*array-layout\*](#array-layout))
              VALUE)

### full-like

```lisp
Function: (full-like array-like value)
```

### macro-map-array

```lisp
Macro: (macro-map-array result-array function &rest arrays)
```

### make-array

```lisp
Function: (make-array dimensions &rest args &key
           (element-type default-element-type)
           (initial-element NIL initial-element-p)
           (initial-contents NIL initial-contents-p)
           (constructor NIL constructor-p) (strides NIL strides-p)
           (adjustable NIL adjustable-p) (fill-pointer NIL fill-pointer-p)
           (class *dense-array-class*) (layout *array-layout*)
           (displaced-to NIL displaced-to-p) (offsets NIL offsets-p)
           (displaced-index-offset 0 displaced-index-offset-p))
```

Like CL:MAKE-ARRAY but returns a DENSE-ARRAYS::DENSE-ARRAY instead of [cl:array](#array).
Additionally takes

- `layout` argument which can be one of (:ROW-MAJOR :COLUMN-MAJOR NIL)
- `class` argument which should be a class designator denoting the class to which the
  constructed dense array will belong to

- `constructor` if supplied should be a function that takes as many arguments
  as the number of dimensions aka rank of the array, and return the element that
  should correspond to the position indicated by the arguments of the function.
  For example:

    (make-array '(2 3) :constructor (lambda (&rest indexes) (cons 'indexes indexes)))
    ;=> #<[standard-dense-array](#standard-dense-array) :ROW-MAJOR 2x3 T
          ((INDEXES 0 0) (INDEXES 0 1) (INDEXES 0 2))
          ((INDEXES 1 0) (INDEXES 1 1) (INDEXES 1 2))
         {10194A2FE3}>
    

### narray-dimensions

```lisp
Polymorphic Function: (narray-dimensions array)
```

#### Polymorph: `((common-lisp:array abstract-arrays:abstract-array))`

Returns the dimensions of the `array`. The consequences are undefined if the
returned dimensions are modified. Use [array-dimensions](#array-dimensions) if destructive usage of
the returned list is intended.

### ones

```lisp
Function: (ones &rest args)
```

LAMBDA-LIST: (SHAPE &KEY (TYPE DEFAULT-ELEMENT-TYPE) (LAYOUT [\*array-layout\*](#array-layout)))

### ones-like

```lisp
Function: (ones-like array-like)
```

### print-array

```lisp
Function: (print-array array &optional array-element-print-format &key level
           length (stream NIL streamp))
```

Prints `array` as if by CL:PRINT.
Format recipes: http://www.gigamonkeys.com/book/a-few-format-recipes.html.

### rand

```lisp
Function: (rand &rest args)
```

LAMBDA-LIST: (SHAPE &KEY (TYPE DEFAULT-ELEMENT-TYPE) (LAYOUT [\*array-layout\*](#array-layout))
              (MIN (COERCE 0 TYPE)) (MAX (COERCE 1 TYPE)))

### rand-like

```lisp
Function: (rand-like array-like)
```

### reshape

```lisp
Function: (reshape array-like new-shape &key (view NIL viewp)
           (layout NIL layoutp))
```

`view` argument is considered only if `array-like` is a SIMPLE-DENSE-ARRAY.
If `array-like` is a SIMPLE-DENSE-ARRAY, it is guaranteed that when `view` is supplied,
- :VIEW non-NIL means that no copy of `array-like` is created
- :VIEW NIL a copy of the array *will be* created
What is not guaranteed: if `array-like` is not a SIMPLE-DENSE-ARRAY,
then a new array is created. In the future, an attempt may be made to avoid
creating the new array and instead return a view instead. 

### row-major-aref

```lisp
Polymorphic Function: (row-major-aref array index)
```

Return the element of `array` corresponding to the row-major `index`.
This is SETFable

#### Polymorph: `((common-lisp:array common-lisp:array) (abstract-arrays::index t))`

No documentation found.

#### Polymorph: `((array dense-array) (dense-arrays::index t))`

No documentation found.

### simple-array

```lisp
Type: (SIMPLE-ARRAY &OPTIONAL (ABSTRACT-ARRAYS::ELEMENT-TYPE '*)
       (ABSTRACT-ARRAYS::DIM/RANK '*))
```

A wrapper around (AND STANDARD-DENSE-ARRAY SIMPLE-DENSE-ARRAY) with support for specifying ELEMENT-TYPE and DIMENSIONS or RANK.
These specializers are the same like the CL:ARRAY compound type.


### simple-unupgraded-array

```lisp
Type: (SIMPLE-UNUPGRADED-ARRAY &OPTIONAL (ABSTRACT-ARRAYS::ELEMENT-TYPE '*)
       (ABSTRACT-ARRAYS::DIM/RANK '*))
```

A wrapper around (AND UNUPGRADED-DENSE-ARRAY SIMPLE-DENSE-ARRAY) with support for specifying ELEMENT-TYPE and DIMENSIONS or RANK.
These specializers are the same like the CL:ARRAY compound type.


### standard-dense-array

```lisp
Type: STANDARD-DENSE-ARRAY
```


### standard-dense-array-class

```lisp
Type: STANDARD-DENSE-ARRAY-CLASS
```


### storage-accessor

```lisp
Generic Function: (storage-accessor class)
```

Returns a SYMBOL that is fbound to an accessor function that takes
 (STORAGE INDEX) as arguments and returns the element at INDEX in STORAGE.
The function is an accessor function in the sense that SYMBOL should also be
associated with (SETF SYMBOL) function that takes (NEW-VALUE STORAGE INDEX) as
arguments and sets the STORAGE element at INDEX to NEW-VALUE.
  This function is primarily used inside [aref](#aref), [row-major-aref](#row-major-aref) and [do-arrays](#do-arrays),
and their SETF counterparts.
  See src/protocol.lisp and plus/cl-cuda.lisp for reference.

### storage-allocator

```lisp
Generic Function: (storage-allocator class)
```

Returns a symbol fbound to a function with signature
  (SIZE &KEY ELEMENT-TYPE INITIAL-ELEMENT)
that allocates a VECTOR of length SIZE of ELEMENT-TYPE with each element as
INITIAL-ELEMENT for use as a STORAGE-VECTOR for the ABSTRACT-ARRAY.

### storage-deallocator

```lisp
Generic Function: (storage-deallocator class)
```

Returns either NIL or a symbol fbound to a function to be called
to delete the STORAGE when the ABSTRACT-ARRAY goes out of scope. This function should
take only the STORAGE object as its argument.
  Internally, this function plays a role in the finalizer of the garbage collection
using TRIVIAL-GARBAGE.
  See plus/static-vectors.lisp and the [dense-arrays:make-array](#make-array) function for reference.

### storage-element-type-upgrader

```lisp
Generic Function: (storage-element-type-upgrader class)
```

Equivalent to the CL:UPGRADED-ARRAY-ELEMENT-TYPE, this returns a function
that takes a single argument element-type as input and returns the upgraded
array element type for the array class given by `class` used for STORAGE.
The upgraded array element type is then stored in the dense-array object and
used for other tasks downstream.
  See plus/cl-cuda.lisp and the [dense-arrays:make-array](#make-array) function for reference.

### storage-type-inferrer-from-array-type

```lisp
Generic Function: (storage-type-inferrer-from-array-type class)
```

This should return a function that takes as input the ARRAY-TYPE and returns
the possibly specialized type of storage that the corresponding array object
will have. This is primarily used for optimization purposes inside [dense-arrays:do-arrays](#do-arrays)
and the compiler macros of [dense-arrays:aref](#aref) [dense-arrays:row-major-aref](#row-major-aref) and SETF
counterparts.
  See src/protocol.lisp, plus/cl-cuda.lisp, src/do-arrays.lisp and optim/aref.lisp
for reference.

### transpose

```lisp
Function: (transpose array-like &key axes)
```

### unupgraded-array

```lisp
Type: (UNUPGRADED-ARRAY &OPTIONAL (ABSTRACT-ARRAYS::ELEMENT-TYPE '*)
       (ABSTRACT-ARRAYS::DIM/RANK '*))
```

A wrapper around UNUPGRADED-DENSE-ARRAY with support for specifying ELEMENT-TYPE and DIMENSIONS or RANK.
These specializers are the same like the CL:ARRAY compound type.


### unupgraded-dense-array

```lisp
Type: UNUPGRADED-DENSE-ARRAY
```


### zeros

```lisp
Function: (zeros &rest args)
```

LAMBDA-LIST: (SHAPE &KEY (TYPE DEFAULT-ELEMENT-TYPE) (LAYOUT [\*array-layout\*](#array-layout)))

### zeros-like

```lisp
Function: (zeros-like array-like)
```
