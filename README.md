# dense-arrays

[Last README update: 5th June 2022. To skip rant, click [here](#introduction).]

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

# Usage

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
