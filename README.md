# Status

> README is outdated; to be updated soon.

[Last update: 27th December 2020]

- Under Construction:
  - The API may change
  - The package and system names may change. Therefore, please use package-local-nicknames to avoid global symbol replacements.
  - Semantics of displaced-indexed-offset

Original plan included incorporation of SIMD into `do-arrays`; but I do not see a good way to do this. Instead these facilities may be provided in a separate system either using sb-simd (once constructed!) or BLAS/LAPACK/Intel-MKL/etc. Towards this, integration with [static-vectors](https://github.com/sionescu/static-vectors) is also provided to combat moving-GC based implementations. See [how fast we can get](./perf.org) in the absence of SIMD operations.

# What

A *better* dense array - it *looks* better, and provides copy-free slicing facilities.

From the name, one might guess that one could also make sparse-arrays. I think it should be possible to abstract this system to an abstract array package in line with Julia - or at the least provide interchangeable backends. But, until then, this is what we have!

```lisp
DENSE-ARRAYS-DEMO> (make-array '(2 3 4) :constructor #'+)
#<DENSE-ARRAYS:ARRAY T 2x3x4
   ((0 1 2 3)
    (1 2 3 4)
    (2 3 4 5))
   ((1 2 3 4)
    (2 3 4 5)
    (3 4 5 6))
 {1037AA2DA3}>
DENSE-ARRAYS-DEMO> (aref * 1 '(0 :step 2))
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x4
   (1 2 3 4)
   (3 4 5 6)
 {103840E153}>
DENSE-ARRAYS-DEMO> (aref ** 1 '(-1 :step -2)) ; negative indices
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x4
   (3 4 5 6)
   (1 2 3 4)
 {1017E628A3}>

```

- **Looks:** I wanted to gear this towards prototyping. Most everything one needs to know about the arrays is available at a glance: the element-type, the dimensions, its contents, whether the array is "continuous/original" or just a view into another, and its id. One may also obtain a still better representation using `print-array`.
- **Slicing:**
    - Quick, tell me what `2:10:4` represents! It depends on the context - it means something in numpy and something else in julia; we get rid of this, and instead use the more unambiguous representation `'(start :end end :step step)`. Perhaps, longer to type - lisp is - but better readability!
    - Other than this, slicing facilities are also provided using bit and integer arrays similar to numpy.
- **Speed and compiler-notes:** An effort has been made to provide speed-optimizing compiler notes for `aref` and `do-arrays`. See [perf.org](./perf.org) for more details.

### Couldn't I have just used Common Lisp arrays?

```lisp
CL-USER> (let ((a (make-array '(1000 1000))))
           (time (loop for i below 1000 do (select:select a t i))))
Evaluation took:
  0.044 seconds of real time
  0.044320 seconds of total run time (0.044315 user, 0.000005 system)
  100.00% CPU
  97,854,068 processor cycles
  8,313,888 bytes consed

NIL
CL-USER> (let ((a (dense-arrays:make-array '(1000 1000))))
           (time (loop for i below 1000 do (dense-arrays:aref a nil i))))
Evaluation took:
  0.000 seconds of real time
  0.000222 seconds of total run time (0.000208 user, 0.000014 system)
  100.00% CPU
  486,648 processor cycles
  163,712 bytes consed

NIL
```


In these, we obtain the speed boost by merely allocating the "view" object, instead of copying over the entire subsection of the array.

To enable copy-free slicing, one needs the concept of offsets and strides which are outside the scope of the ANSI standard. Technically, one could get implementors to maintain this separate array object, but hey, if you could implement this *over* the ANSI standard, why not?!

On my machine, dense-arrays-plus works on SBCL and CCL - and to some extent on ECL as well. I'm not setting up CI just yet to save myself some CI-debug-time. In particular, the following systems are provided

- `dense-arrays`: the super bare essentials
- `dense-arrays-plus-lite`: some utilities
- `dense-arrays+static-vectors`: provides `*use-static-vectors*` and `*use-static-vectors-alist*` and adds a `:static` keyword to make-array to enable usage of [static-vectors](https://github.com/sionescu/static-vectors) in the background
- `dense-arrays-plus`: more utilities as well as static-vectors

Minimalists would want to stick to the first two. The last one also introduces

- `shape` as an alias for `array-dimensions`
- `int32 uint32 uint8` types as aliases for their common lisp counterparts
- integration with [py4cl2](#py4cl2): simply set `py4cl2:*array-type*` to `:dense-arrays` when you want to use py4cl2 with dense-arrays.
- and perhaps more things!

Development of the following system could aid for further minimalism of the first two systems:

- trivial-form-type: For form-type, there is a full dependency on [compiler-macros](https://github.com/Bike/compiler-macro)

# Exported Symbols as of this commit

- dense-arrays:
    - array
    - arrayp
    - make-array
    - aref
    - row-major-aref
    - copy-array
    - do-arrays
    - array=
    - print-array
    - array-dimensions
    - narray-dimensions
    - array-displaced-to
    - array-dimension
    - array-rank
    - array-displacement
    - array-element-type
    - array-total-size
    - \*array-element-print-format\*

- dense-arrays-plus-lite:
    - asarray
    - as-cl-array
    - ones
    - ones-like
    - zeros
    - zeros-like
    - rand
    - rand-like
    - transpose
    - \*element-type-alist\*

- dense-arrays-plus-
    - uint32
    - uint8
    - int32
    - shape
    - size

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
DENSE-ARRAYS-DEMO> (make-array '(2 10))
#<DENSE-ARRAYS:ARRAY T 2x10
   (0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0)
 {103C4A3643}>
DENSE-ARRAYS-DEMO> (make-array '(2 100)) ; thanks to *print-length*
#<DENSE-ARRAYS:ARRAY T 2x100
   (0 0 0 0 0 0 0 0 0 0 ...)
   (0 0 0 0 0 0 0 0 0 0 ...)
 {1042B69C93}>
DENSE-ARRAYS-DEMO> (describe (make-array '(2 10))) ; may change
#<DENSE-ARRAYS:ARRAY T 2x10 {1043053683}>
  [structure-object]

Slots with :INSTANCE allocation:
  DISPLACED-TO                   = #(0 0 0 0 0 0 0 0 0 0 ...)
  ELEMENT-TYPE                   = T
  DIM                            = (2 10)
  STRIDES                        = (10 1)
  OFFSETS                        = (0 0)
  CONTIGUOUS-P                   = T
  TOTAL-SIZE                     = 20
  RANK                           = 2
  ROOT-ARRAY                     = NIL
; No value
DENSE-ARRAYS-DEMO> (defparameter a (make-array '(4 10) :constructor #'+))
A
DENSE-ARRAYS-DEMO> (print-array a "~2d")
#<DENSE-ARRAYS:ARRAY T 4x10
   ( 0  1  2  3  4  5  6  7  8  9)
   ( 1  2  3  4  5  6  7  8  9 10)
   ( 2  3  4  5  6  7  8  9 10 11)
   ( 3  4  5  6  7  8  9 10 11 12)
 {103CEB6BC3}>
 ```

**Slicing facilities**

```lisp
DENSE-ARRAYS-DEMO> (aref a nil 1) ; The view indicates that mutating this array would change another original array*
#<DENSE-ARRAYS:ARRAY (VIEW) T 4
   1 2 3 4
 {103D417A13}>
DENSE-ARRAYS-DEMO> (aref a nil -1) ; negative indices start from the end
#<DENSE-ARRAYS:ARRAY (VIEW) T 4
   9 10 11 12
 {1020A92133}>

DENSE-ARRAYS-DEMO> (aref a 1)
#<DENSE-ARRAYS:ARRAY (VIEW) T 10
   1 2 3 4 5 6 7 8 9 10
 {103D426BB3}>
DENSE-ARRAYS-DEMO> (aref a '(1 :end 3) '(1 :end 3))
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x2
   (2 3)
   (3 4)
 {103FE68543}>
DENSE-ARRAYS-DEMO> (defparameter b (aref a '(1 :end 3) '(1 :end 8 :step 2)))
B
DENSE-ARRAYS-DEMO> b
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x4
   (2 4 6 8)
   (3 5 7 9)
 {1011617503}>
DENSE-ARRAYS-DEMO> (aref b (make-array '(2 4) :initial-contents '((0 1 0 0) (1 1 0 0))
                                       :element-type 'bit))
#<DENSE-ARRAYS:ARRAY T 3
   4 3 5
 {1013D6FE23}>
DENSE-ARRAYS-DEMO> (setf (aref b (make-array '(2 4)
                                             :initial-contents '((0 1 0 0) (1 1 0 0))
                                             :element-type 'bit))
                         0)
0
DENSE-ARRAYS-DEMO> b
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x4
   (2 0 6 8)
   (0 0 7 9)
 {1011617503}>
DENSE-ARRAYS-DEMO> a
#<DENSE-ARRAYS:ARRAY T 4x10
   (0 1 2 3 4 5 6 7 8 9)
   (1 2 3 0 5 6 7 8 9 10)
   (2 0 4 0 6 7 8 9 10 11)
   (3 4 5 6 7 8 9 10 11 12)
 {10115A1073}>
```

Planned for the future: Using SIMD operations wherever possible.

Tests are also littered throughout out the system and may serve as examples, for instance [plus/py4cl2.lisp](plus/py4cl2.lisp).

*The semantics do feel debatable: mutating the original array would also mutate the views 🤷‍♂️.

# Usage

1. Clone into `$QUICKLISP_HOME/local-projects`. (See `ql:*local-project-directories*`.)
2. `(ql:quickload "dense-arrays")` - or dense-arrays-plus or dense-arrays-plus-lite
3. Optionally: `(asdf:test-system "dense-arrays")`- or dense-arrays-plus or dense-arrays-plus-lite

Feel free to raise an issue!
