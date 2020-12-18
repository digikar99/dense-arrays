# Status

[Last update: 07th December 2020]

- Under Construction:
  - The API may change
  - The package and system names may change. Therefore, please use package-local-nicknames to avoid global symbol replacements.
  - Yet to be optimized for speed and user-facing debugging
  - Yet to incorporate SIMD
  - Semantics of displaced-indexed-offset

# What

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
DENSE-ARRAYS-DEMO> (aref a 1)
#<DENSE-ARRAYS:ARRAY (VIEW) T 10
   1 2 3 4 5 6 7 8 9 10
 {103D426BB3}>
DENSE-ARRAYS-DEMO> (aref a '(1 :stop 3) '(1 :stop 3))
#<DENSE-ARRAYS:ARRAY (VIEW) T 2x2
   (2 3)
   (3 4)
 {103FE68543}>
DENSE-ARRAYS-DEMO> (defparameter b (aref a '(1 :stop 3) '(1 :stop 8 :step 2)))
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

*The semantics do feel debatable: mutating the original array would also mutate the views 🤷‍♂️.


# Why

Common Lisp arrays provide no fast way to do this, amongst other things:

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

# Usage

1. Clone into `$QUICKLISP_HOME/local-projects`. (See `ql:*local-project-directories*`.)
2. `(ql:quickload "dense-arrays")`
3. Optionally: `(5am:run :dense-arrays)`

Feel free to raise an issue if there's an issue!
