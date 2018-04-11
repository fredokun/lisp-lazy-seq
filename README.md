# lisp-lazy-seq

Lazy sequences in Common Lisp. Mostly inspired by 
[Clojure's lazy sequences](http://clojure-doc.org/articles/language/laziness.html), with 
some features of [Heresy](http://cl-heresy.sourceforge.net/Heresy.htm) and Haskell.

CLOS generic methods are used to provide an extensible
library of functions and macros for operating on sequences. Lazy
evaluation is done using structures which can be in either a computed
state with a head and a tail like a standard cons cell, or in a
not-yet-computed state containing a generator function, usually a
lambda closure. This is essentially the method used in Scheme streams.
See the manual in [notebook](lisp-lazy-seq.ipynb) or
[pdf](lisp-lazy-seq.pdf) form for details of the internals. 

Functions for creating sequences include `cycle`, `iterate`, `repeat`, 
`repeatedly`, `range` and `lazy-sort`. 

Functions for operating on lazy sequences include `maps`, `filters`,
`reduces`, `reductions`, `any`, `all`, `lazy-cat`
(concatenation), `drop` and `drop-while`.

Functions to force evaluation of sequences include `take`, `take-while`, 
`take-all` and `flush-seq`.

## Getting it

If you have Quicklisp installed then clone into your `local-projects`
directory

```bash
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/fredokun/lisp-lazy-seq.git 
```

then in your favourite lisp implementation

```lisp
(ql:quickload :lazyseq)

(use-package :lazyseq)
```

## Examples

### Infinite sequences

The natural numbers `1 2 3 ...` can be generated lazily with 
the `lazy-seq` macro. This delays evaluation of a given expression
until it is needed.

```lisp
(defun nats (n)
  (lazy-seq
   (cons n (nats (1+ n)))))
```

Alternatively we can iterate the function `1+`

```lisp
(iterate #'1+ 1)
```

use a recursive list

```lisp
(alazy-list* 1 (maps #'1+ self))
```

or more efficiently use an object representing a range:

```lisp
(range 1)
```

The Fibonacci sequence can be defined using `lazy-seq` as

```lisp
(defun fib (&optional (a 0) (b 1))
  "Lazily calculate the Fibonacci sequence"
  (lazy-seq
   (cons b (fib b (+ a b)))))
```

or using `maps` and the anaphoric macro `alazy-list*`:

```lisp
(alazy-list* 1 1 (maps #'+ self (tail self)))
```

or using `scanl`:

```lisp
(alazy-list* 1 (scanl #'+ 1 self))
```

Mutually recursive sequences can also be defined

```lisp
(lazy-labels ((evens (lazy-list* 0 (maps #'1+ odds)))
              (odds (maps #'1+ evens)))
    (take 4 odds))
 => '(1 3 5 7))
```

### Lazy sorting

If only part of a sequence needs to be fully sorted, for example
the highest 10 values from a list of 1000, then lazy sorting can
be significantly faster than sorting the entire list.

```lisp
;; Generate a list of 1000 random numbers
(defparameter nums
  (take 1000 (repeatedly
              (lambda () (random 10.0)))))

;; Create a lazy sequence representing the sorted list
(defparameter sorted-nums (lazy-sort nums #'>))
=> #<lazy:...>

(take 5 sorted-nums)
=> (9.998831 9.997225 9.989832 9.987843 9.98232)

;; The start of sorted-nums is now fully sorted
sorted-nums
=> #<lazy:9.998831 9.997225 9.989832 9.987843 9.98232 9.961885 ...>
```

