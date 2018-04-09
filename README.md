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

Standard functions for operating on lazy sequences are provided,
including `take`, `drop` 

## Examples

The Fibonacci sequence can be defined using `lazy-seq` as

```lisp
(defun fib (&optional (a 0) (b 1))
  "Lazily calculate the Fibonacci sequence"
  (lazy-seq
   (cons b (fib b (+ a b)))))
```

or using `map` and the anaphoric macro `alazy-list*`:

```lisp
(alazy-list* 1 1 (maps #'+ self (tail self)))
```

or using `scanl`:

```lisp
(alazy-list* 1 (scanl #'+ 1 self))
```

A quick comparison (with SBCL) indicates that these three all take approximately the same
CPU and memory resources.
