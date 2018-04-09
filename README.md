# lisp-lazy-seq

Lazy sequences in Common Lisp. Mostly inspired by 
[Clojure's lazy sequences](http://clojure-doc.org/articles/language/laziness.html), with 
some features of [Heresy](http://cl-heresy.sourceforge.net/Heresy.htm) and Haskell.



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
