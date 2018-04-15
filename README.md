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

Functions for creating sequences include `lazy-seq`, `lazy-list`,
`cycle`, `iterate`, `repeat`, `repeatedly`, `range` and `lazy-sort`. 

Functions for operating on lazy sequences include `maps`, `filters`,
`reductions`, `lazy-cat`.

Functions which evaluate lazy sequences include `seq-elt`, `take`, `take-while`, 
`take-all`, `drop`, `drop-while`, `flush-seq`, `reduces`, `any` and `all`.

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

or, more efficiently, use an object representing a range:

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

Prime numbers can be generated using the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes):

```lisp
(defun sieve (s)
  (cons (head s)
        (lazy-seq (sieve (filters (lambda (n) (not (= 0 (mod n (head s)))))
                                  (tail s))))))
                                  
(take 10 (sieve (iterate #'1+ 2)))
=> (2 3 5 7 11 13 17 19 23 29)
```

or a more efficient alternative which shouldn't exhaust the stack:

```lisp
(defparameter primes
  (alazy-list* 2 3 5 (filters (lambda (n)
                                (all (lambda (d) (not (zerop (mod n d))))
                                     (take-while (lambda (x) (<= (* x x) n))
                                                 self)))
                              (range 7 nil 2))))

(seq-elt primes 1000000)
=> 15485863
```

### Lazy sorting

If only part of a sequence needs to be fully sorted, for example
the highest 5 values from a list of 1000, then lazy sorting can
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

;; The start of sorted-nums is now fully sorted but most is only
;; partly sorted i.e. known to be <= 9.961885
sorted-nums
=> #<lazy:9.998831 9.997225 9.989832 9.987843 9.98232 9.961885 ...>
```

