# lisp-lazy-seq

Lazy sequences in Common Lisp

These package implements the notion of a lazy sequence as found
in the Clojure programming language.

Note that everything has been implemented from scratch, without
any claim of compatibility.

## Getting started

The package is implemented as an ASDF system named `lazyseq`

It is available on ultralisp :

```lisp
* (ql:quickload "lazyseq")
```

The main package is `:lazyseq`

For example:

```lisp
* (in-package :lazyseq)
#<PACKAGE "LAZYSEQ">

* (defun nats (n)
    (lazy-seq (cons n (nats (1+ n)))))
NATS

* (head (tail (tail (nats 1))))
3

* (take 5 (nats 1))
(1 2 3 4 5)
```

More examples are available in the files: 

 - [lisp-lazy-seq.lisp](https://github.com/fredokun/lisp-lazy-seq/blob/master/lisp-lazy-seq.lisp) (commented lisp)
 - [lisp-lazy-seq.pdf](https://github.com/fredokun/lisp-lazy-seq/blob/master/lisp-lazy-seq.pdf) (pdf)
 - [lisp-lazy-seq.ipynb](https://github.com/fredokun/lisp-lazy-seq/blob/master/lisp-lazy-seq.ipynb) (cl-jupyter notebook)
 

## License

Copyright (C) 2016- Frederic Peschanski under the MIT License
(cf. `LICENSE` file)
