
#|

   Lazy comprehensions

   This is from: https://github.com/fredokun/lisp-list-comprehensions

|#

(in-package #:lazyseq)

;; Seq[a] * (a -> Seq[b]) -> Seq[b]
(defun lazy-bind (s f)
  "The monadic bind operator for lazy sequences, here apply `F` on `S`."
  (lazy-catmap f s))

;; a -> Seq[a]
(defun lazy-pure (x)
  "The monadic pure operator, a sequence containing only `S`."
  (list x))

(example
 (take 10 (lazy-bind '(1 2 3 4 5)
		     (lambda (x) (lazy-pure (* x x)))))
 => '(1 4 9 16 25))



 

