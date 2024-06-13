
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

(example
 (take 10 (lazy-bind (iterate #'1+ 1)
		     (lambda (x) (lazy-pure (* x x)))))
 => '(1 4 9 16 25))

(example
 (take 8 (lazy-bind
	  '(1 2 3 4)
	  (lambda (x) (lazy-bind
		       '(A B)
		       (lambda (y) (lazy-pure (cons x y)))))))
  => '((1 . A) (1 . B) (2 . A) (2 . B) (3 . A) (3 . B) (4 . A) (4 . B)))


(example
 (take 8 (lazy-bind
	  (iterate #'1+ 1)
	  (lambda (x) (lazy-bind
		       '(A B)
		       (lambda (y) (lazy-pure (cons x y)))))))
 => 
)
