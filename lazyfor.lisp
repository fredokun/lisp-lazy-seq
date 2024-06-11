
#|

   Lazy comprehensions

   This is from: https://github.com/fredokun/lisp-list-comprehensions

|#

(in-package #:lazyseq)

(defun lazy-catmap (f ll)
  (if (emptyp ll)
      nil
      (lazy-cat (lazy-seq (funcall f (head ll))) (lazy-catmap f (tail ll)))))

(example
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (lazy-list 1 2 3)))




