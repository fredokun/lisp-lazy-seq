
#|

   Lazy comprehensions

   This is from: https://github.com/fredokun/lisp-list-comprehensions

|#

(in-package #:lazyseq)

(defun lazy-catmap (f ll)
  (lazy-cat (funcall f (head ll)) (lazy-catmap f (tail ll))))

(example
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (iterate #'1+ 1)))



