
(in-package #:lazyseq-test)

(in-suite basic)

(defun nats (n)
  (lazy-seq (cons n (nats (1+ n)))))

(test take-nats
      (is (equal (take 5 (nats 1)) '(1 2 3 4 5))))

