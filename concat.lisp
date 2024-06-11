
(in-package #:lazyseq)

(defstruct cat-cell
  "The representation of a cell for a (lazy) concatenating sequences.

The LEFT slot is the first sequence of the concat, and RIGHT is
 a list of the next sequences."
  (left nil)
  (right nil))

(defmethod head ((c cat-cell))
  (head (cat-cell-left c)))

(defmethod tail ((c cat-cell))
  (if (cat-cell-left c)
     (let ((ltl (tail (cat-cell-left c))))
       (cond (ltl (make-cat-cell :left ltl :right (cat-cell-right c)))
             ((cat-cell-right c)
              (if (rest (cat-cell-right c))
                  (make-cat-cell :left (first (cat-cell-right c))
                                 :right (rest (cat-cell-right c)))
                  (first (cat-cell-right c))))
             (t nil)))
     nil))

(defmethod print-cell ((c cat-cell) out)
  (print-cell (cat-cell-left c) out)
  (when (cat-cell-right c)
    (format out " @ ~A ..." (first (cat-cell-right c)))))

(defmethod print-object ((c cat-cell) out)
  (format out "#<cat:")
  (print-cell c out)
  (format out ">"))

(defmacro lazy-cat (s1 s2 &rest rs)
  "Lazy concatenation of the sequences S1, followed
 by S2 and then every sequences in RS, if any."
  `(make-cat-cell :left ,s1 :right (cons ,s2 ,rs)))

(example
 (take 4 (lazy-cat (lazy-list 1 2 3) (lazy-list 4 5 6)))
 => '(1 2 3 4))

(example
 (take 10 (lazy-cat (lazy-list 1 2 3) (iterate #'1+ 4)))
 => '(1 2 3 4 5 6 7 8 9 10))

(example
 ;; of course it's stupid ... but it should work
 (take 10 (lazy-cat (iterate #'1+ 1) (lazy-list 1 2 3)))
 => '(1 2 3 4 5 6 7 8 9 10))


(defun cycle (s)
  "Produces a lazy infinite sequence consisting
in the repetition of the elements of sequence S."
  (lazy-cat s (lazy-seq (cycle s))))

(example
 (take 10 (cycle (lazy-list 1 2 3)))
 => '(1 2 3 1 2 3 1 2 3 1))


