(in-package #:lazyseq)

(defun lazy-sort-next (ls pred remainder)
  "Lazily sort list LS so that the next element is at the front. 
Predicate PRED should return non-NIL if ARG1 is to precede ARG2.

Note: The result is a mix of cons cells and lazy-seq cells
  "
  (let ((pivot (first ls))
        before-pivot
        after-pivot)
    (dolist (elem (rest ls))
      (if (funcall pred elem pivot)
          (push elem before-pivot)
          (push elem after-pivot)))
    ;; Now have partitioned into [before-pivot : pivot : after-pivot]
    (if (not before-pivot)
        (if (not after-pivot)
            (cons pivot remainder) ; ( (a) rem ) => (cons a rem)
            (cons pivot
                  (lazy-seq
                   (lazy-sort-next
                    after-pivot pred remainder)))) ; ( (a b) rem ) => (cons a (sort b rem))
        
        ;; Partially sorted, but before-pivot not empty.
        ;; Keep sorting to find the first element
        (if (not after-pivot)
            ;; pivot is the last element
            (lazy-sort-next before-pivot pred (cons pivot remainder)) ; ( (b a) rem ) => (sort b (cons a rem))
            ;; Have elements before and after pivot: ( (b a c) rem) => (sort b (cons a (sort c)))
            (lazy-sort-next before-pivot pred (cons pivot
                                                    (lazy-seq
                                                     (lazy-sort-next
                                                      after-pivot pred remainder))))))))

        
(defun lazy-sort (s pred)
  "Lazily sort sequence S using the Quicksort algorithm. 
Predicate PRED should return non-NIL if ARG1 is to precede ARG2.

The result is a mix of cons cells and lazy-seq cells. Only the part
of the result which is needed is sorted completely. This can be faster
than SORT if only a small number of elements is needed.

CAUTION: This forces evaluation of the whole sequence S, so
will not return if given an infinite sequence.

Idea from Clojure code by Ben Ashford: 
http://benashford.github.io/blog/2014/03/22/the-power-of-lazy-sequences/
"
  ;; Start sorting, ensuring that S is a list
  (when s ; Catch the empty list case
    (lazy-seq (lazy-sort-next (take-all s) pred nil))))

(example
 (take-all (lazy-sort nil #'>))
 => nil)

(example
 (take 10 (lazy-sort '(1) #'>))
 => '(1))

(example
 (take 10 (lazy-sort '(1 2) #'<))
 => '(1 2))

(example
 (take 10 (lazy-sort '(1 2) #'>))
 => '(2 1))

(example
 (take 10 (lazy-sort '(2 1 3) #'>))
 => '(3 2 1))

(example
 (take 10 (lazy-sort '(5 4 3 2 1 6 7 8 9) #'<))
 => '(1 2 3 4 5 6 7 8 9))

