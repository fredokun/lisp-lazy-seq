
(in-package #:lazyseq)

(defun map1 (f s)
  "Maps the (unary) function F over sequence S."
  (when s
    (lazy-seq (cons (funcall f (head s))
		    (map1 f (tail s))))))

(example
 (take 5 (map1 (lambda (x) (* x x)) (range)))
 => '(0 1 4 9 16))

(defun map2 (f s1 s2)
  "Maps the (binary) function F over sequences S1 and S2."

  (when (and s1 s2)
    (lazy-seq (cons (funcall f (head s1) (head s2))
		    (map2 f (tail s1) (tail s2))))))

(example
 (take 5 (map2 #'* (range) (repeat 2)))
 => '(0 2 4 6 8))


(defun mapn (f &rest seqs)
  "Maps the (n-ary) function F over the sequences SEQS."
  (when (every (lambda (s) s) seqs)
    (lazy-seq (cons (apply f (mapcar #'head seqs))
		    (apply #'mapn (cons f (mapcar #'tail seqs)))))))


(example
 (take 5 (mapn (lambda (x) (* x x)) (range)))
 => '(0 1 4 9 16))

(example
 (take 5 (mapn #'* (range) (repeat 2)))
 => '(0 2 4 6 8))

(example
 (take 5 (mapn (lambda (x y z u) (+ x y z u))
	       (range)
	       (range 10)
	       (range 100)
	       (range 1000)))
 => '(1110 1114 1118 1122 1126))

(defun maps (f s &rest seqs)
  "Maps the function F over the sequences S (and SEQS)."
  (if seqs
      (if (rest seqs)
	  (apply #'mapn f (cons s seqs))
	  (map2 f s (first seqs)))
      (map1 f s)))

(example
 (take 5 (maps (lambda (x) (* x x)) (range)))
 => '(0 1 4 9 16))


(example
 (take 5 (maps #'* (range) (repeat 2)))
 => '(0 2 4 6 8))

(example
 (take 5 (maps (lambda (x y z u) (+ x y z u))
	       (range)
	       (range 10)
	       (range 100)
	       (range 1000)))
 => '(1110 1114 1118 1122 1126))

(defun filters (pred s)
  "Filters sequence S for predicate PRED."
  (when s
    (if (funcall pred (head s))
	(lazy-seq (cons (head s) (filters pred (tail s))))
	(filters pred (tail s)))))

(example
 (take 5 (filters #'evenp (range)))
 => '(0 2 4 6 8))

(example
 (take 5 (filters #'oddp (range)))
 => '(1 3 5 7 9))

(defun scanl (f a s)
  "Applies binary function F to A and the first element of the sequence S
   then to this result and the second element, and so on. 
   Returns a lazy sequence [A (F A S[0]) (F (F A S[0]) S[1]) ...]"
  (if s
      (lazy-seq (cons a (scanl f (funcall f a (head s)) (tail s))))
      (list a)))

(example
 (take 10 (scanl #'+ 1 '(1 2 3)))
 => '(1 2 4 7))



