
(in-package #:lazyseq)

(defstruct cat-cell
  "The representation 1of a cell for a (lazy) concatenating sequences.

The LEFT slot is the first sequence of the concat, and RIGHT is
 a list of the next sequences."
  (left nil)
  (rights nil))

(defmethod head ((c cat-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (if (emptyp (cat-cell-left c))
	  (head (first (cat-cell-rights c)))
	  (head (cat-cell-left c)))))

(defun find-non-emptyp (seqs)
  (if seqs
      (if (emptyp (first seqs))
	  (first-non-emptyp (rest seqs))
	  seqs)
      nil))
	  
(defmethod tail ((c cat-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (if (emptyp (cat-cell-left c))
	  (let ((seqs (find-non-emptyp (cat-cell-rights c))))
	    (if seqs
		(let ((nleft (tail (first seqs)))
		      (nrights (rest seqs)))
		  (if nrights
		      (make-cat-cell :left nleft :rights nrights)
		      nleft))
		;; all empty
		(error "Empty sequence")))
	  ;; something on the left
	  (let ((ltl (tail (cat-cell-left c))))
	    (if (emptyp ltl)
		(let ((rights (cat-cell-rights c)))
		  (if rights
		      (make-cat-cell :left (first rights) :rights (rest rights))
		      ;; nothing on the left and on the right
		      (list))) 
		;; something's left on the left
		(make-cat-cell :left ltl :rights (cat-cell-rights c)))))))

(defmethod emptyp ((c cat-cell))
  (and (emptyp (cat-cell-left c))
       (not (cat-cell-rights c))))

(defmethod print-cell ((c cat-cell) out)
  (if (emptyp c)
      (format out "<end>")
      (progn (print-cell (cat-cell-left c) out)
	     (when (cat-cell-rights c)
	       (format out " @ ~A ...")
	       (print-cell (first (cat-cell-rights c)))))))


(defmethod print-object ((c cat-cell) out)
  (format out "#<cat:")
  (print-cell c out)
  (format out ">"))

(defmacro lazy-cat (s1 s2 &rest rs)
  "Lazy concatenation of the sequences S1, followed
 by S2 and then every sequences in RS, if any."
  `(make-cat-cell :left ,s1 :rights (cons ,s2 ,rs)))


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


(defun lazy-catmap (f s)
  "Apply function `F : a -> Seq[b]` on each element of `S : Seq[a]`.
  The resulting sequences are lazily concatenated (using `LAZY-CAT`)."
  (if (emptyp s)
      s
      (if s 
	  (lazy-cat (lazy-seq (funcall f (head s))) (lazy-seq (lazy-catmap f (tail s))))
	  s)))

(comment

(defparameter s1 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (lazy-list 1 2 3)))

s1  ; => #<cat:... @ #<lazy:...> ...>
(defparameter v1 (head s1))
v1  ; => 1 (1 bit, #x1, #o1, #b1)
s1  ; => #<cat:1... @ #<lazy:...> ...>

(defparameter s2 (tail s1))
s2 ; => #<cat:... @ #<lazy:...> ...>
(defparameter v2 (head s2))
v2 ; => 10 (4 bits, #xA, #o12, #b1010)
s2 ; => #<cat:10 @ #<lazy:...> ...>

(defparameter s3 (tail s2))
s3 ; => #<cat:...>
(defparameter v3 (head s3))
v3 ; => 2 (2 bits, #x2, #o2, #b10)
s3

(defparameter s4 (tail s3))
s4 ; => #<cat:... @ #<lazy:...> ...>
(defparameter v4 (head s4))
v4 ; => 20 (5 bits, #x14, #o24, #b10100)
s4 ; => #<cat:20 @ #<lazy:...> ...>

(defparameter s5 (tail s4))
s5 ; => #<cat:...>
(defparameter v5 (head s5))
v5 ; => 3 (2 bits, #x3, #o3, #b11)
s5 ; => #<CAT-CELL <<error printing object>> {10037883A3}>

(defparameter s6 (tail s5))
s6 ; => #<cat:... @ #<lazy:...> ...>
(defparameter v6 (head s6))
v6

(defparameter s7 (tail s6))
s7  ; => #<cat:...>



(type-of s7)
(type-of (cat-cell-left s7))
(emptyp (cat-cell-left s7))

(cat-cell-left (cat-cell-left (cat-cell-left s7)))

(emptyp s7)


(defparameter v7 (head s7))  ;; ! empty
(tail s7) ;; ! empty


(type-of s7)
(cat-cel-left s7)




(example
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (lazy-list 1 2 3)))
  => '(1 10 2 20 3 30))

(example
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (iterate #'1+ 1)))


(defparameter s1 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
			      (list 1 2 3 4)))

(defparameter f (lambda (x) (lazy-list x (* x 10))))

(defparameter s1 (lazy-cat (lazy-seq (funcall f (head s))

(defparameter s1 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (iterate #'1+ 1)))



)


