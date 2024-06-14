(in-package #:lazyseq)

(defstruct cat-cell
  "The representation 1of a cell for a (lazy) concatenating sequences.

The LEFT slot is the first sequence of the concat, and RIGHT is
 a list of the next sequences."
  (left nil)
  (right nil))

(defun cat-cell-normalize (c)
  (cond ((emptyp (cat-cell-left c)) (cat-cell-right c))
        ((emptyp (cat-cell-right c)) (cat-cell-left c))
        (t c)))

(defmethod head ((c cat-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (head (cat-cell-left c))))

(defmethod tail ((c cat-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (let ((left (tail (cat-cell-left c))))
        (cat-cell-normalize (make-cat-cell :left left :right (cat-cell-right c))))))

(defmethod emptyp ((c cat-cell))
  (and (emptyp (cat-cell-left c))
       (emptyp (cat-cell-right c))))

(defmethod print-cell ((c cat-cell) out)
  (progn (print-cell (cat-cell-left c) out)
         (format out " @ ")
         (print-cell (cat-cell-right c) out)))

(defmethod print-object ((c cat-cell) out)
  (format out "#<cat:")
  (print-cell c out)
  (format out ">"))

(defun debug-cat-cell (c &key (out t))
  (format out "cat-cell{~%  left=~A~%  right=~A~%" (cat-cell-left c) (cat-cell-right c)))

(defmacro lazy-cat (s1 s2 &rest rs)
  "Lazy concatenation of the sequences S1, followed
 by S2 and then every sequences in RS, if any."
  (if rs
      `(lazy-cat ,s1 (lazy-cat ,s2 ,@rs))
      `(cat-cell-normalize (make-cat-cell :left ,s1 :right ,s2))))

(examples
 (take 4 (lazy-cat (lazy-list 1 2 3) (lazy-list 4 5 6)))
 => '(1 2 3 4)

 (take 10 (lazy-cat (lazy-list 1 2 3) (iterate #'1+ 4)))
 => '(1 2 3 4 5 6 7 8 9 10)

 ;; of course it's stupid ... but it should work
 (take 10 (lazy-cat (iterate #'1+ 1) (lazy-list 1 2 3)))
 => '(1 2 3 4 5 6 7 8 9 10))

(example
 ;; with more than 2 sub-sequences
 (take 10 (lazy-cat (list 1 2 3) (lazy-list (+ 2 2) (+ 2 3) (+ 3 3)) (iterate #'1+ 7)))
 => '(1 2 3 4 5 6 7 8 9 10))

(defun cycle (s)
  "Produces a lazy infinite sequence consisting
in the repetition of the elements of sequence S."
  (lazy-cat s (lazy-seq (cycle s))))

(example
 (take 10 (cycle (lazy-list 1 2 3)))
 => '(1 2 3 1 2 3 1 2 3 1))

(defstruct catmap-cell
  (fun nil)
  (left nil)
  (right nil))

(defun catmap-cell-normalize (c)
  (if (emptyp (catmap-cell-left c))
      (if (emptyp (catmap-cell-right c))
	  (catmap-cell-left c)
	  (loop :for r = (catmap-cell-right c) :then (tail r)
		:for nleft = (if (emptyp r) nil (funcall (catmap-cell-fun c) (head r)))
		;;:do (format t "r = ~A, nleft=~A~%" r nleft)
		:when (emptyp r) :do (return r)
		:when (not (emptyp nleft))
		  :do (return (progn (setf (catmap-cell-left c) nleft)
				     (setf (catmap-cell-right c) (tail r))
				     c))))
      ;; left is not empty, nothing to normalize
      c))

(defmethod head ((c catmap-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (head (catmap-cell-left c))))

(defmethod tail ((c catmap-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (catmap-cell-normalize 
       (make-catmap-cell :fun (catmap-cell-fun c)
			 :left (tail (catmap-cell-left c))
			 :right (catmap-cell-right c)))))

(defmethod emptyp ((c catmap-cell))
  (emptyp (catmap-cell-left c)))

(defmethod print-cell ((c catmap-cell) out)
  (progn (print-cell (catmap-cell-left c) out)
         (format out " >>= ... ")))

(defmethod print-object ((c catmap-cell) out)
  (format out "#<catmap:")
  (print-cell c out)
  (format out ">"))


(defmacro lazy-catmap (f s)
  "Apply function `F : a -> Seq[b]` on each element of `S : Seq[a]`.
  The resulting sequences are lazily concatenated (using `LAZY-CAT`)."
  `(catmap-cell-normalize (make-catmap-cell :fun ,f :left nil :right ,s)))

(examples
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (lazy-list 1 2 3)))
 => '(1 10 2 20 3 30)
 
 (take 10 (lazy-catmap (lambda (x) (lazy-list x (* x 10)))
		       (iterate #'1+ 1)))
 => '(1 10 2 20 3 30 4 40 5 50))




