
(in-package #:lazyseq)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defgeneric head (sequence)
  (:documentation "Taking the head of SEQUENCE."))

(defmethod head ((c list))
  (first c))

(defgeneric tail (sequence)
  (:documentation "Taking the tail of SEQUENCE."))

(defmethod tail ((c list))
  (rest c))

(defgeneric print-cell (c out)
  (:documentation "Priting cell C to OUT."))

(defmethod print-cell ((c list) out)
  (when c
    (format out "~A" (first c))
    (when (rest l)
      (format out " ")
      (print-cell (rest l) out))))

(defstruct lazy-cell
  "The cell representation of lazy sequences.

When GENFN is NIL then the cell is said *computed*.
In this state, the HD slot contains the computed head
of the sequence, and TL is the tail sequence.

When GENFN is non-NIL, then it is i an argument-less 
*generator function* explaining how to compute the head
 and tail. In this state, both the HD and TL slots are
NIL."
  (hd nil)
  (tl nil)
  (genfn nil))

(defun compute-lazy-cell (c)
  "Compute the head and tail of the lazy cell C.

CAUTION: this function should only be called
in a non-computed state (i.e. GENFN must be non-NIL.

"
  (let ((val (funcall (lazy-cell-genfn c))))
    (setf (lazy-cell-hd c) (head val))
    (setf (lazy-cell-tl c) (tail val))
    (setf (lazy-cell-genfn c) nil)))

(defmethod head ((c lazy-cell))
  (when (lazy-cell-genfn c)
    (compute-lazy-cell c))
  (lazy-cell-hd c))

(defmethod tail ((c lazy-cell))
  (when (lazy-cell-genfn c)
    (compute-lazy-cell c))
  (lazy-cell-tl c))

(defmethod print-cell ((c lazy-cell) out)
  (if (lazy-cell-genfn c)
      (format out "?")
      (progn (format out "~A" (lazy-cell-hd c))
	     (when (lazy-cell-tl c)
	       (format out " ")
	       (print-cell (lazy-cell-tl c) out)))))

(defmethod print-object ((c lazy-cell) out)
  (format out "#<lazy:") (print-cell c out) (format out ">"))

(defmacro lazy-seq (expr)
  "Build a lazy sequence out of expression EXPR.

The EXPR should construct a sequence with a computed
 value as head, and a (possibly lazy) sequence as tail.

An exemple of usage is as follows:

(defun nats (n)
  (lazy-seq (cons n (nats (1+ n)))))

"
`(make-lazy-cell :hd nil :tl nil :genfn #'(lambda () ,expr)))


(example-progn
 (defun nats (n)
   (lazy-seq (cons n (nats (1+ n)))))

 (example (head (tail (tail (nats 1)))) => 3))


(defun take (n s)
  "Returns the list of the N first elements of the sequence S."
  (declare (type fixnum n))
  (loop repeat n
     for cell = s then (tail cell)
     when cell collect (head cell)))


(example (take 5 (nats 1)) => '(1 2 3 4 5))

(defun drop (n s)
  "Drops the first N elements of sequence S."
  (declare (type fixnum n))
  (loop repeat n
     for cell = s then (tail cell)
     when (not cell) do (return cell)
     finally (return cell)))

(example
 (take 5 (drop 10000 (nats 1)))
 => '(10000 10001 10002 10003 10004))


  
