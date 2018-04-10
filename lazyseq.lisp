
(in-package #:lazyseq)

(defgeneric head (sequence)
  (:documentation "Taking the head of SEQUENCE."))

(defmethod head ((c list))
  (first c))

(defmethod head ((c array))
  (row-major-aref c 0))

(example
 (head '(:a :b :c))
 => :a)

(example
 (head "hello")
 => #\h)

(example
 (head #(1 2 3))
 => 1)

(example
 (head #2A((4 3) (2 1)))
 => 4)

(defgeneric tail (sequence)
  (:documentation "Taking the tail of SEQUENCE."))

(defmethod tail ((c list))
  (rest c))

(defmethod tail ((c array))
  "Return a flattened array, displaced to C. 
This avoids copying data."
  (multiple-value-bind (displace-to displace-index) (array-displacement c)
    (make-array (1- (array-total-size c))
                ;; Avoid multiple levels of displacement
                ;; by displacing to the same array as C if C is displaced
                :displaced-to (or displace-to c)
                :displaced-index-offset (if displace-to
                                            (1+ displace-index)
                                            1)
                :element-type (array-element-type c))))

(example
 (tail "hello")
 => "ello")

(example
 (array-displacement (tail (tail "hello")))
 => (values "hello" 2)) ; Displaced to original array

(let ((*example-equal-predicate* #'equalp))
  (example
   (tail #2A((4 3) (2 1)))
   => #(3 2 1))) ; note array flattened, displaced

(defgeneric print-cell (c out)
  (:documentation "Priting cell C to OUT."))

(defmethod print-cell ((c list) out)
  (format out "~{~a~^ ~}" c))

(defmethod print-cell ((c array) out)
  (format out "~{~a~^ ~}" c))

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
  (let ((cell (loop :for cell = c :then (lazy-cell-tl cell)
                 :for sep = "" :then " "
                 :while (and cell (null (lazy-cell-genfn cell)))
                 :do (progn (format out sep)
                            (format out "~A" (lazy-cell-hd cell)))
                 :finally (return cell))))
    (if (and cell (lazy-cell-genfn cell))
        (format out "..."))))

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

(defun take-while (pred s)
  "Returns the list of the prefix elements of sequence S
satisfying the predicate PRED."
  (loop
     for cell = s then (tail cell)
     for hd = (head cell)
     when (or (not cell)
              (not (funcall pred hd)))
     do (return prefix)
     collecting hd into prefix
     finally (return prefix)))

(example
 (take-while (lambda (x) (< x 10)) (nats 1))
 => '(1 2 3 4 5 6 7 8 9))

(defun take-all (s)
  "Returns the list of all elements of the sequence S.

CAUTION: This will never return if given an infinite sequence."
  (loop
     for cell = s then (tail cell)
     when (not cell) do (return result)
     collecting (head cell) into result))


(defun drop (n s)
  "Drops the first N elements of sequence S."
  (declare (type fixnum n))
  (loop repeat (1+ n)
     for cell = s then (tail cell)
     when (not cell) do (return cell)
     finally (return cell)))

(example
 (take 5 (drop 10000 (nats 1)))
 => '(10001 10002 10003 10004 10005))

(defun drop-while (pred s)
  "Drops the prefix elements of sequence S while
they satisfy predicate PRED."
  (loop
     for cell = s then (tail cell)
     when (or (not cell)
              (not (funcall pred (head cell))))
       do (return cell)
     finally (return cell)))

(example
 (take 5 (drop-while (lambda (x) (< x 10)) (nats 1)))
 => '(10 11 12 13 14))

(defun seq-elt (s n)
  "Returns the N-th element of sequence S."
  (head (drop (1- n) s)))

(example
 (seq-elt (nats 1) 30) => 30)




