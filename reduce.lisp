
(in-package #:lazyseq)

(defun reduces (f init s)
  "Reduction of sequence S using function F with two arguments: 
(F <ACC> <ELEMENT>) produces for current <ELEMENT> 
 a new accumulator value from preceding value ACC"
  (loop
     :for cell = s :then (tail cell)
     :and acc = init :then (if (emptyp cell)
			       acc
			       (funcall f acc (head cell)))
     :when (emptyp cell) do (return acc)))

(example
 (reduces #'+ 0 (range 1 11)) => 55)

(defun reductions (f init s)
  "The same as REDUCES but keep all intermediate values of
accumulator."
  (loop
     :for cell = s :then (tail cell)
     :and acc = init :then (if (emptyp cell)
			       acc
			       (funcall f acc (head cell)))
     collecting acc into accs
     when (emptyp cell) do (return accs)))

(example
 (reductions #'+ 0 (range 1 6))
 => '(0 1 3 6 10 15))


(defun flush-seq (s)
  "Flushes the sequences S so that it is completely
computed. Returns T if some computation was perfomed,
 otherwise NIL.

CAUTION: similarly to reductions FLUSH-SEQ should not
be applied on infinite sequences."
  (if (emptyp s)
      nil
      (loop
         for cell = s then (tail cell)
         when (emptyp cell) do (return t))))

(examples
 (flush-seq nil) => nil
 (flush-seq '(1 2 3)) => t
 )

(defun any (f s)
  "Returns T if unary function F returns true for any of sequence S.
CAUTION: This may never return if applied to an infinite sequence"
  (if (emptyp s)
      nil
      (loop
         :for cell = s :then (tail cell)
         :when (emptyp cell) :do (return nil)
         :when (funcall f (head cell)) :do (return t))))

(example
 (any #'evenp (nats 1))
 => t)

(example 
 (any #'evenp '(1 3 5))
 => nil)

(defun all (f s)
  "Returns T if unary function F returns true for all of sequence S.
CAUTION: This may never return if applied to an infinite sequence"
  (if s
      (loop
         for cell = s then (tail cell)
         when (not cell) do (return t)
         when (not (funcall f (head cell))) do (return nil))
      nil))

(examples
 (all #'evenp '(2 4 6 8))
 => t

 (all #'evenp '(2 4 6 8 9))
 => nil)

