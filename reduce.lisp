
(in-package #:lazyseq)

(defun reduces (f init s)
  (loop
     for cell = s then (tail cell)
     and acc = init then (funcall f acc (head cell))
     when (not cell) do (return acc)))

(example
 (reduces #'+ 0 (range 1 11)) => 55)

(defun reductions (f init s)
  (loop
     for cell = s then (tail cell)
     and acc = init then (funcall f acc (head cell))
     collecting acc into accs
     when (not cell) do (return accs)))

(example
 (reductions #'+ 0 (range 1 6))
 => '(0 1 3 6 10 15))


(defun flush-seq (s)
  "Flushes the sequences S so that it is completely
computed. Returns T if some computation was perfomed,
 otherwise NIL.

CAUTION: similarly to reductions FLUSH-SEQ should not
be applied on infinite sequences."
  (if s
      (loop
         for cell = s then (tail cell)
         when (not cell) do (return nil))
      nil))

(defun any (f s)
  "Returns T if unary function F returns true for any of sequence S.
CAUTION: This may never return if applied to an infinite sequence"
  (if s
      (loop
         for cell = s then (tail cell)
         when (not cell) do (return nil)
         when (funcall f (head cell)) do (return t))
      nil))

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

(example
 (all #'evenp '(2 4 6 8))
 => t)

(example 
 (all #'evenp '(2 4 6 8 9))
 => nil)

