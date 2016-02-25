
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


  

  

