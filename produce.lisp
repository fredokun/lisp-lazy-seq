
(in-package #:lazyseq)

(defun iterate (f x)
  "Generate the lazy sequence: X (F X) (F (F X)) ..."
  (lazy-seq (cons x (iterate f (funcall f x)))))

(example
 (take 5 (iterate #'1+ 1)) => '(1 2 3 4 5))

(defun repeat (x)
  "Generate the lazy sequence: X X X ..."
  (lazy-seq (cons x (repeat x))))

(example
 (take 5 (repeat :yeah)) => '(:yeah :yeah :yeah :yeah :yeah))

(defun repeatedly (f)
  "Generate the lazy sequence (F) (F) (F) ..."
  (lazy-seq (cons (funcall f) (repeatedly f))))

(example
 (integerp
  (head (drop 10
	      (repeatedly (lambda () (random 10))))))
 => T)


(defstruct range-state
  (val 0)
  (lim nil)
  (step 1))

(defmethod head ((r range-state))
  (if (emptyp r)
      (error "Empty sequence")
      (range-state-val r)))

(defmethod tail ((r range-state))
  (let ((nval (+ (range-state-val r)
		 (range-state-step r))))
    (if (and (range-state-lim r)
	     (> nval (range-state-lim r)))
	(error "Empty sequence")
	(make-range-state :val nval
			  :lim (range-state-lim r)
			  :step (range-state-step r)))))

(defmethod emptyp ((r range-state))
  (and (range-state-lim r)
       (>= (range-state-val r)
	   (range-state-lim r))))

(defmethod print-cell ((r range-state) out)
  (format out "~A.." (range-state-val r))
  (if (range-state-lim r)
    (format out "~A" (range-state-lim r))
    (format out "."))
  (when (> (range-state-step r) 1)
    (format out "/~A" (range-state-step r))))

(defmethod print-object ((r range-state) out)
  (format out "#<range:")
  (print-cell r out)
  (format out ">"))

(defun range (&optional (start 0) (lim nil) (step 1))
  "Generate a range sequence with START value (defaulting to 0)
until LIM (or an \"infinite\" range if LIM is NIL),
 and with an increment STEP (defaulting to 1)."
  (make-range-state :val start :lim lim :step step))


(examples
 (take 3 (range 1 5))
 => '(1 2 3)

 (take 10 (range 1 5))
 => '(1 2 3 4)

 (take 0 (range 1 5))
 => NIL

 (take 8 (range 1 nil))
 => '(1 2 3 4 5 6 7 8)

 (take 8 (range 1 nil 2))
 => '(1 3 5 7 9 11 13 15)
 
)
