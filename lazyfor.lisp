
#|

   Lazy comprehensions

   This is from: https://github.com/fredokun/lisp-list-comprehensions

|#

(in-package #:lazyseq)

;; Seq[a] * (a -> Seq[b]) -> Seq[b]
(defun lazy-bind (s f)
  "The monadic bind operator for lazy sequences, here apply `F` on `S`."
  (lazy-catmap f s))

;; a -> Seq[a]
(defun lazy-pure (x)
  "The monadic pure operator, a sequence containing only `S`."
  (list x))

(examples
 (take 10 (lazy-bind '(1 2 3 4 5)
		     (lambda (x) (lazy-pure (* x x)))))
 => '(1 4 9 16 25)

 (take 10 (lazy-bind (iterate #'1+ 1)
		     (lambda (x) (lazy-pure (* x x)))))
  => '(1 4 9 16 25 36 49 64 81 100)

 (take 8 (lazy-bind
	  '(1 2 3 4)
	  (lambda (x) (lazy-bind
		       '(A B)
		       (lambda (y) (lazy-pure (cons x y)))))))
 => '((1 . A) (1 . B) (2 . A) (2 . B) (3 . A) (3 . B) (4 . A) (4 . B))


 (take 8 (lazy-bind
	  (iterate #'1+ 1)
	  (lambda (x) (lazy-bind
		       '(A B)
		       (lambda (y) (lazy-pure (cons x y)))))))
 => '((1 . A) (1 . B) (2 . A) (2 . B) (3 . A) (3 . B) (4 . A) (4 . B))
 )


(defun lazy-fail ()
  "The monadic fail operator."
  (list))

(examples
 (take 10 (lazy-bind '(1 2 3 4 5 6 7 8)
		     (lambda (x) (if (evenp x) (lazy-pure x) (lazy-fail)))))
 => '(2 4 6 8)

 (take 10 (lazy-bind '(1 2 3 4 5 6 7 8)
		     (lambda (x) (if (oddp x) (lazy-pure x) (lazy-fail)))))
  => '(1 3 5 7)

 (take 10 (lazy-bind (iterate #'1+ 1)
		     (lambda (x) (if (evenp x) (lazy-pure x) (lazy-fail)))))
 => '(2 4 6 8 10 12 14 16 18 20)

 (take 10 (lazy-bind (iterate #'1+ 1)
		     (lambda (x) (if (oddp x) (lazy-pure x) (lazy-fail)))))
 => '(1 3 5 7 9 11 13 15 17 19)
)

(defmacro lazy-for (&body body)
  (cond 
    ((null body) (progn))
    ((null (cdr body)) (car body))
    ((is-sym (cadr body) "<-")
     `(lazy-bind ,(caddr body) (lambda (,(car body)) (lazy-for ,@(cdddr body)))))
    ((is-sym (car body) "WHEN")
     `(if ,(cadr body) (lazy-for ,@(cddr body)) (lazy-fail)))
    ((is-sym (car body) "YIELD")
     `(lazy-pure ,(cadr body)))
    (t (error "Not a for expression: ~S" `(quote ,body)))))

(examples  ;; basic examples
 (take 10 (lazy-for 
	    i <- '(1 2 3 4 5 6 7 8)
	    :yield (* i i)))
 => '(1 4 9 16 25 36 49 64)

(take 10 (lazy-for
	   i <- '(1 2 3 4)
	   j <- '(A B)
	   :yield (cons i j)))
 => '((1 . A) (1 . B) (2 . A) (2 . B) (3 . A) (3 . B) (4 . A) (4 . B))
)

(examples  ;; infinite lists
 (take 10 (lazy-for 
	    i <- (iterate #'1+ 1)
	    :yield (* i i)))
 => '(1 4 9 16 25 36 49 64 81 100)

(take 10 (lazy-for
	   i <- (iterate #'1+ 1)
	   j <- '(A B)
	   :yield (cons i j)))
 => '((1 . A) (1 . B) (2 . A) (2 . B) (3 . A) (3 . B) (4 . A) (4 . B) (5 . A) (5 . B))
)

(examples  ;; with :when clause
 (take 10 (lazy-for 
	    i <- '(1 2 3 4 5 6 7 8)
	    :when (evenp i)
	    :yield (* i i)))
 => '(4 16 36 64)

 (take 10 (lazy-for 
	    i <- '(1 2 3 4 5 6 7 8)
	    :when (oddp i)
	    :yield (* i i)))
 => '(1 9 25 49)
)

(examples  ;; with :when clause and infinite lists
 (take 10 (lazy-for 
	    i <- (iterate #'1+ 1) 
	    :when (evenp i)
	    :yield (* i i)))

 => '(4 16 36 64 100 144 196 256 324 400)

 (take 10 (lazy-for 
	    i <- (iterate #'1+ 1)
	    :when (oddp i)
	    :yield (* i i)))
 => '(1 9 25 49 81 121 169 225 289 361)
)










