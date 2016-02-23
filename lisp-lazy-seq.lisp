
#|

# Lazy sequences in Common Lisp

The *lazy sequence* abstraction provided in Clojure is quite an interesting beast.
It can be found as other names, e.g. *generators* in Python, *(implicitly lazy) lists* in
 Haskell, *streams* in OCaml, etc. There is nothing about such an abstraction in the
 CLHS and although there are various approaches (e.g. Clazy, slow-jam and probably many
 others), I wondered if I could :

  - implement a similar abstraction in my favorite programming language, with
 an API similar to that of Clojure

  - make it fast enough

Because I want to compare with the Clojure version, I will :

|#


(declaim
 (optimize (speed 3) (safety 0) (debug 0)))

#|

and use my favorite implementation of Lisp: SBCL for the (silly) benchmarks.

**Remark**: this is not a Common Lisp port of the Clojure source code, and any
 resemblance would be accidental (although there isn't a million ways to implement
 lazy-sequences.

|#

#|

## The lazy sequence representation

For the internal implementation of a lazy sequence, I will use a simple *cell structure* composed of
 three slots :

|#

(defstruct lazy-cell
  (genfn nil)  ;; a generator function
  (hd nil)     ;; a computed head (`nil` when not already computed)
  (tl nil))    ;; a computed tail (also `nil` initially)

#|

A `lazy-cell` instance has only two possible states :

  - either it is *computed*, when `genfn` is `nil`, 

  - or *not yet computed*, in which case `genfn` is an argument-less lambda,
 sometimes called a frozen computation (or a one-shot, argument-less continuation for
 the schemer crowd).

## Head and tail

There is only two things that can basically be done with a lazy sequence (represented as a `lazy-cell`):

  - taking it's **head**, i.e. it's first element (it is called *first* in Clojure)

  - taking it's **tail**, which is a lazy sequence of the rest of the elements (*rest* in Clojure)

In the *not yet computed* state, taking the *head* or the *tail* requires the invocation
 of the `genfn` function once. The function should return a list whose `car` is the value of the
 head, and whose `cdr` correspond to the tail sequence.

The "magic" happens in the 3 lines function below:

|#

(defun compute-seq (zseq)
  (let ((val (funcall (lazy-obj-genfn zseq))))
    (setf (lazy-obj-genfn zseq) nil)
    val))

#|

The `compute-seq` function should only be called in the *not yet computed* state,
 and since we set the `genfn` slot to `nil` we know that the lazy sequence instance is in
 the 

Now, taking the head is simply a matter of calling `compute-flow` 

|#

(defmethod head ((zobj lazy-obj))
  (when (lazy-obj-genfn zobj)
    (let ((val (compute-seq zobj)
            (setf (lazy-obj-hd zobj) (car val))
            (setf (lazy-obj-tl zobj) (cdr val))))))
  (lazy-obj-hd zobj))

;; (defmethod head ((zobj lazy-obj))
;;   (when (lazy-obj-genfn zobj)
;;     (let ((val (funcall (lazy-obj-genfn zobj))))
;;       (setf (lazy-obj-hd zobj) (first val))
;;        (let ((tv (rest val)))
;; 	(setf (lazy-obj-tl zobj) (make-lazy-obj :genfn (lambda () tv)  :hd nil :tl nil))
;; 	(setf (lazy-obj-genfn zobj) nil))))
;;   (lazy-obj-hd zobj))

(defmethod tail ((zobj lazy-obj))
  (when (lazy-obj-genfn zobj)
    (compute-seq zobj))
  (lazy-obj-tl zobj))

;; (defmethod tail ((zobj lazy-obj))
;;   (if (lazy-obj-genfn zobj)
;;       (let ((val (funcall (lazy-obj-genfn zobj))))
;;         (rest val))
;;         (let ((tv (rest val)))
;;           (make-lazy-obj :genfn (lambda () tv) :hd nil :tl nil)))
;;       (lazy-obj-tl zobj)))


(defmacro lazy-seq (expr)
  `(make-lazy-obj :genfn (lambda () ,expr) :hd nil :tl nil))

(defun const (val)
  (lazy-seq (cons val (const val))))

(head (const 1))
(tail (const 1))
(head (tail (const 1)))

;;(defparameter vals (const 42))

;;vals

;;(head vals)

;;vals

;;(head vals)

;;(tail vals)

;;(head (tail vals))

(defun nats (n)
  (lazy-seq (cons n (nats (1+ n)))))

(head (nats 1))
(head (tail (nats 1)))

;;(defparameter nats1 (nats 1))

;;nats1

;;(defparameter nats2 (tail nats1))

;;nats1

;;nats2

;;(head nats2)

;;nats2

;;(head nats1)
;;(head (tail nats1))

;;(head (tail (tail nats1)))


(defun take (n s)
  (if (zerop n)
      ()
      (cons (head s) (take (1- n) (tail s)))))

(take 10 (const 10))

(take 10 (nats 1))

;;(take 5 nats1)

;;(take 5 nats1)

(defun drop (n s)
  (declare (type fixnum n))
  (let ((res s))
    (dotimes (_ n)
      (setf res (tail res)))
    res))


(time (head (drop 1000000 (nats 1))))

;;(defparameter nats10000 (drop 10000 nats1))

;; nats1

;;(head nats10000)

;;nats10

 ;; (require :sb-sprof)

 ;; (sb-sprof:with-profiling (:max-samples 500
 ;;                                       :report :flat
 ;;                                        :loop nil)
 ;;   (drop 5000 (nats 1)))


;;(time (head (drop 1000 (nats 1))))

;;(format t "~A~%" (head (drop 10000 (nats 1))))

(time (head (drop 10000 (const 1))))


