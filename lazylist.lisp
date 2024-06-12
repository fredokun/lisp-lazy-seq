#|
 Defines:

   lazy-list, lazy-list*     Create lazy sequences conveniently
   self-ref        Self-referential lazy sequences
   alazy-list, alazy-list*   Anamorphic macro variants for self referencing
   lazy-labels     Creates scope in which mutually recursive lazy sequences can be defined
|#

(in-package #:lazyseq)

(defmacro lazy-list (&rest items)
  "Construct a lazy sequence from given ITEMS.
These will be evaluated lazily."
  (let ((inner nil))
    (dolist (it (reverse items))
      (setf inner `(lazy-seq (cons ,it ,inner))))
    inner))

(example
 (take 2 (lazy-list))
 => nil)

(example
 (take 3 (lazy-list 1 2 3))
 => '(1 2 3))

(example
 (take 2 (lazy-list (+ 1 2) nil))
 => '(3 nil))


(defmacro lazy-list* (&rest items)
  "Construct a lazy sequence from given ITEMS.
These will be evaluated lazily. 
The last element of ITEMS is expected to be a lazy sequence or list."
  (let* ((items-rev (reverse items))
         (inner (first items-rev)))
    (dolist (it (rest items-rev))
      (setf inner `(lazy-seq (cons ,it ,inner))))
    inner))

(example
 (take 4 (lazy-list* 1 2 '(3 4)))
 => '(1 2 3 4))

(example
 (take 4 (lazy-list* 1 2 (nats 0)))
 => '(1 2 0 1))

(example
 (take 4 (lazy-list* 1 2 (lazy-list 3 4)))
 => '(1 2 3 4))

;;; lazy-ref-cell structure

(defstruct lazy-ref-cell
  "A transparent object which references another object. 
   Used as a target for self references"
  (target nil))

(defmethod head ((c lazy-ref-cell))
  (head (lazy-ref-cell-target c)))

(defmethod tail ((c lazy-ref-cell))
  (tail (lazy-ref-cell-target c)))

(defmethod emptyp ((c lazy-ref-cell))
  (emptyp (lazy-ref-cell-target c)))

(defmethod print-cell ((c lazy-ref-cell) out)
  (print-cell (lazy-ref-cell-target c) out))

(defmethod print-object ((c lazy-ref-cell) out)
  (format out "#<lazy-ref:")
  (print-cell c out)
  (format out ">"))


(defmacro self-ref (sym items)
  "Binds a symbol SYM to refer to the lazy sequence or list ITEMS.
SYM is bound to a lazy-ref-cell in the evaluation of ITEMS
so can be used to create self-referential lazy lists

Example:  The Fibonacci sequence

(take 10
  (self-ref fib (lazy-list* 1 1 (maps #'+ fib (tail fib)))))
 => '(1 1 2 3 5 8 13 21 34 55))

  "
  `(let ((,sym (make-lazy-ref-cell)))
     (setf (lazy-ref-cell-target ,sym)
           ,items)
     ,sym))


(example 
 (take 10
       (self-ref fib (lazy-list* 1 1 (maps #'+ fib (tail fib)))))
 => '(1 1 2 3 5 8 13 21 34 55))

(defmacro alazy-list (&rest items)
  "Anaphoric macro which creates a lazy list consisting
   of the given ITEMS, which will be evaluated lazily.

   The symbol SELF is bound to the start of the sequence, so can be
   used to define self-referential lazy sequences.
  "
  (let ((self (intern (symbol-name 'self)))) ; intern so that SELF does not need to be exported
    `(self-ref ,self (lazy-list ,@items))))

(defmacro alazy-list* (&rest items)
  "Anaphoric macro which creates a lazy list consisting
   of the given ITEMS, which will be evaluated lazily. 
   The final item in ITEMS is expected to be a list or lazy sequence.

   The symbol SELF is bound to the start of the sequence, so can be
   used to define self-referential lazy sequences.
   
   Example:
   
   (take 10 (alazy-list* 1 (maps #'1+ self)))
   => '(1 2 3 4 5 6 7 8 9 10))
  "
  (let ((self (intern (symbol-name 'self)))) ; intern so that SELF does not need to be exported
    `(self-ref ,self (lazy-list* ,@items))))

(example
 (take 10 (alazy-list* 1 (maps #'1+ self)))
 => '(1 2 3 4 5 6 7 8 9 10))

(example 
 (take 10 (alazy-list* 1 1 (maps #'+ self (tail self))))
 => '(1 1 2 3 5 8 13 21 34 55))


;;; lazy-labels, a more general form than self-ref

(defmacro lazy-labels (bindings &body body)
  "Evaluates BODY in a scope where BINDINGS are defined
   using a list of (symbol form) pairs, where the forms
   evaluate to lazy sequences or lists. 

   All symbols are first bound to lazy-ref-cells objects, 
   and are available during the evaluation of all forms. 
   This enables the definition of mutually recursive 
   lazy sequences.

   Example:
    
    (lazy-labels ((evens (lazy-list* 0 (maps #'1+ odds)))
                  (odds (maps #'1+ evens)))
       (take 4 odds))
    => (1 3 5 7)
  "
  
  (let (;; For each binding in BINDINGS, create a lazy reference cell
        (ref-cells (mapcar (lambda (binding)
                             (let ((sym (first binding)))
                               (unless (= (length binding) 2)
                                 (error "Each binding must have two elements: a symbol and form: ~S" binding))
                               (unless (symbolp sym)
                                 (error "First element of bindings must be a symbol: ~S" sym))
                               (list sym '(make-lazy-ref-cell))))
                           bindings))

        ;; For each binding make a form which evaluates the given expression
        ;; and assigns it to the lazy-ref-cell target.
        (set-refs (mapcar (lambda (binding)
                            `(setf (lazy-ref-cell-target ,(first binding))
                                   ,(second binding)))
                          bindings)))

    `(let ,ref-cells
       ,.set-refs
       ,@body)))

(example 
 (lazy-labels ((evens (lazy-list* 0 (maps #'1+ odds)))
               (odds (maps #'1+ evens)))
   (take 4 odds))
 => '(1 3 5 7))

(example 
 (lazy-labels ((evens '(0 2 4 6))        ; Also works with lists
               (odds (maps #'1+ evens)))
   (take 4 odds))
 => '(1 3 5 7))
 
