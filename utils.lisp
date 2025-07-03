
(in-package #:fredokun-utilities)

#||

# CommonTypes: Utilities #

This file should not be modified directly, but copied verbatim
from: https://gitlab.com/fredokun/cl-utils
(and modified there, in case)

**Copyright** © 2023 Frederic Peschanski under the MIT License

||#

;; To activate the inline examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-enabled* t) ;; nil in production / t for self-testing

  (defparameter *example-equal-predicate* #'equalp)

  (defparameter *example-with-echo* nil)
  
  )

(defmacro example (expr arrow expected &key (warn-only nil))
  "Show an evaluation example, useful for documentation and lightweight testing.
   (example `EXPR` => `EXPECTED`) evaluates `EXPR` and compare, wrt. `EQUIV`
 (EQUAL by default) to `EXPECTED` and raise an error if inequal.
  Set `WARN-ONLY` to T for warning instead of error.
"
  (if (not *example-enabled*)
      (progn
        (when *example-with-echo*
          (format t "------------------~%")
          (format t "Example:~%~A~%=?= ~A~%" (format nil "~A" expr) expected)
          (format t "  ===> SKIP~%"))
        (values));; synonymous of nil if disabled
      ;; when enabled
      (let ((result-var (gensym "result-"))
            (expected-var (gensym "expected-"))
            (err-fun-var (gensym "err-fun-"))
            (expr-str (format nil "~A" expr)))
        `(progn
           (when *example-with-echo*
             (format t "------------------~%")
             (format t "Example:~%~A~%=?= ~A~%" ,expr-str ,expected))
           (let ((,err-fun-var (if ,warn-only #'warn #'error))
                 (,result-var ,expr)
                 (,expected-var ,expected))
             (if (not (equal (symbol-name (quote ,arrow)) "=>"))
                 (error "Missing arrow '=>' in example expression"))
             (if (funcall *example-equal-predicate* ,result-var ,expected-var)
                 (progn (if *example-with-echo*
                            (format t "  ===> PASS~%"))
                        t)
                 (funcall ,err-fun-var "Failed example:~%  Expression: ~S~%  ==> expected: ~A~%  ==> evaluated: ~A~%"
                          ,expr-str ,expected-var ,result-var)))))))

(defmacro examples (&rest exs)
  (when (not (zerop (mod (length exs) 3)))
    (error "Examples should be triples: (test-form ...) => <expected-value>"))
  (when (not *example-enabled*)
    (values))
  `(progn ,@(loop for i from 0 to (1- (length exs)) by 3
		  collect `(example ,@(subseq exs i (+ i 3))))))


(defmacro example-progn (&body body)
  "The toplevel forms of BODY are evaluated only if examples are enabled"
  (if *example-enabled*
      `(progn ,@body)
      (values)))

(defmacro comment (&body body)
  nil)

(defmacro vbinds (binders expr &body body)
  "An abbreviation for MULTIPLE-VALUE-BIND."
  (labels ((replace-underscores (bs &optional (result nil) (fresh-vars nil) (replaced nil))
             (if (null bs)
                 (let ((nresult (nreverse result))
                       (nfresh (nreverse fresh-vars)))
                   (values replaced nresult nfresh))
                 (if (equal (symbol-name (car bs)) "_")
                     (let ((fresh-var (gensym "underscore-")))
                       (replace-underscores (cdr bs) (cons fresh-var result) (cons fresh-var fresh-vars) t))
                     (replace-underscores (cdr bs) (cons (car bs) result) fresh-vars replaced)))))
    (multiple-value-bind (has-underscore nbinders fresh-vars) (replace-underscores binders)
      (if has-underscore
          `(multiple-value-bind ,nbinders ,expr
             (declare (ignore ,@fresh-vars))
             ,@body)
          `(multiple-value-bind ,binders ,expr ,@body)))))

(example (vbinds (a _ b) (values 1 2 3)
           (cons a b))
         => '(1 . 3)) ;; without a warning

(example (vbinds (a _ b _) (values 1 2 3 4)
           (cons a b))
         => '(1 . 3)) ;; without a warning


(defun afetch (comp alist &key (test #'eql))
  (let ((binding (assoc comp alist :test test)))
    (if binding
        (cdr binding)
        (error "No such key: ~A" comp))))

(defmacro while (condition &body body)
  (let ((eval-cond-var (gensym "eval-cond-"))
        (body-val-var (gensym "body-val-")))
    `(flet ((,eval-cond-var () ,`,condition))
       (do ((,body-val-var nil (progn ,@body)))
           ((not (,eval-cond-var))
            ,body-val-var)))))

(example (let ((count 0))
           (while (< count 10)
             ;;(format t "~A " count)
             (incf count)
             count))
         => 10)

(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun read-binary-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun read-string-file (filename)
  (with-open-file (stream filename)
    (let ((str (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer str) (read-sequence str stream))
      str)))

(defmacro comment (&rest args)
  (declare (ignore args))
  nil)

(defun mk-float-vector (contents)
  (map 'vector #'float contents))

;; Use now from trivial-clock: https://github.com/ak-coram/cl-trivial-clock
;; LICENSE: MTT
(define-symbol-macro current-time-start
    (load-time-value (logand (now) (1- (expt 2 32)))))

(declaim (inline current-time))
(defun current-time ()
  (declare (optimize speed (safety 0)))
  (multiple-value-bind (s ns) (now)
    (let* ((s (logand s (1- (expt 2 62))))
           (ms (logand ns (1- (expt 2 62)))))
          (declare (type (unsigned-byte 62) s ms))
            (+ (- s current-time-start)
               (* ns (coerce 1/1000000000 'double-float)))))
    (* (get-internal-real-time)
       (coerce (/ internal-time-units-per-second) 'double-float)))


(defun make-dynarray (&key (contents nil contents-p) (init-size (if contents-p
								    (length contents)
								    0) init-size-p))
  (if contents-p
      (make-array init-size :initial-contents contents :fill-pointer init-size :adjustable t)
      (make-array init-size :fill-pointer 0 :adjustable t)))

(declaim (inline dynarray-push))
(defun dynarray-push (arr val)
  (vector-push-extend val arr))
	   

#||

# Functional loop macro

(floop ((x1 init1) (x2 init2) ... (xn initn))
  <body>)

==>

(labels ((recur (x1 x2 ... xn))
  <body>)
  (let* ((x1 init1) (x2 init2) ... (xn initn))
    (recur x1 x2 ... xn)))


||#

(defmacro floop (bindings &body body)
  (when (not (listp bindings))
    (error "floop: variable bindings should be a list"))
  (let ((vars (mapcar #'first bindings)))
    (when (not (every #'symbolp vars))
      (error "floop: variables should be symbols"))
    `(labels ((recur ,vars ,@body))
       (let* ,bindings
	 (recur ,@vars)))))

(example
 (floop ((lst '(1 2 3)) (count 0))
   (if (endp lst)
       count
       (recur (cdr lst) (+ count (car lst)))))
 => 6)

(example
 (floop ((x 5) (y (* x 2)) (res (list)))
   (if (zerop x)
       res
       (recur (1- x) (1- y) (cons (list x y) res))))
 => '((1 6) (2 7) (3 8) (4 9) (5 10)))


(defun plist->alist (plst)
  (loop :for (k v) :on plst :by #'cddr :collect (list k v)))

(examples
 (plist->alist '(k1 v1 k2 v2 k3 v3)) => '((K1 V1) (K2 V2) (K3 V3))
 (plist->alist '(k1 v1)) => '((K1 V1))
 (plist->alist '(k)) => '((K NIL))
 (plist->alist '()) => '()
)

(defun alist->plist (alst)
  (loop :for (k v) :in alst :append (list k v)))

(examples
 (alist->plist '((k1 v1) (k2 v2) (k3 v3))) => '(K1 V1 K2 V2 K3 V3)
 (alist->plist '((k1 v1))) => '(K1 V1)
 (alist->plist '()) => '()
)

(defun reverse-plist (plst)
  (loop :for (val key) :on (reverse plst) :by #'cddr
        :append (list key val)))

(examples
  (reverse-plist '(k1 v1 k2 v2 k3 v3)) => '(K3 V3 K2 V2 K1 V1)
  (reverse-plist '(k1 v1)) => '(K1 V1)
  (reverse-plist '()) => '()
)


;; a very permissive recognizer for "symbols"
(defun is-sym (kw str)
  (and (symbolp kw)
       (string= (symbol-name kw) str)))
