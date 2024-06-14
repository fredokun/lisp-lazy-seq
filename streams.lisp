
(in-package #:lazyseq)

(defclass stream-cell ()
  ((stream :initarg :stream)
   (autoclose :initarg :autoclose :initform nil))
  (:documentation "The representation of a cell for an abstract (input) stream."))

(defmethod print-cell ((c stream-cell) out)
  (with-slots (stream hd tl) c
    (format out "~A" hd)
    (if stream
        (format out "..~A" stream)
        (progn (format out " ")
               (print-cell tl out)))))

(defmethod print-object ((c stream-cell) out)
  (format out "#<stream:")
  (print-cell c out)
  (format out ">"))

;;; ===================
;;; Character streams
;;; ===================

(defclass char-stream-cell (stream-cell)
  ((hd :initarg :hd)
   (tl :initform nil))
  (:documentation "A stream cell for character inputs."))

(defun compute-char-stream (c)
  "Compute the char stream by performing a single read.
Returns NIL if the stream is at the end. Conditions
are simply propagated.

CAUTION: the char stream must be non-NIL when calling
this function.

If the AUTOCLOSE flag is non-NIL, then the stream is
automagically closed when reaching the EOF."

  (with-slots (stream autoclose hd tl) c
    (let  ((ch (read-char stream nil :at-eof)))
      (if (eql ch :at-eof)
          (progn (when autoclose
                   (close stream))
                 (setf stream nil)
                 nil)
          ;; not at eof
          (progn (setf tl (make-instance 'char-stream-cell :stream stream :autoclose autoclose :hd ch))
                 (setf stream nil)
                 c)))))

(defmethod head ((c char-stream-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (slot-value c 'hd)))

(defmethod tail ((c char-stream-cell))
  (if (emptyp c)
      (error "Empty sequence")
      (slot-value c 'tl)))

(defmethod emptyp ((c char-stream-cell))
  (if (compute-char-stream c)
      nil
      t))

(defun char-seq (in-stream &key (autoclose nil))
  "Generates a lazy sequence of characters from an INPUT-STREAM.
Note that the stream must be such that characters can be fetched
 using READ-CHAR.

Because of EOF detection, there is always one character read in advance
 from the stream.

If the AUTOCLOSE flag is set, then the stream will be automagically
 closed when reaching the EOF. This may be dangerous (if the stream
 is shared) but is otherwise practical when working at the REPL.

Also note that the stream is only closed when reaching the EOF.
Use FLUSH-SEQ for forcing the close operation."

  (let ((ch (read-char in-stream nil :at-eof)))
    (if (eql ch :at-eof)
        (progn (when autoclose
                 (close in-stream))
               nil)
        ;; not at eof
        (make-instance 'char-stream-cell :stream in-stream :autoclose autoclose :hd ch))))

(example
 (with-input-from-string (in "")
   (char-seq in)) 
 => '())

(example
 (with-input-from-string (in "hello world")
   (take 5 (char-seq in)))
 => '(#\h #\e #\l #\l #\o))


(example
 (with-input-from-string (in "hello world")
   (take 25 (char-seq in))) => '(#\h #\e #\l #\l #\o #\Space #\w #\o #\r #\l #\d))

(example
 (let ((in (make-string-input-stream "hello world")))
   (progn (flush-seq (char-seq in :autoclose t))
          (open-stream-p in))) => nil)

(defun string-char-seq (in-string &optional (start 0) (end nil))
  "Generates a lazy sequence of characters from an IN-STRING.

The function is a simple wrapper around MAKE-STRING-INPUT-STREAM.
The START and END optional arguments work accordingly.

CAUTION: the underlying stream will only be closed if the
sequence is fully computed. Use FLUSH-SEQ to force the closing operation."

  (let ((in-stream (make-string-input-stream in-string start end)))
    (char-seq in-stream :autoclose t)))

(example
 (take 5 (string-char-seq "hello world"))
 => '(#\h #\e #\l #\l #\o))

(defun file-char-seq (in-file &key (if-does-not-exist :error))
  "Generates a lazy sequence of characters from an IN-FILE.

The function is a simple wrapper around OPEN for opening file
 as text input input. The :IF-DOES-NOT-EXIST keyword argument
 works accordingly.

CAUTION: the underlying stream will only be closed if the
sequence is fully computed. Use FLUSH-SEQ to force the closing operation."
  (let ((in-stream (open in-file :if-does-not-exist if-does-not-exist)))
    (char-seq in-stream :autoclose t)))

(example
 (take 10 (file-char-seq (truename "./streams.lisp"))) ;; use *load-truename* instead
 => '(#\Newline #\( #\i #\n #\- #\p #\a #\c #\k #\a))

;;; ===================
;;; Line streams
;;; ===================

(defclass line-stream-cell (stream-cell)
  ((hd :initarg :hd)
   (tl :initform nil))
  (:documentation "A stream cell for text line inputs."))

(defun compute-line-stream (c)
  "Compute the line stream by performing a single read.
Returns NIL if the stream is at the end. Conditions
are simply propagated.

CAUTION: the char stream must be non-NIL when calling
this function.

If the AUTOCLOSE flag is non-NIL, then the stream is
automagically closed when reaching the EOF."

  (with-slots (stream autoclose hd tl) c
    (let  ((line (read-line stream nil :at-eof)))
      (if (eql line :at-eof)
          (progn (when autoclose
                   (close stream))
                 (setf stream nil)
                 nil)
          ;; not at eof
          (progn (setf tl (make-instance 'line-stream-cell :stream stream :autoclose autoclose :hd line))
                 (setf stream nil)
                 c)))))

(defmethod head ((c line-stream-cell))
  (slot-value c 'hd))

(defmethod tail ((c line-stream-cell))
  (if (slot-value c 'stream)
      (if (compute-line-stream c)
          (slot-value c 'tl)
          nil)
      (slot-value c 'tl)))

(defun line-seq (in-stream &key (autoclose nil))
  "Generates a lazy sequence of lines from an INPUT-STREAM.
Note that the stream must be such that lines can be fetched
 using READ-LINE.

Because of EOF detection, there is always one line read in advance
 from the stream.

If the AUTOCLOSE flag is set, then the stream will be automagically
 closed when reaching the EOF. This may be dangerous (if the stream
 is shared) but is otherwise practical when working at the REPL.

Also note that the stream is only closed when reaching the EOF.
Use FLUSH-SEQ for forcing the close operation."

  (let ((line (read-line in-stream nil :at-eof)))
    (if (eql line :at-eof)
        (progn (when autoclose
                 (close in-stream))
               nil)
        ;; not at eof
        (make-instance 'line-stream-cell :stream in-stream :autoclose autoclose :hd line))))

(example
 (with-input-from-string (in "")
   (line-seq in)) => '())

(example
 (with-input-from-string (in "hello
lisp
world")
   (take 2 (line-seq in))) => '("hello" "lisp"))

(example
 (with-input-from-string (in "hello
lisp
world")
   (take 25 (line-seq in))) => '("hello" "lisp" "world"))

(example
 (let ((in (make-string-input-stream "hello
lisp
world")))
   (progn (flush-seq (line-seq in :autoclose t))
          (open-stream-p in))) => nil)

(defun string-line-seq (in-string &optional (start 0) (end nil))
  "Generates a lazy sequence of text lines from an IN-STRING.

The function is a simple wrapper around MAKE-STRING-INPUT-STREAM.
The START and END optional arguments work accordingly.

CAUTION: the underlying stream will only be closed if the
sequence is fully computed. Use FLUSH-SEQ to force the closing operation."

  (let ((in-stream (make-string-input-stream in-string start end)))
    (line-seq in-stream :autoclose t)))

(example
 (take 5 (string-line-seq "hello
lisp
world"))
 => '("hello" "lisp" "world"))

(defun file-line-seq (in-file &key (if-does-not-exist :error))
  "Generates a lazy sequence of text lines from an IN-FILE.

The function is a simple wrapper around OPEN for opening file
 as text input input. The :IF-DOES-NOT-EXIST keyword argument
 works accordingly.

CAUTION: the underlying stream will only be closed if the
sequence is fully computed. Use FLUSH-SEQ to force the closing operation."

  (let ((in-stream (open in-file :if-does-not-exist if-does-not-exist)))
    (line-seq in-stream :autoclose t)))

(example
 (take 4 (file-line-seq (truename "./streams.lisp"))) ;; use *load-truename* instead
 => '("" "(in-package #:lazyseq)" "" "(defclass stream-cell ()"))

;;; ===================
;;; Byte streams
;;; ===================

(defclass byte-stream-cell (stream-cell)
  ((hd :initarg :hd)
   (tl :initform nil))
  (:documentation "A stream cell for byte inputs."))

(defun compute-byte-stream (c)
  "Compute the byte stream by performing a single read.
Returns NIL if the stream is at the end. Conditions
are simply propagated.

CAUTION: the stream must be non-NIL when calling
this function.

If the AUTOCLOSE flag is non-NIL, then the stream is
automagically closed when reaching the EOF."

  (with-slots (stream autoclose hd tl) c
    (let  ((b (read-byte stream nil :at-eof)))
      (if (eql b :at-eof)
          (progn (when autoclose
                   (close stream))
                 (setf stream nil)
                 nil)
          ;; not at eof
          (progn (setf tl (make-instance 'byte-stream-cell :stream stream :autoclose autoclose :hd b))
                 (setf stream nil)
                 c)))))

(defmethod head ((c byte-stream-cell))
  (slot-value c 'hd))

(defmethod tail ((c byte-stream-cell))
  (if (slot-value c 'stream)
      (if (compute-byte-stream c)
          (slot-value c 'tl)
          nil)
      (slot-value c 'tl)))

(defun byte-seq (in-stream &key (autoclose nil))
  "Generates a lazy sequence of bytes from an INPUT-STREAM.
Note that the stream must be such that bytes can be fetched
 using READ-BYTE.

Because of EOF detection, there is always one byte read in advance
 from the stream.

If the AUTOCLOSE flag is set, then the stream will be automagically
 closed when reaching the EOF. This may be dangerous (if the stream
 is shared) but is otherwise practical when working at the REPL.

Also note that the stream is only closed when reaching the EOF.
Use FLUSH-SEQ for forcing the close operation."

  (let ((b (read-byte in-stream nil :at-eof)))
    (if (eql b :at-eof)
        (progn (when autoclose
                 (close in-stream))
               nil)
        ;; not at eof
        (make-instance 'byte-stream-cell :stream in-stream :autoclose autoclose :hd b))))

(defun file-byte-seq (in-file &key (if-does-not-exist :error))
  "Generates a lazy sequence of bytes from an IN-FILE.

The function is a simple wrapper around OPEN for opening file
 as binary input. The :IF-DOES-NOT-EXIST keyword argument
 works accordingly.

CAUTION: the underlying stream will only be closed if the
sequence is fully computed. Use FLUSH-SEQ to force the closing operation."

  (let ((in-stream (open in-file
                         :element-type 'unsigned-byte
                         :if-does-not-exist if-does-not-exist)))
    (byte-seq in-stream :autoclose t)))

(example
 (take 4 (file-byte-seq (truename "./streams.lisp"))) ;; use *load-truename* instead
 => '(10 40 105 110))


(comment  ;; I don't remind what this was ... ( ´_ﾉ`)

(defclass sequence-stream-cell (stream-cell)
  ((prefetch :initarg :prefetch :initform 32)
   (chunk :initform nil)
   (pos :initform 0))
  (:documentation "A stream cell for sequence inputs."))

)
