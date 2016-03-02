
(in-package #:lazyseq)

(defclass stream-cell ()
  ((stream :initarg :stream))
  (:documentation "The representation of a cell for an abstract (input) stream."))

(defmethod print-object ((c stream-cell) out)
  (format out "#<steam:~A>" (slot-value c 'stream)))

(defclass char-stream-cell (stream-cell)
  ((hd :initform nil)
   (tl :initform nil))
  (:documentation "A stream cell for character inputs."))



(defclass line-stream-cell (stream-cell)
  ((hd :initform nil)
   (tl :initform nil))
  (:documentation "A stream cell for character inputs."))

(defclass byte-stream-cell (stream-cell)
  ((hd :initform nil)
   (tl :initform nil))
  (:documentation "A stream cell for byte inputs."))
  

(defclass sequence-stream-cell (stream-cell)
  ((prefetch :initarg :prefetch :initform 32)
   (chunk :initform nil)
   (pos :initform 0))
  (:documentation "A stream cell for sequence inputs."))


