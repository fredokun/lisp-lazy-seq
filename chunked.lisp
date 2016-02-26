
(in-package #:lazyseq)

(defstruct chunked-cell
  "The cell representation of chuncked lazy sequences.

The SEQ slot is the sequence whose consumption is driven
 in a chunked way. The PREFETCH slot contains the size
 of the chunk, it is 32 by default.
The POS slot contains the position in the current CHUNK.

"
  (prefetch 32)
  (chunk nil)
  (pos nil)
  (seq nil))

(defun compute-chunked-cell (c)
  "Compute the chunk for chunked cell C.

CAUTION: this should only be called when the CHUNK cell is NIL.

If the chunked sequence has less elements than the chunk size then
 the chunking process is aborpted and the function returns NIL."
  (let ((chunk (make-array (chunked-cell-prefetch c) :initial-element nil))
        (seq (chunked-cell-seq c)))
    (loop
       for i from 0 upto (1- (chunked-cell-prefetch c))
       when (not seq) do (return-from compute-chunked-cell nil)
       do
         (setf (aref chunk i) (head seq))
         (setf seq (tail seq)))
    (setf
     (chunked-cell-chunk c) chunk
     (chunked-cell-pos c) 0
     (chunked-cell-seq c) seq)
    t))

(defmethod head ((c chunked-cell))
  (when (not (chunked-cell-chunk c))
    (if (not (compute-chunked-cell c))
        (return-from head (head (chunked-cell-seq c)))))
  (aref (chunked-cell-chunk c) (chunked-cell-pos c)))

(defmethod tail ((c chunked-cell))
  (when (not (chunked-cell-chunk c))
    (if (not (compute-chunked-cell c))
        (return-from tail (tail (chunked-cell-seq c)))))
  (if (= (chunked-cell-pos c) (1- (array-total-size (chunked-cell-chunk c))))
      (make-chunked-cell
       :prefetch (chunked-cell-prefetch c)
       :chunk nil
       :pos nil
       :seq (chunked-cell-seq c))
      ;; some more chunked elements
      (make-chunked-cell
       :prefetch (chunked-cell-prefetch c)
       :chunk (chunked-cell-chunk c)
       :pos (1+ (chunked-cell-pos c))
       :seq (chunked-cell-seq c))))

(defmethod print-cell ((c chunked-cell) out)
  (when (chunked-cell-chunk c)
    (format out "[")
    (let ((sep ""))
      (loop for e across (chunked-cell-chunk c)
         for i = 0 then (1+ i)
         do
           (format out sep)
           (setf sep " ")
           (if (eql i (chunked-cell-pos c))
               (format out "<~A>" e)
               (format out "~A" e))))
    (format out "]")
    (format out " .. "))
  (format out "~A" (chunked-cell-seq c)))

(defmethod print-object ((c chunked-cell) out)
  (format out "#<chunked:") (print-cell c out) (format out ">"))

(defmacro chunk-seq (seq &key (prefetch 32))
  "Defines a chunked sequence out of the sequence SEQ.
This way, the consumption of SEQ will be made by whole
 chunks of PREFETCH elements (defaulting to 32)."
  `(make-chunked-cell :prefetch ,prefetch
                      :chunk nil
                      :pos nil
                      :seq ,seq))
