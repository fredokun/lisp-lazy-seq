
(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
  (:use #:cl)
  (:export #:*example-enabled*
           #:*example-equal-predicate*
           #:example
           #:example-progn
           #:*logg-enabled*
           #:*logg-level*
           #:logg
           #:vbinds
           #:afetch
	   #:while
	   #:read-file-lines
	   #:read-string-file
	   #:read-binary-file
	   ))

(defpackage #:lazyseq
  (:use #:cl #:fredo-utils)
  (:export
   #:head
   #:tail
   #:print-cell
   #:lazy-seq
   #:take
   #:take-while
   #:take-all
   #:drop
   #:drop-while
   #:seq-elt
   #:chunk-seq
   #:lazy-cat
   #:cycle
   #:iterate
   #:repeatedly
   #:repeat
   #:range
   #:maps
   #:filters
   #:scanl
   ;; reductions
   #:reduces
   #:reductions
   #:flush-seq
   #:any
   #:all
   ;; lists
   #:lazy-list
   #:lazy-list*
   #:self-ref
   #:alazy-list
   #:alazy-list*
   #:lazy-labels
   ;; streams
   #:char-seq
   #:string-char-seq
   #:file-char-seq
   #:line-seq
   #:string-line-seq
   #:file-line-seq
   ;; Sorting
   #:lazy-sort
   ))


