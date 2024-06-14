
(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
  (:use #:cl)
  (:export #:*example-enabled*
           #:*example-equal-predicate*
           #:example
           #:examples
           #:example-progn
	   #:comment
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
   #:emptyp
   #:print-cell
   #:lazy-cons
   #:lazy-seq
   #:take
   #:take-while
   #:take-all
   #:drop
   #:drop-while
   #:seq-elt
   #:chunk-seq
   #:lazy-cat
   #:lazy-catmap
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
   #:lazy-rec
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
   ;; Comprensions
   #:lazy-for
   ))


