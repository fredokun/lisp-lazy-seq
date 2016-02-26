
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
   #:reduces
   #:reductions
   ))


