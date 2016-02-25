
(asdf:defsystem #:lazyseq
  :description "A library for lazy sequences."
  :version "0.1"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
  :license "MIT License. See LICENSE."
  :depends-on (:alexandria
	       )
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "lazyseq")
	       (:file "produce")
	       (:file "xform")
	       (:file "reduce")
	       ))

