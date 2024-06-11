
;; start with this when developing the library.
(push (truename "./") asdf:*central-registry*)

;; tell quicklisp to load the project.
(ql:quickload "lazyseq")

;; you can also do some testing (not required)
(ql:quickload "lazyseq-test")
(require 'asdf)
;; you can retest everything with the following
(asdf:test-system :lazyseq)


;; and now, code happily ...
