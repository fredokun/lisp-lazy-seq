
;; start with this when developing the library.
(push (truename "./") asdf:*central-registry*)

;; tell quicklisp to load the project.
(ql:quickload "lazyseq")

;; you can also do some testing
(ql:quickload "lazyseq-test")
(require 'asdf)
(asdf:test-system :lazyseq)


;; and now, code happily ...
