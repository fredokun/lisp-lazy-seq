
;; start with this when developing the library.
(push (truename "./") asdf:*central-registry*)

;; tell quicklisp to load the project.
(ql:quickload "lazyseq")

;; or alternatively, do some testing
(require 'asdf)
(asdf:test-system 'lazyseq-test)


;; and now, code happily ...
