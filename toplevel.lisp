
;; start with this when developing the library.
(push (truename "./") asdf:*central-registry*)

;; for debugging / dev. time, also eval the following line
(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) 

;; tell quicklisp to load the project.
(ql:quickload "lazyseq")

;; you can also do some testing (not required)
(ql:quickload "lazyseq-test")
(require 'asdf)
;; you can retest everything with the following
(asdf:test-system :lazyseq)


;; and now, code happily ...
