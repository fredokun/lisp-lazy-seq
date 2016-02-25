
;; start with this when developing the library.
(push (truename "./") asdf:*central-registry*)

;; tell quicklisp to load the project.
(ql:quickload "lazyseq")

;; and now, code happily ...
