
(asdf:defsystem :lazyseq
  :description "A library for lazy sequences."
  :version "0.1"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
  :license "MIT License. See LICENSE."
  :depends-on (:alexandria
               )
  :in-order-to ((test-op (load-op lazyseq-test)))
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "lazyseq")
               (:file "chunked")
               (:file "concat")
               (:file "produce")
               (:file "xform")
               (:file "reduce")
               ))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :lazyseq))))
  (load-system 'lazyseq-test)
  (let ((suites '(basic)))
    (dolist (suite suites)
      (funcall (intern "RUN!" :5am)
               (intern (symbol-name suite) :lazyseq-test)))))
