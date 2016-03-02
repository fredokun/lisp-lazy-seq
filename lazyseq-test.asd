(defsystem lazyseq-test
  :name "lazyseq-test"
  :author "Frederic Peschanski"
  :license "MIT License. See LICENSE"
  :description "Tests for the lazyseq library."
  :depends-on (:lazyseq :fiveam)
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "suites"
                                            :depends-on ("packages"))
                                     (:file "basic"
                                            :depends-on ("suites"))))))


