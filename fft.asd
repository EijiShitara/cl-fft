(defsystem "fft"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "fft/tests"))))

(defsystem "fft/tests"
  :author ""
  :license ""
  :depends-on ("fft"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for fft"
  :perform (test-op (op c) (symbol-call :rove :run c)))
