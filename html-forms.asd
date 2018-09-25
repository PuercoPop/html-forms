(defsystem "html-forms"
  :name "html-forms"
  :description ""
  :version "0.1.0"
  :license "AGPLv3"
  :author "Javier Olaechea <pirata@gmail.com>"
  :depends-on ("closer-mop"
               "spinneret"
               "clavier")
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "html-forms" :depends-on ("utils" "package"))
               (:file "spinneret")
               (:file "clavier"))
  :in-order-to ((test-op (test-op "html-forms/test"))))

(defsystem "html-forms/test"
  :name "html-forms-test"
  :description "Tests for the system html-forms."
  :license "AGPLv3"
  :author "Javier Olaechea <pirata@gmail.com>"
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("cl-ppcre"
               "html-forms"
               "prove")
  :components ((:test-file "tests"))
  :perform (test-op (op c)
                    (symbol-call :prove-asdf :run-test-system c)))
