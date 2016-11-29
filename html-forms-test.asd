(in-package #:asdf-user)

(defsystem "html-forms-test"
  :name "html-forms-test"
  :description "Tests for the system html-forms."
  :license "AGPLv3"
  :author "Javier Olaechea <pirata@gmail.com>"
  :depends-on (#:cl-ppcre
               #:html-forms
               #:prove)
  :components ((:file "tests")))
