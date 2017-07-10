(in-package #:asdf-user)

(defsystem "html-forms"
  :name "html-forms"
  :description ""
  :version "0.1.0"
  :license "AGPLv3"
  :author "Javier Olaechea <pirata@gmail.com>"
  :depends-on (#:closer-mop
               ;; To be splitted
               #:spinneret

               #:clavier)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "html-forms" :depends-on ("utils" "package"))
               ;; Split into their own systems
               (:file "spinneret")
               (:file "clavier")))
