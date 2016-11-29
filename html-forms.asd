(in-package #:asdf-user)

(defsystem "html-forms"
  :name "html-forms"
  :description ""
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
               ;; Split into its own system
               (:file "spinneret")
               (:file "clavier")))
