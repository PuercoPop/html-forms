(defpackage "HTML-FORMS/CLAVIER"
  (:use #:cl
        #:html-forms))
(in-package "HTML-FORMS/CLAVIER")

(set-field-validator 'email-field
                     (clavier:valid-email))
