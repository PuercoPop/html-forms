(defpackage "HTML-FORMS-TESTS"
  (:use "CL"
        "HTML-FORMS"
        "PROVE"))
(in-package "HTML-FORMS-TESTS")

;; Some runnable examples
(setf prove.color:*enable-colors* t)

(plan 8)

(let ((text-field (make-instance 'text-field :name "Name"
                                             :value "John Doe"
                                             :validator (clavier:not-blank))))
  (prove.test:is-values (validate text-field) '(t nil))
  (setf (field-value text-field) "")
  (prove.test:is-values (validate text-field) '(nil "Should not be blank"))

  (prove:ok (cl-ppcre:scan "Should(\\s+?)not(\\s+?)be(\\s+?)blank" 
                    (with-output-to-string (out)
                      (show text-field out)))
            "Field validation error message should be SHOWn."))

(let ((email-field (make-instance 'text-field :name "Email"
                                              :value "pirata@gmail.com"
                                              :validator (clavier:valid-email))))
  (prove:is-values (validate email-field) '(t nil)))

(define-form entry
 ((title :text :validator (clavier:not-blank))
  (content :text)))

(let ((form (make-entry-form))) ; Luego la data en el constructor
  (bind-form form '((:title "José Olaya")
                    (:content "Six-Pack King")))
  (prove:ok (validate form)))

(let ((form (make-entry-form :title "José Olaya" :content "Six-Pack King")))
  (prove:ok (validate form)))

;; Fails validation
(let ((form (make-entry-form))) ; Luego la data en el constructor
  (bind-form form '((:title "")
                    (:content "Six-Pack King")))
  (prove:ok (not (validate form))))

;; Write a test case for form wide validation. Start with all the values are
;; not the same


(define-form foo
  ((bar :text)))

(ok (search "Bar:"
            (with-output-to-string (out)
              (show (make-foo-form) out)))
    "When a label is not provided default to the capitalized field name followed by a colon (:).")

(finalize)
