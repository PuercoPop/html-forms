(defpackage "HTML-FORMS"
  (:use #:cl)
  (:import-from #:alexandria
                #:compose)
  (:export
   #:form
   #:form-enctype
   #:form-method
   #:submit-caption
   #:form-fields
   
   #:validator
   #:error-message
   
   #:field
   #:field-label
   #:field-name

   #:text-field
   #:textarea-field
   #:password-field
   #:hidden-field
   #:file-field
   #:field-type
   #:field-value
   #:input-field
   #:form-action
   #:email-field

   #:button
   #:button-type
   #:button-caption
   
   ;; Extension points
   #:show

   ;; Validation protocol
   #:validate
   #:set-field-validator
   #:bind-form

   ;; Entry point
   #:define-form
   #:form-data))
