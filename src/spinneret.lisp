(defpackage "HTML-FORMS/SPINNERET"
  (:use #:cl
        #:alexandria
        #:spinneret
        #:html-forms))

(in-package "HTML-FORMS/SPINNERET")

(defmethod show ((button button) stream)
  (let ((*html* stream))
    (with-html
      (:li (:button :type (button-type button) (button-caption button))))))

(defmethod show ((form form) stream)
  (let ((*html* stream)
        (submit-button (make-instance 'button :type "submit" :caption (submit-caption form))))
    (with-html (:form :action (form-action form) :method (form-method form) :enctype (form-enctype form)
                      (:ul 
                       (loop :for field :in (form-fields form)
                             :do (show field stream))
                       (show submit-button stream))))))

(defmethod show ((field input-field) stream)
  (let ((*html* stream)
        (name (field-name field))
        (value (field-value field)))
    (with-html
      (:li
       (:label :for name (or (field-label field) (format nil "~@(~A~):" name)))
       (:input :name name :type (field-type field) :value value)
       (:span :class "error" (error-message field))))))

(defmethod show ((field textarea-field) stream)
  (let ((*html* stream)
        (name (field-name field))
        (value (field-value field)))
    (with-html
      (:li
       (:label :for name (or (field-label field) (format nil "~@(~A~):" name)))
       (:textarea :name name value)
       (:span :class "error" (error-message field))))))
