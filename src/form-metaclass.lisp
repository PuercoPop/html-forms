(in-package "HTML-FORMS")

#|

Should I add a slot for each of ACTION, ENCTYPE, METHOD, NAME, SUBMIT-CAPTION,
VALIDATORS (form-wide) and ON-SUCCESS?

|#

(defclass form-metaclass (c2mop:standard-class)
  ()
  (:documentation "The meta-class for form objects."))

(defmethod c2mop:validate-superclass ((class c2mop:standard-class) (super form-metaclass))
  t)


#+example
(defclass entry ()
  (title content)
  (:metaclass form-metaclass))
