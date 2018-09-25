(in-package "HTML-FORMS")


(defclass form-field-slot-defintion (c2mop:standard-slot-definition)
  ((name :initarg :name
         :reader field-name)
   (label :initarg :label
          :initform nil
          :reader field-label)
   (validator :initarg :validator
              :initform (constantly t)
              :reader validator
              :documentation "The function to determine if the value is
              valid. It takes as a parameter the value.")
   (error-message :initform nil :accessor error-message
                  :documentation "The error message returned by the VALIDATOR
                  function.")
   (validp :initform :unknown
           :reader validp
           :type (member :unknown :invalid :valid)
           :documentation "Does the VALUE slot pass the validator?")))

(defclass form-field-direct-slot (form-field-slot-definition
                                  c2mop:standard-direct-slot-definition)
  ())

(defclass form-field-effective-slot (form-field-slot-definition
                                     c2mop:standard-effective-slot-definition)
  ())

(defparameter +form-field-extendend-attributes+ '(name label validator error-message validp))

(defun form-field-extended-attribute-p (x)
  (member x +form-field-extendend-attributes+))

;; XXX: Code would be more straight forward if it wasn't meant to work on sequences
(defun andmap (predicate &rest sequences)
  (flet ((first-of (xs)
           (elt xs 0))
         (rest-of (xs)
           (subseq xs 1)))
    (if (funcall 'some 'null sequences)
        t
        (and (apply predicate (mapcar #'first-of sequences))
             (apply 'andmap predicate (mapcar #'rest-of sequences))))))

;; Only use our slot class if there is an attribute

(defmethod direct-slot-definition-class ((class form-metaclass) &rest initargs)
  (if (andmap 'form-field-extendend-attribute-p initarags)
      (find-class 'form-field-direct-slot)
      (find-class 'c2mop:standard-direct-slot-definition)))

(defmethod effective-slot-definition-class ((class form-metaclass) &rest initargs)
  (if (andmap 'form-field-extendend-attribute-p initarags)
      (find-class 'form-field-effective-slot)
      (find-class 'c2mop:standard-effective-slot-definition)))

(defmethod compute-effective-slot-definition ((class form-metaclass) slot-name direct-slot-defintion)
  (let ((effective-slot-definition (call-next-method)))
    effective-slot-definition))
