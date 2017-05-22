(in-package "HTML-FORMS")

(defclass form ()
  ((name :initarg :name
         :reader form-name
         :documentation "The name of the form.")
   (fields :initarg :fields
           :reader form-fields
           :documentation "The FIELDS the forms contains.")
   (validators :initarg :validator
               :initform (constantly t)
               :reader validator
               :documentation "?")
   ;; Should this fields be in a class named html-form instead?
   (submit-caption :initarg :submit
                   :initform "Submit"
                   :reader submit-caption
                   :documentation "The caption to be displayed in the submit button.")
   (method :initarg :method
           :initform :post
           :reader form-method
           :type :keyowrd
           :documentation "The HTTP verb to use on submission.")
   (action :initarg :action
           :initform ""
           :reader form-action
           :documentation "The URL to use when submitting the form.")
   (enctype :initarg :enctype
            :initform "application/x-www-form-urlencoded"
            :reader form-enctype
            :type (member "application/x-www-form-urlencoded"
                          "multipart/form-data"
                          "text/plain")
            :documentation "The structure to into which the form data will be converted to prior to submitting the form.")
   ;; Maybe move this to hunchentoot/wookie/etc integration?
   (on-success :initarg :on-success
               :reader on-success
               :documentation "handler to call when form validates correctly")))

(defclass field ()
  ((name :initarg :name
         :reader field-name)
   (label :initarg :label
          :initform nil
          :reader field-label)
   (validator :initarg :validator
              :initform (lambda (v) (declare (ignore v)) t)
              :reader validator
              :documentation "")
   (error-message :initform nil :accessor error-message
                  :documentation "The error message returned by the VALIDATOR function.") ; XXX: Should this be a list instead?
   default-value
   (value :initarg :value
          :initform nil ;; Should I instead use unbound as the no-value test?
          :accessor field-value
          :documentation "TODO: Is this the value passed?")
   (validp :initform :unknown
           :reader validp
           :type (member :unknown :invalid :valid)
           :documentation "Does the VALUE slot pass the validator?")
   ;; Instead use a validp?
   (safe-value :reader safe-value
               :documentation "The value of the field")))

(defmethod print-object ((obj field) stream)
  (let ((name (if (slot-boundp obj 'name)
                  (field-name obj)
                  "Anonymous")))
    (print-unreadable-object (obj stream :type t)
      (format stream "~A name" name))))

(defclass input-field (field)
  ((type :initarg :type
         :initform "text"
         :reader field-type
         :documentation "The type the field should be when being displayed with SHOW."))
  (:documentation "The class of the fields that correspond to the INPUT HTML element."))

(defclass text-field (input-field)
  ())

(defclass email-field (input-field)
  ((type :initform "email")))

(defclass password-field (input-field)
  ((type :initform "password")))

(defclass hidden-field (input-field)
  ())

(defclass file-field (input-field)
  ())

(defclass checkbox-field (input-field)
  ())

(defclass textarea-field (field)
  ())


(defclass button ()
  ((type :initarg :type :initform "submit" :reader button-type)
   (caption :initarg :caption :initform "" :reader button-caption))
  (:documentation "An HTML Button Element"))

;; field-set for multi-value fields


;; Display protocol
;; XXX: Maybe rename it to RENDER or DISPLAY?
(defgeneric show (form-or-field stream)
  (:documentation "Print the FORM-OR-FIELD object to the STREAM in a HTML
  representation. This function is similar to print-object except it is not
  intended to be readable for humans, hence unsuitable for REPL use."))


;; Validation Protocol
(defgeneric validate (form-or-field)
  (:documentation "Returns two values the first one is a list of errors and
  second one is the value returned by the object. ie. (values errors value)"))

(defun set-field-validator (field-name validator)
  "Set the validator to be used by the field named FIELD-NAME."
  (let ((class (find-class field-name)))
    (setf (c2mop:slot-definition-initform (find-slot 'validator class))
          validator)))

(defmethod validate ((field field))
  "Return T if the value is valid, an error message otherwise."
  (multiple-value-bind (validp error-message)
      (funcall (validator field) (field-value field))
    (setf (error-message field) error-message)
    (values validp error-message)))

(defmethod validate ((form form))
  "Return (VALUES ERROR-MESSAGES DATA) where DATA is map from field-names to
values."
  (let ((error-messages ())
        (data ()))
    (loop :for field :in (form-fields form)
          :for (field-valid? reason) := (multiple-value-list (validate field))
          :do
             (if field-valid?
                 (push (cons (field-name field) (field-value field))
                       data)
                 (push reason error-messages)))
    (funcall (validator form) data) ; Think this through

    (values (if (null error-messages)
                data
                nil)
            error-messages)))


;; Data binding

(defun get-field (form field-name)
  (find (symbol-name field-name) (form-fields form)
        :key (compose 'symbol-name 'field-name)
        :test 'string=))

;; (get-field (forms:make-entry) :title)
(defun bind-form (form data)
  (loop :for (field-name value) :in data
        :do (setf (field-value (get-field form field-name)) value)))

(defun form-data (form)
  (loop :for field :in (form-fields form)
        :collect (cons (field-name field)
                       (field-value field))))

;; Syntax

(defparameter +field-type-map+
  '((:text . text-field)
    (:textarea . textarea-field))
  "A map between the name used in DEFINE-FORM to refer to the type and the
  corresponding FIELD-TYPE.")

(defun field-type-abbrev-expander (abbreviation)
  (cdr (assoc abbreviation +field-type-map+)))

;; Lets try returning code as well
;; Don't include keywords when not provided
(defun parse-field-definition (field-name field-type-abbrev &key validator label)
  ;; XXX: Maybe use &rest args instead of key args and apply them?
  ;; TODO: Fix this, see DEFCLASS and CANONIZE-DEFCLASS-SLOTS for ideas
  `(make-instance ',(field-type-abbrev-expander field-type-abbrev) :name ',field-name
                                                                   ,@(when validator
                                                                       (list :validator validator))
                                                                   ,@(when label
                                                                       (list :label label))))

;; XXX: use guicho's lisp-namespace for form-constructors?

;; The constructor takes a keyword argument for each field to bind the value of
;; the field to it.
(defun expand-field-name (symbol)
  (list symbol nil (intern (build-symbol-name symbol '-provided-p))))

(defmacro define-form (name (&rest fields) &rest options)
  ;; Maybe the form constructor should be named new-<name>-form
  (let* ((constructor-name (intern (build-symbol-name 'make- name '-form)))
         (field-names (mapcar 'car fields))
         (constructor-keyword-arguments (mapcar 'expand-field-name field-names))
         (field-name-provided-predicates (mapcar 'third constructor-keyword-arguments))
         (form-action (second (assoc :action options))))
    (alexandria:with-gensyms (form)
      `(defun ,constructor-name (&key ,@constructor-keyword-arguments)
         (let ((,form (make-instance 'form :name ',name
                                           :fields (list ,@(mapcar (partial-apply 'parse-field-definition) fields))
                                           ,@(when form-action (list :action form-action)))))

           ;; Set the value of the fields if provided
           ,@(loop :for field-name :in field-names
                   :for field-name-provided-p :in field-name-provided-predicates
                   :collect `(when ,field-name-provided-p
                               (setf (field-value (get-field ,form ',field-name))
                                     ,field-name)))
           ,form)))))
