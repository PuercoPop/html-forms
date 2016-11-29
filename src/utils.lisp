(in-package "HTML-FORMS")

(defun slots-of (class)
  "Return a list of slots of CLASS."
  (unless (c2mop:class-finalized-p class)
    (c2mop:ensure-finalized class))
  (c2mop:class-slots class))

(defun find-slot (slot-name class)
  (find slot-name (slots-of class) :key 'c2mop:slot-definition-name))

(defun partial-apply (function)
  "Partially apply FUNCTION as the first argument to APPLY."
  (lambda (x) (apply function x)))

(defun build-symbol-name (&rest things)
  "Similar to ALEXANDRIA:SYMBOLICATE except that it returns a string, not a symbol."
  ;; TODO: Find better name
  (flet ((to-string (x)
           (etypecase x
             (string x)
             (symbol (symbol-name x)))))
    (apply 'concatenate 'string (mapcar #'to-string things))))
