# Design Goal

*html-forms* provides a way to display forms, validate and access the form
data.

# Design

TODO: Explain what are forms and fields
*html-forms* works with mainly two objects forms and fields. It is important to
remember that errors are returned in a list, so that if more than multiple
errors can be shown to the user, not only the first one encountered.

# Validation Protocol

There are two levels of validation: the form level and the field level.

On the field level each field validates the value associated with it.

The form first validates each field. If every field is successful then the form
calls `VALIDATE` with an alist of all names and values as its value to perform
form-wide validation. ej the either one of two fields must have a value.

The validation function takes one parameter as a value and returns two
values. The first one is a Boolean indicating if the value is valid or not, the
second one is a list of error messages.

# Display Protocol

The display protocol is comprised only of the show function, which takes an
object to display and a stream. It is similar to CL:PRINT-OBJECT but with.

# Extracting bound values

The prefer way to access form-data is by calling `validate`. The first value
`validate` returns, which indicates if the values are valid or not, returns a
plist in the form `(:<field-name> <value>)`.  Another way to access the form's data is through the `form-data` function, although it doesn't ensure tha the values are valid..

# Integration with Other libraries

To allow customization *html-forms* does as little as possible, only providing
a protocol and using other libraries to do the heavy lifting, currently it
provides integration with:

- Spinneret
- Clavier

## Hunchentoot

Handles setting up the validation endpoint for the form-field automatically
This feature is still being designed.

# Example use case: Login form

```lisp
(define-form login
 ((username text :validator (clavier:not-blank))
  (email email)
  (password password)
  (bio bio)))
```

# Form fields

- text-field
- textarea
- password-field
- email-field

## Defining a custom field

## Extending fields
## Customizing field behaviour

Take hints from:
- https://docs.djangoproject.com/en/1.10/topics/forms/
- https://docs.djangoproject.com/en/1.10/ref/forms/
