(in-package #:quickutil-utilities.utilities)


(defutil prog1-let (:version (1 . 0)
                    :depends-on let1
                    :category (binding anaphoric))
  "Like PROG1, except it lets you bind the result of the `result-form` (i.e., the returned
form) to `name` (via LET) for the scope of `body`.

Inspired by ActiveSupport: Object#returning
https://weblog.jamisbuck.org/2006/10/27/mining-activesupport-object-returning.html"
  #>%%%>
  (defmacro prog1-let ((name result-form) &body body)
    %%DOC
    (prog1-let-expand name result-form body))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun prog1-let-expand (name result-form body)
      `(let1 ,name ,result-form
         ,@body
         ,name)))
  %%%)
