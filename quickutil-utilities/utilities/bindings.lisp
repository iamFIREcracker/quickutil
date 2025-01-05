(in-package #:quickutil-utilities.utilities)


(defutil prog1-let (:version (1 . 0)
                     :depends-on let1
                     :category binding)
  "Evaluates `result-form`, binds its value to `name`, executes `body`, and
finally return `name`.  Similar to PROG1 but maintains the binding of
the result throughout `body`'s execution.

Example:

    (prog1-let x (list 1 2 3)
       (setf (first x) 10)    ; modifies the list
       (print x))             ; prints (10 2 3)
    => (10 2 3)               ; returns the modified list

    (prog1-let x (make-hash-table)
       (setf (gethash 'hello x) t)                   ; modifies the hash-table
       (setf (gethash 'world x) t)                   ; modifies it again
    => #<HASH-TABLE :TEST EQL :COUNT 2 {700AB03913}> ; returns the initialized hash-table

Inspired by ActiveSupport's Object#returning from Ruby on Rails:
https://weblog.jamisbuck.org/2006/10/27/mining-activesupport-object-returning.html"
  #>%%%>
  (defmacro prog1-let (name result-form &body body)
    %%DOC
    (prog1-let-expand name result-form body))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun prog1-let-expand (name result-form body)
      `(let1 ,name ,result-form
         ,@body
         ,name)))
  %%%)

#+#:excluded (defutil prog1-let (:version (1 . 0)
                                 :category binding)
               "Creates a lexical environment with `result-bindings`, executes `body`,
and returns a list of all bound values.

The bindings are established simultaneously via LET (not sequentially like LET*).
Each binding can be either a symbol (bound to NIL) or a (symbol value-form) pair.

Unlike PROG1, which returns only a single value, PROG1-LET collects and returns
all bindings as a list in the order they were defined.

Examples:

    ;; Basic usage with value forms
    (prog1-let ((x (+ 1 2))
                (y (list 'a 'b)))
      (print (list x y)))      ; prints (3 (A B))
    => (3 (A B))               ; returns list of bound values

    ;; With symbol-only bindings and side effects
    (prog1-let (result1
                (result2 \"initial\"))
      (setf result1 42
            result2 \"modified\"))
    => (42 \"modified\")       ; returns both values as a list
"
               #>%%%>
               (defmacro prog1-let (result-bindings &body body)
                 %%DOC
                 (prog1-let-expand result-bindings body))

               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defun prog1-let-expand (result-bindings body)
                   (flet ((var-name (binding) (if (atom binding) binding (car binding))))
                     `(let (,@result-bindings)
                        ,@body
                        (list ,@(mapcar #'var-name result-bindings))))))
               %%%)


#+#:excluded (defutil multiple-value-prog1-let (:version (1 . 0)
                                                :category binding)
               "Creates a lexical environment with `result-bindings`, executes `body`, and
returns all bound values as multiple values using VALUES.

The bindings are established simultaneously via LET (not sequentially like LET*).
Each binding can be either a symbol (bound to NIL) or a (symbol value-form) pair.

Examples:

    ;; Basic usage returning multiple values
    (multiple-value-prog1-let ((x (+ 1 2))
                              (y (list 'a 'b)))
      (print (list x y)))      ; prints (3 (A B))
    => 3                       ; first value
    => (A B)                   ; second value

    ;; Capturing multiple values with MULTIPLE-VALUE-BIND
    (multiple-value-bind (result1 result2)
        (multiple-value-prog1-let ((x 42)
                                  (y \"hello\"))
          (setf x (* x 2)
                y (string-upcase y)))
      (list result1 result2))
    => (84 \"HELLO\")          ; shows both values were captured
"
               #>%%%>
               (defmacro multiple-value-prog1-let (result-bindings &body body)
                 %%DOC
                 (prog1-let-expand result-bindings body))

               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defun prog1-let-expand (result-bindings body)
                   (flet ((var-name (binding) (if (atom binding) binding (car binding))))
                     `(let (,@result-bindings)
                        ,@body
                        (values ,@(mapcar #'var-name result-bindings))))))
               %%%)
