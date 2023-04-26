(in-package #:quickutil-utilities.utilities)

(defutil aif (:version (1 . 0)
              :depends-on let1
              :category (binding anaphoric))
  "Like IF, except binds the result of `test` to IT (via LET) for the scope of `then` and `else` expressions."
  #>%%%>
  (defmacro aif (test then &optional else)
    %%DOC
    (aif-expand test then else))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aif-expand (test then &optional else)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (if ,it ,then ,else)))))
  %%%)

(defutil awhen (:version (1 . 0)
                :depends-on let1
                :category (binding anaphoric))
  "Like WHEN, except binds the result of `test` to IT (via LET) for the scope of `body`."
  #>%%%>
  (defmacro awhen (test &body body)
    %%DOC
    (awhen-expand test body))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun awhen-expand (test body)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (when ,it
             ,@body)))))
  %%%)


(defutil aand (:version (1 . 0)
               :depends-on (let1 aif)
               :category (binding anaphoric))
  "Like AND, except binds the result of each form to IT (via LET)."
  #>%%%>
  (defmacro aand (&rest forms)
    %%DOC
    (aand-expand forms))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aand-expand (forms)
      (cond ((not (car forms)) nil)
            ((not (cdr forms)) (car forms))
            (t (let1 car (car forms)
                 `(aif ,car
                    (aand ,(cdr forms))))))))
  %%%)
