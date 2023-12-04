(in-package #:quickutil-utilities.utilities)

(defutil flet* (:version (1 . 0)
                :category (language abbr))
  "Like LABELS, but 1 character shorter.
Also, FLET* is to FLET what LET* is to LET."
  #>%%%>
  (defmacro flet* (&rest body)
    %%DOC
    `(labels ,@body))
  %%%)

(defutil if-not (:version (1 . 0)
                 :category control)
  "Like IF, except TEST gets wrapped inside NOT."
  #>%%%>
  (defmacro if-not (test then &optional else)
    %%DOC
    `(if (not ,test) ,then ,else))
  %%%)
