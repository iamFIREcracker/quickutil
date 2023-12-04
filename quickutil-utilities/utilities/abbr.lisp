(in-package #:quickutil-utilities.utilities)

(defutil if-not (:version (1 . 0)
                 :category control)
  "Like IF, except TEST gets wrapped inside NOT."
  #>%%%>
  (defmacro if-not (test then &optional else)
    %%DOC
    `(if (not ,test) ,then ,else))
  %%%)
