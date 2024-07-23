(in-package #:quickutil-utilities.utilities)

(defutil flet* (:version (1 . 0)
                :category (language abbr))
  "Like LABELS, but 1 character shorter.
Also, FLET* is to FLET what LET* is to LET.

Note: cannot use ABBR for this, because LABELS is a special operator."
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

(defutil when-not (:version (1 . 0)
                   :category control)
  "Like WHEN, except TEST gets wrapped inside NOT."
  #>%%%>
  (defmacro when-not (test &body body)
    %%DOC
    `(when (not ,test) ,@body))
  %%%)


(defutil abbr (:version (1 . 0)
               :category (language abbr))
  "Defines a new function/macro named `short` and sharing
FDEFINITION/MACRO-FUNCTION with `long`."
  #>%%%>
  (defmacro abbr (short long)
    %%DOC
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         (t
           (error "Can't abbreviate ~a" ',long)))))
  %%%)

(defmacro defabbrutil (short long &rest util-spec)
  (let ((doc (format nil "Like ~A, but few chars shorter." (string-upcase long)))
        (version (getf util-spec :version '(1 . 0)))
        (depends-on (cons 'abbr (getf util-spec :depends-on)))
        (compilation-depends-on (getf util-spec :compilation-depends-on))
        (category (getf util-spec :category '(language abbr)))
        (body (format nil "  ~((abbr ~a ~a)~)" short long)))
    `(defutil ,short (:version ,version
                      :depends-on ,depends-on
                      :compilation-depends-on ,compilation-depends-on
                      :category ,category)
       ,doc
       ,body)))

(defabbrutil d-b destructuring-bind)
(defabbrutil m-v-b multiple-value-bind)
(defabbrutil w/slots with-slots)
(defabbrutil w/gensyms with-gensyms :depends-on (with-gensyms))

(defabbrutil split split-sequence :compilation-depends-on (split-sequence))
(defabbrutil split-if split-sequence-if :compilation-depends-on (split-sequence))
(defabbrutil split-if-not split-sequence-if-not :compilation-depends-on (split-sequence))

(defabbrutil keep-if remove-if-not)
(defabbrutil keep-if-not remove-if)

(defabbrutil while-not until :depends-on (until))
