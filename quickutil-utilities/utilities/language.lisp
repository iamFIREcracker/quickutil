(in-package #:quickutil-utilities.utilities)

(defutil void (:version (1 . 0)
               :category (language misc))
  "Do absolutely nothing, and return absolutely nothing."
  #>%%%>
  (defun void (&rest args)
    %%DOC
    (declare (ignore args))
    (values))
  %%%)

(defutil ensure-boolean (:version (1 . 0)
                         :category (language misc))
  "Convert `x` into a Boolean value."
  #>%%%>
  (defun ensure-boolean (x)
    %%DOC
    (and x t))
  %%%)

(defutil letf* (:version (1 . 0)
                :depends-on (appendf zip)
                :category (language binding))
  "Given a list of `bindings` whose keys are places and whose values are forms, set them for the duration of `body`, but restore their values (as visible upon evaluation of this macro) upon completion. The restoration is ensured with `unwind-protect`."
  #>%%%>
  (defmacro letf* (bindings &body body &environment env)
    %%DOC
    (let (all-dummy-bindings
          all-news all-new-values all-getters all-setters
          gensyms)
      (loop
        :for (place value) :in bindings
        :do (multiple-value-bind
                  (dummy-names dummy-vals news setter getter)
                (get-setf-expansion place env)
              (appendf all-dummy-bindings (zip dummy-names dummy-vals))
              (push (car news) all-news)
              (push value all-new-values)
              (push (gensym (format NIL "old-~A" place)) gensyms)
              (push getter all-getters)
              (push setter all-setters)))
      `(let* ,all-dummy-bindings
         (let ,(zip gensyms all-getters)
           (unwind-protect
                (progn
                  ,@(nreverse (loop :for setter :in all-setters
                                    :for new :in all-news
                                    :for new-value :in all-new-values
                                    :collect `(let ((,new ,new-value))
                                                ,setter)))
                  ,@body)
             ,@(loop :for setter :in all-setters
                     :for new :in all-news
                     :for gensym :in gensyms
                     :collect `(let ((,new ,gensym))
                                 ,setter)))))))
  %%%)

(defutil let1 (:version (1 . 0)
               :category (language binding))
  "Bind VAR to VAL within BODY. Equivalent to LET with one binding."
  #>%%%>
  (defmacro let1 (var val &body body)
    %%DOC
    `(let ((,var ,val))
       ,@body))
  %%%)

(defutil bnd* (:version (1 . 0)
               :category (language binding))
  "Like LET*, but more powerful.

Use a symbol as the name of the binding to expand to a standard LET:

(bnd* (x
       (y (list 1 2 3)))
  (list x y)) ≡
(let (x)
  (let ((y (list 1 2 3)))
    (list x y)))

Use a list as the name of the binding to enable special type of expansions.

If the CAR of the list is the symbol VALUES, expand to MULTIPLE-VALUE-BIND
call:

(bnd* (((values f r) (floor 130 11)))
  (list f r)) ≡
(multiple-value-bind (f r)
     (floor 130 11)
   (list f r))

If the CAR of the list is the symbol WITH-SLOTS, expand to a WITH-SLOTS call:

(bnd* (((with-slots x y) thing))
  (incf x) (incf y))
≡
(with-slots (x y) thing
  (incf x) (incf y))

Otherwise, if the name of the binding is a list but none of the above applies,
BND* will expand to a DESTRUCTURING-BIND call:

(bnd* (((a b) '(1 2)))
  (list a b))
≡
(destructuring-bind (a b)
    '(1 2)
  (list a b))"
  #>%%%>
  (defmacro bnd* (bindings &body body)
    %%DOC
    (labels ((mklist (x) (if (atom x) (list x) x))
             (expand (bb)
               (cond ((null bb) (signal 'unexpected))
                     (t (let* ((b (mklist (car bb)))
                               (var (car b))
                               (val (cadr b)))
                          (cond ((symbolp var)
                                 `(let (,b)
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'values)
                                 `(multiple-value-bind ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'with-slots)
                                 `(with-slots ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                (t `(destructuring-bind ,@b
                                      ,@(if (rest bb)
                                          (list (expand (rest bb)))
                                          body)))))))))
      (expand bindings)))
  %%%)
#+#:excluded (bnd* (y
                     (x (list 1 2 3))
                     ((a b c) x)
                     ((values d e f) (values 4 5 6)))
               (list y x a b c d e f))


(defutil bnd1 (:version (1 . 0)
               :depends-on (bnd*)
               :category (language binding))
  "Equivalent to BND* with one binding."
  #>%%%>
  (defmacro bnd1 (binding &body body)
    %%DOC
    `(bnd* (,binding)
       ,@body))
  %%%)
#+#:excluded (bnd1 y y)
#+#:excluded (bnd1 (y (list 1 2 3)) y)
#+#:excluded (bnd1 ((a b c) (list 1 2 3)) (list a b c))
#+#:excluded (bnd1 ((values a b c) (values 1 2 3)) (list a b c))

(defutil defaccessor (:version (1 . 0)
                      :depends-on (parse-body with-gensyms)
                      :provides (defaccessor accesses)
                      :category language)
  "Define the function named `name` just as with a normal `defun`. Also define the setter `(setf name)`. The form to be set (i.e., the place) should be wrapped in the local macro `accesses`. For example,

```
  CL-USER> (let ((x 0))
             (defaccessor saved-x ()
               (accesses x)))
  SAVED-X
  (SETF SAVED-X)
  CL-USER> (saved-x)
  0
  CL-USER> (setf (saved-x) 5)
  5
  CL-USER> (saved-x)
  5
```
"
  #>%%%>
  (defmacro defaccessor (name lambda-list &body body)
    %%DOC
    (multiple-value-bind (remaining-forms decls doc)
        (parse-body body :documentation t)
      (with-gensyms (new-value)
        `(progn
           (defun ,name ,lambda-list
             ,doc
             ,@decls
             (macrolet ((accesses (form)
                          form))
               ,@remaining-forms))
         
           (defun (setf ,name) ,(cons new-value lambda-list)
             ,(format nil "Setter for the function ~S." name)
             ,@decls
             (macrolet ((accesses (form)
                          `(setf ,form ,',new-value)))
               ,@remaining-forms
               ,new-value))
           (values
            ',name
            '(setf ,name))))))
  %%%)
