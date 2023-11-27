(in-package #:quickutil-utilities.utilities)

(defutil dorange (:version (1 . 0)
                  :category (language control))
  "Binds `var` to all the distinct values in the range [`from`, `to`[, with
`step` step (note: `to` is excluded), and runs `body` inside that
lexical environmnet."
  #>%%%>
  (defmacro dorange ((var from to &optional (step 1) (result nil result?)) &body body)
    %%DOC
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (>= ,var ,to-g) (<= ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  %%%)

(defutil dorangei (:version (1 . 0)
                   :category (language control))
  "Like DORANGE, `to` is inclusive (the range is: [`from`, `to`])."
  #>%%%>
  (defmacro dorangei ((var from to &optional (step 1) (result nil result?)) &body body)
    %%DOC
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (> ,var ,to-g) (< ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  %%%)

(defutil until (:version (1 . 0)
                :category (language control))
  "Executes `body` until `expression` is true."
  #>%%%>
  (defmacro until (expression &body body)
    %%DOC
    `(do ()
         (,expression)
       ,@body))
  %%%)

(defutil while (:version (1 . 0)
                :depends-on until
                :category (language control))
  "Executes `body` while `expression` is true."
  #>%%%>
  (defmacro while (expression &body body)
    %%DOC
    `(until (not ,expression)
       ,@body))
  %%%)

(defutil recursively (:version (1 . 0)
                      :depends-on let1
                      :category (language misc))
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

`bindings` should contain a list of symbols and (optional) starting values.

In `body` the symbol `recur` will be bound to the function for recurring."
  #>%%%>
  (defmacro recursively (bindings &body body)
    (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
          (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
      (let1 recur (intern "RECUR")
        `(labels ((,recur (,@names)
                    ,@body))
           (,recur ,@values)))))
  %%%)

(defutil if-not (:version (1 . 0)
                 :category control)
  "Like IF, except TEST gets wrapped inside NOT."
  #>%%%>
  (defmacro if-not (test then &optional else)
    %%DOC
    `(if (not ,test) ,then ,else))
  %%%)
