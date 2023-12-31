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
                :category (language control))
  "Executes `body` while `expression` is true."
  #>%%%>
  (defmacro while (expression &body body)
    %%DOC
    `(loop while ,expression do
       ,@body))
  %%%)

(defutil repeat (:version (1 . 0)
                 :category (language control))
  "Runs BODY N times."
  #>%%%>
  (defmacro repeat (n &body body)
    %%DOC
    `(loop repeat ,n do ,@body))
  %%%)

(defutil looping (:version (1 . 0)
                  :depends-on (with-gensyms symb)
                  :category (language misc))
  "Run `body` in an environment where the symbols COLLECT!, APPEND!, SUM!,
MULTIPLY!, COUNT!, MINIMIZE!, and MAXIMIZE! are bound to functions that can be
used to collect / append, sum, multiply, count, minimize or maximize things
respectively.

Mixed usage of COLLECT!/APPEND!, SUM!, MULTIPLY!, COUNT!, MINIMIZE! and
MAXIMIZE! is not supported.

Examples:

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (collect! i))))
  =>
  (1 3)

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (sum! i))))
  =>
  4

  (looping
    (dotimes (i 5)
      (count! (oddp i))))
  =>
  2

  (looping
    (dotimes (i 5)
      (sum! i)
      (count! (oddp i))))
  ;; Signals an ERROR: Cannot use COUNT! together with SUM!
  "
  #>%%%>
  (defmacro looping (&body body)
    %%DOC
    (with-gensyms (loop-type result last collect-last)
      (labels ((extract-loop-type (body)
                 (cond ((null body) nil)
                       ((symbolp body) (find body
                                             '(collect! append! sum! multiply! count! minimize! maximize!)
                                             :test #'string=))
                       ((consp body) (unless (and (symbolp (car body))
                                                  (string= (car body) 'looping))
                                       (or (extract-loop-type (car body))
                                           (extract-loop-type (cdr body)))))))
               (init-result (loop-type)
                 (ecase loop-type
                   ((collect! append! minimize! maximize) nil)
                   ((sum! count!) 0)
                   ((multiply!) 1))))
        (let* ((loop-type-value (extract-loop-type body))
               (result-value (init-result loop-type-value)))
          `(let* ((,loop-type ',loop-type-value)
                  (,result ,result-value)
                  (,last nil))
             (declare (ignorable ,last))
             (labels ((,collect-last (item)
                       (if (not ,last)
                         (prog1 (push item ,result)
                           (setf ,last ,result))
                         (prog1 (push item (cdr ,last))
                           (setf ,last (cdr ,last)))))
                      (,(symb "COLLECT!") (item)
                       (if (and ,loop-type (and (not (eql ,loop-type 'collect!))
                                                (not (eql ,loop-type 'append!)) ))
                         (error "Cannot use COLLECT! together with ~A" ,loop-type)
                         (,collect-last item)))
                      (,(symb "APPEND!") (item)
                       (if (and ,loop-type (and (not (eql ,loop-type 'collect!))
                                                (not (eql ,loop-type 'append!)) ))
                         (error "Cannot use APPEND! together with ~A" ,loop-type)
                         (progn
                           (setf ,result (append ,result item)
                                 ,last (last item))
                           item)))
                      (,(symb "SUM!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'sum!)))
                         (error "Cannot use SUM! together with ~A" ,loop-type)
                         (progn
                           (incf ,result item)
                           item)))
                      (,(symb "MULTIPLY!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'multiply!)))
                         (error "Cannot use MULTIPLY! together with ~A" ,loop-type)
                         (setf ,result (* ,result item))))
                      (,(symb "COUNT!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'count!)))
                         (error "Cannot use COUNT! together with ~A" ,loop-type)
                         (progn
                           (when item
                             (incf ,result)
                             item))))
                      (,(symb "MINIMIZE!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'minimize!)))
                         (error "Cannot use MINIMIZE1 together with ~A" ,loop-type)
                         (setf ,result (min (or ,result item) item))))
                      (,(symb "MAXIMIZE!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'maximize!)))
                         (error "Cannot use MAXIMIZE! together with ~A" ,loop-type)
                         (setf ,result (max (or ,result item) item)))))
               ,@body)
             ,result)))))
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
