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
                  :depends-on (let1 aif with-gensyms)
                  :category (language misc))
  "Run `body` in an environment where the symbols COLLECT!, APPEND!, ADJOIN!,
SUM!, MULTIPLY!, COUNT!, MINIMIZE!, and MAXIMIZE! are bound to functions that
can be used to collect / append, sum, multiply, count, minimize or maximize
things respectively.

Mixed usage of COLLECT!/APPEND!/ADJOIN!, SUM!, MULTIPLY!, COUNT!, MINIMIZE! and
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
  (defparameter *looping-reduce-keywords*  '(collect! append! adjoin! sum! multiply! count! minimize! maximize!))

  (defun %extract-reduce-keywords (body)
    "Walk `body` and collect any symbol that matches any of the keywords inside
*LOOPING-REDUCE-KEYWORDS*"
    (cond ((null body) nil)
          ((symbolp body) (aif (find body *looping-reduce-keywords* :test #'string=)
                            (list it)))
          ((consp body) (unless (and (symbolp (car body))
                                     (string= (car body) 'looping))
                          (append (%extract-reduce-keywords (car body))
                                  (%extract-reduce-keywords (cdr body)))))))

  (defun %assert-compatible-reduce-keywords (keywords)
    "Assert LOOPING reduce functions `keywords` are compatible with one another
E.g. COLLECT! is compatible with APPEND!, or ADJOIN!, but not with SUM!"
    (flet ((incompatible-keyword! (k rest)
             (ecase k
               ((collect! append! adjoin!) (aif (find-if (lambda (k1) (and (not (eql k1 'collect!))
                                                                           (not (eql k1 'append!))
                                                                           (not (eql k1 'adjoin!))))
                                                         rest)
                                             (error "Cannot use ~A together with ~A" it k)))
               (sum! (aif (find 'sum! rest :test-not 'eq)
                       (error "Cannot use ~A together with ~A" it k)))
               (multiply! (aif (find 'multiply! rest :test-not 'eq)
                            (error "Cannot use ~A together with ~A" it k)))
               (count! (aif (find 'count! rest :test-not 'eq)
                         (error "Cannot use ~A together with ~A" it k)))
               (minimize! (aif (find 'minimize! rest :test-not 'eq)
                            (error "Cannot use ~A together with ~A" it k)))
               (maximize! (aif (find 'minimize! rest :test-not 'eq)
                            (error "Cannot use ~A together with ~A" it k))))))
      (loop for (k . rest) on keywords do (incompatible-keyword! k rest))))

  (defun %initialize-result (keywords)
    "Initialize the LOOPING return value
 E.g. when COLLECT!-ing, the initial value will be NIL; when SUM!-ing, the
 initial value will be 0"
    (ecase (car keywords)
      ((collect! append! adjoin! minimize! maximize!) nil)
      ((sum! count!) 0)
      ((multiply!) 1)))

  (defgeneric %expand-keyword-into-label (k result last)
    (:method ((k (eql 'collect!)) result last)
      `(,(intern "COLLECT!") (item)
         (if (not ,last)
           (prog1 (push item ,result)
             (setf ,last ,result))
           (prog1 (push item (cdr ,last))
             (setf ,last (cdr ,last))))))
    (:method ((k (eql 'append!)) result last)
      `(,(intern "APPEND!") (item)
         (setf ,result (append ,result item)
               ,last (last item))
         item))
    (:method ((k (eql 'adjoin!)) result last)
      `(,(intern "ADJOIN!") (item &rest adjoin-args)
         (setf ,result (apply #'adjoin item ,result adjoin-args))))
    (:method ((k (eql 'sum!)) result last)
      `(,(intern "SUM!") (item)
         (incf ,result item)))
    (:method ((k (eql 'multiply!)) result last)
      `(,(intern "MULTIPLY!") (item)
         (setf ,result (* ,result item))))
    (:method ((k (eql 'count!)) result last)
      `(,(intern "COUNT!") (item)
         (when item
           (incf ,result))))
    (:method ((k (eql 'minimize!)) result last)
      `(,(intern "MINIMIZE!") (item)
         (setf ,result (min (or ,result item) item))))
    (:method ((k (eql 'maximize!)) result last)
      `(,(intern "MAXIMIZE!") (item)
         (setf ,result (max (or ,result item) item)))))

  (defmacro looping (&body body)
    %%DOC
    (let1 keywords (remove-duplicates (%extract-reduce-keywords body))
      (%assert-compatible-reduce-keywords keywords)
      (with-gensyms (result last expand-fn)
        (let1 labels (mapcar (lambda (k) (%expand-keyword-into-label k result last)) keywords)
          `(let* ((,result ,(%initialize-result keywords))
                  (,last nil))
             (declare (ignorable ,last))
             (labels (,@labels)
               ,@body)
             ,result)))))
  %%%)

(defutil recursively (:version (1 . 0)
                      :category (language misc))
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

`bindings` should contain a list of symbols and (optional) starting values.

In `body` the symbol `recur` will be bound to the function for recurring."
  #>%%%>
  (defmacro recursively (bindings &body body)
    %%DOC
    (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
          (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
      `(labels ((,(intern "RECUR") (,@names)
                 ,@body))
         (,(intern "RECUR") ,@values))))
  %%%)

(defutil ~>> (:version (1 . 0)
              :compilation-depends-on (recursively)
              :category (language misc))
  "Threads the expr through the forms, like Clojure's `->>`.

While threading, for each element of `forms`:

- if a SYMBOL, it's converted into a function call with the accumulated value
as it's first argument
- if a function call already, the accumulated value is **appended** to the
list of args unless it contains the placeholder '~ (in which case '~ is
replaced with the accumulated value)

Examples:
(~>> 'World
  (list 'Hello))
=>
(HELLO WORLD)

(~>> 'World
  (list ~ 'Hello))
=>
(HELLO WORLD)

(~>> 'World
  (list ~ 'Hello)
  reverse)
=>
(HELLO WORLD)
  "
  #>%%%>
  (defmacro ~>> (x &rest forms)
    %%DOC
    (labels ((replace-or-append (old form new)
               (if (contains? old form)
                 (subst new old form)
                 (append form (list new))))
             (contains? (target form)
               (recursively ((form form))
                 (if (atom form)
                   (eq form target)
                   (or (recur (car form))
                       (recur (cdr form)))))))
      (let ((placeholder (intern "~")))
        (with-gensyms (result)
          `(let* ((,result ,x)
                  ,@(mapcar (lambda (form)
                              (if (atom form)
                                `(,result (,form ,result))
                                `(,result ,(replace-or-append placeholder
                                                              form
                                                              result))))
                            forms))
             ,result)))))
  %%%)


(defutil continuable (:version (1 . 0)
                      :depends-on (let1)
                      :category (language))
  "Wraps `body` in a RESTART-CASE with a CONTINUE restart. When invoked, the
restart will simply return NIL, allowing the program to continue execution.

Returns the value of the last form in body, or NIL if the CONTINUE restart is
invoked.

By default, the message reported by the restart case will be \"Continue.\".
This can be overridden by providing a :report form as the first element of the
body.

Examples:

  ;; Basic usage
  (continuable
    (format t \"This might fail~%\")
    (/ 1 0))

  ;; With custom report message
  (continuable
    (:report \"Ignore division by zero and continue\")
    (format t \"This might fail~%\")
    (/ 1 0))
"
  #>%%%>
  (defmacro continuable (&body body)
    %%DOC
    (let1 report "Continue."
      (if (eq (caar body) :report)
        (setf report (cadar body) body (nthcdr 2 body)))
      `(restart-case
         (progn ,@body)
         (continue () :report report))))
  %%%)

(defutil retriable (:version (1 . 0)
                    :depends-on (let1 aif)
                    :category (language))
  "Wraps `body` in a RESTART-CASE with a RETRY restart. When invoked, the
restart will re-execute the body forms until they return a non-NIL value.

Returns the first non-NIL value returned by the body forms.

By default, the message reported by the restart case will be \"Retry.\".  This
can be overridden by providing a :report form as the first element of the
body.

Examples:

  ;; Basic usage
  (retriable
    (let ((x (random 10)))
      (when (> x 5)
        x)))

  ;; With custom report message
  (retriable
    (:report \"Try again to get a number greater than 5\")
    (let ((x (random 10)))
      (when (> x 5)
        x)))
"
  #>%%%>
  (defmacro retriable (&body body)
    %%DOC
    (let1 report "Retry."
      (if (eq (caar body) :report)
        (setf report (cadar body) body (nthcdr 2 body)))
      (flet ((call-with-retry-restart (msg think)
               (loop (with-simple-restart (retry msg)
                       (return (funcall thunk))))))
        `(call-with-retry-restart ,msg (lambda () ,@body)))))
  %%%)
