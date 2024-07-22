(in-package #:quickutil-utilities.utilities)

(defutil execution-time (:version (1 . 0)
                         :category instrumentation)
  "Return the number of milliseconds it takes to execute `body`. Also
returns the result as the second value."
  #>%%%>
  (defmacro execution-time (&body body)
    %%DOC
    (let ((tm (gensym))
          (res (gensym)))
      ;; This is coded very particularly to be as accurate as possible.
      `(let* ((,tm (get-internal-real-time))
              (,res (progn ,@body))
              (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                          internal-time-units-per-second)))
         (values ,tm ,res))))
  %%%)

(defutil pmx (:version (1 . 0)
              :category instrumentation)
  "MACROEXPAND-1 and then PRETTY-PRINT `form`."
  #>%%%>
  (defmacro pmx (form)
    %%DOC
    `(pprint (macroexpand-1 ',form)))
  %%%)


(defutil dbg (:version (1 . 0)
              :category printing)
  "Print `args` to screen, separated by a space, and followed by a newline.
Returns the first arg."
  #>%%%>
  (defun dbg (&rest args)
    %%DOC
    (format t "~{~A~^ ~}" args)
    (terpri)
    (finish-output)
    (first args))
  %%%)

(defutil dbgl (:version (1 . 0)
               :depends-on (dbg)
               :category printing)
  "Print `args`, labeled, separated by a newline, and followed by a final
newline.  Returns the last arg. labeled and readably."
  #>%%%>
  (defmacro dbgl (&rest args)
    %%DOC
    `(prog1
       (progn ,@(mapcar (lambda (arg) `(dbg ',arg ,arg)) args))
       (terpri)
       (finish-output)))
  %%%)
