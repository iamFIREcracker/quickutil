(in-package #:quickutil-utilities.utilities)

(defutil range (:version (1 . 0)
                :category lists)
  "Return the list of numbers `n` such that `start <= n < end` and
`n = start + k*step` for suitable integers `k`. If a function `key` is
provided, then apply it to each number."
  #>%%%>
  (defun range (start end &key (step 1) (key 'identity))
    %%DOC
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i)))
  %%%)

(defutil replicate (:version (1 . 0)
                    :category lists)
  "Make a list of `n` copies of `x`."
  #>%%%>
  (defun replicate (n x)
    %%DOC
    (make-list n :initial-element x))
  %%%)

;;; XXX: Make generic? Rename?
(defutil slice (:version (1 . 0)
                :category lists)
  "Compute the slice of a list `list` at indexes `indexes`."
  #>%%%>
  (defun slice (list indexes)
    %%DOC
    (loop
      :for i :in indexes
      :collect (nth i list)))
  %%%)

(defutil transpose (:version (1 . 0)
                    :category lists)
  "Analog to matrix transpose for a list of lists given by `lists`."
  #>%%%>
  (defun transpose (lists)
    %%DOC
    (apply #'mapcar #'list lists))
  %%%)

(defutil zip (:version (1 . 0)
              :depends-on transpose
              :category lists)
  "Take a tuple of lists and turn them into a list of
tuples. Equivalent to `unzip`."
  #>%%%>
  (defun zip (&rest lists)
    %%DOC
    (transpose lists))
  %%%)

(defutil unzip (:version (1 . 0)
                :depends-on transpose
                :category lists)
  "Take a list of tuples and return a tuple of lists. Equivalent to
`zip`."
  #>%%%>
  (defun unzip (&rest lists)
    %%DOC
    (transpose lists))
  %%%)

(defutil long-zip (:version (1 . 0)
                   :depends-on zip
                   :category lists)
  "`zip` using the longest, rather than shortest list, filling with
`fill`."
  #>%%%>
  (defun long-zip (fill &rest lists)
    %%DOC
    (let ((longest (reduce #'max lists :key #'length)))
      (apply #'zip (loop
                     :for i :in lists
                     :collect (append i (make-list (- longest (length i))
                                                   :initial-element fill))))))
  %%%)

(defutil enumerate (:version (1 . 0)
                    :category lists)
  "Equivalent to `(zip (iota (length x)) x)`."
  #>%%%>
  (defgeneric enumerate (x &key start)
    (:documentation %%DOC))

  (defmethod enumerate ((x list) &key (start 0))
    %%DOC
    (loop
      :for i :in x
      :for j :from start
      :collect (list j i)))

  (defmethod enumerate ((x array) &key (start 0))
    %%DOC
    (loop
      :for i :across x
      :for j :from start
      :collect (list j i)))
  %%%)

(defutil flatten-once (:version (1 . 0)
                       :category lists)
  "Flatten `list` once."
  #>%%%>
  (defun flatten-once (list)
    %%DOC
    (loop :for x :in list
          :if (listp x)
            :append x
          :else
            :collect x))
  %%%)

(defutil flatten-tagged-once (:version (1 . 0)
                              :category lists)
  "Flatten once a list `x` with a tag `tag`."
  #>%%%>
  (defun flatten-tagged-once (tag x)
    %%DOC
    (flet ((ensure (x)
             (if (and (consp x)
                      (eq tag (car x)))
                 x
                 (list x)))
           (concat (x y)
             (list* tag (append (cdr x) (cdr y)))))
      (reduce #'concat x :key #'ensure)))
  %%%)

(defutil flatten (:version (1 . 0)
                  :category lists)
  "Flatten (and append) all lists `xs` completely."
  #>%%%>
  (defun flatten (&rest xs)
    %%DOC
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil)))
  %%%)

(defutil ncycle (:version (1 . 0)
                 :category lists)
  "Mutate `list` into a circlular list."
  #>%%%>
  (defun ncycle (list)
    %%DOC
    (nconc list list))
  %%%)

(defutil cycle (:version (1 . 0)
                :depends-on ncycle
                :category lists)
  "Make `list` into a circular list."
  #>%%%>
  (defun cycle (list)
    %%DOC
    (and list
         (ncycle (copy-list list))))
  %%%)

(defutil nest (:version (1 . 0)
               :category lists)
  "Compute a `count` compositions of `function` on `initial-value`."
  #>%%%>
  (defun nest (function initial-value count)
    %%DOC
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :finally (return y)))
  %%%)

(defutil nest-list (:version (1 . 0)
                    :category lists)
  "Compute a list of `count` compositions of `function` on `initial-value`."
  #>%%%>
  (defun nest-list (function initial-value count)
    %%DOC
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :collect y))
  %%%)

(defutil safe-nth (:version (1 . 0)
                   :category lists)
  "Find the `n`th element of `list`. If `n` is out of bounds, return
`if-out-of-bounds` (`nil` by default)."
  #>%%%>
  (defun safe-nth (n list &optional if-out-of-bounds)
    %%DOC
    (let ((nthcdr (nthcdr n list)))
      (if (endp nthcdr)
          if-out-of-bounds
          (car nthcdr))))
  %%%)

(defutil mapply (:version (1 . 0)
                 :category lists)
  "Apply `f` to each list of arguments contained within `list` and collect
the results."
  #>%%%>
  (defun mapply (f list)
    %%DOC
    (mapcar #'(lambda (x) (apply f x)) list))
  %%%)

(defutil cartesian-product (:version (1 . 0)
                            :category lists)
  "Compute the cartesian product of `l1` and `l2` as if they were
sets. Optionally, map the function `f` across the product."
  #>%%%>
  (defun cartesian-product (l1 l2 &optional (f 'cl:list))
    %%DOC
    (loop
      :for i :in l1
      :appending (loop
                   :for j :in l2
                   :collecting (funcall f i j))))
  %%%)

;;; TODO: Define a SETF method for END.
(defutil end (:version (1 . 0)
              :category lists)
  "Return the last element of `list` and whether or not it was
  empty."
  #>%%%>
  (defun end (list)
    %%DOC
    (values (car (last list)) (null list)))
  %%%)

(defutil tabulate (:version (1 . 0)
                   :depends-on range
                   :category lists)
  "Return a list evaluations of `f` over the integers `[0,n)`. Mimics the
SML function of the same name."
  #>%%%>
  (defun tabulate (f n)
    %%DOC
    (range 0 n :key f))
  %%%)

(defutil collect-reduce (:version (1 . 0)
                         :category lists)
  "Collects intermediate reduction results of applying `f` to `list`. More
  or less equivalent to `(loop :for i :in list :collect (reduce f i))`."
  #>%%%>
  (defun collect-reduce (f list &key (initial-value (car list) initial-value-p))
    %%DOC
    (loop
      :for j :in (if (not initial-value-p) (cdr list) list)
      :for r := (funcall f initial-value j) :then (funcall f r j)
      :collect r))
  %%%)

(defutil weave (:version (1 . 0)
                :category lists)
  "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
  #>%%%>
  (defun weave (&rest lists)
    %%DOC
    (apply #'mapcan #'list lists))
  %%%)

;;; Author: Paul Khuong (github: pkhuong)
(defutil interleave (:version (1 . 0)
                     :category lists)
  "Return a list whose elements alternate between each of the lists
  `lists`. When a list has been exhausted, interleaving continues with
  whatever other non-empty lists."
  #>%%%>
  (defun interleave (&rest lists)
    %%DOC
    (loop :while (some #'identity lists)
          :nconc (loop :for list-head :on lists
                       :for list := (first list-head)
                       :when list
                         :collect (pop (first list-head)))))
  %%%)

(defutil riffle (:version (1 . 0)
                 :category lists)
  "Insert the item `obj` in between each element of `list`."
  #>%%%>
  (defun riffle (list obj)
    %%DOC
    (loop :for (x . xs) :on list
          :collect x
          :when xs
            :collect obj))
  %%%)

(defutil extend (:version (1 . 0)
                 :category lists)
  "Adjoin `x` to the end of `xs`."
  #>%%%>
  (defun extend (xs x)
    %%DOC
    (append xs (list x)))
  %%%)

(defutil list-to-vector (:version (1 . 0)
                         :category lists)
  "Convert `list` into a vector."
  #>%%%>
  (defun list-to-vector (list)
    %%DOC
    (check-type list list)
    (coerce list 'vector))
  %%%)

(defutil sequence-to-list (:version (1 . 0)
                           :category lists)
  "Convert the sequence `seq` into a list."
  #>%%%>
  (defun sequence-to-list (seq)
    %%DOC
    (check-type seq sequence)
    (coerce seq 'list))
  %%%)

(defutil explode (:version (1 . 0)
                  :depends-on sequence-to-list
                  :category lists)
  "The classic `explode` function. Take a string and return a list of
  its characters."
  #>%%%>
  (defun explode (string)
    %%DOC
    (check-type string string)
    (sequence-to-list string))
  %%%)

(defutil implode (:version (1 . 0)
                  :depends-on list-to-vector
                  :category lists)
  "The classic `implode` function. Take a list of characters and return
  the corresponding string."
  #>%%%>
  (defun implode (list-of-characters)
    %%DOC
    (check-type list-of-characters list)
    (coerce list-of-characters 'string))
  %%%)

(defutil inits (:version (1 . 0)
                :category lists)
  "Generate a list of initial sublists of the list `list`. The name
`inits` comes from the Haskell function of the same name.

Example:

    > (inits '(a b c d))
    (NIL (A) (A B) (A B C) (A B C D))"
  #>%%%>
  (defun inits (list)
    %%DOC
    (cons nil (nreverse (maplist #'reverse (reverse list)))))
  %%%)

(defutil tails (:version (1 . 0)
                :category lists)
  "Generate a list of tails of the list `list`. The name `tails`
comes from the Haskell function of the same name.

Example

    > (tails '(a b c d))
    ((A B C D) (B C D) (C D) (D) NIL)"
  ;; This is *almost* equivalent to (maplist #'identity list)
  #>%%%>
  (defun tails (list)
    %%DOC
    (loop :collect list
          :while list
          :do (pop list)))
  %%%)


(defutil mklist (:version (1 . 0)
                 :category lists)
  "If not already a list, mklist will return a
   new list with its param as element"
  #>%%%>
  (defun mklist (obj)
    %%DOC
    (if (listp obj)
      obj
      (list obj)))
  %%%)

(defutil dolists (:version (1 . 0)
                  :category lists)
  "Like DOLIST, except it allows you to iterate over multiple lists in parallel.

  > (let ((list '(1 2 3 4)))
      (dolists ((x1 list)
                (x2 (cdr list)))
        (print (list x1 x2))))
  ;; (1 2)
  ;; (2 3)
  ;; (3 4)
  NIL
  "
  #>%%%>
  (defmacro dolists (((var1 list1) (var2 list2) &rest var-list-specs) &body body)
    %%DOC
    `(loop
       :for ,var1 :in ,list1 :for ,var2 :in ,list2
       ,@(loop for (var list) in var-list-specs
               collect 'FOR collect var collect 'IN collect list)
       :do ,@body))
  %%%)


(defutil dosublists (:version (1 . 0)
                     :category lists)
  "Like DOLIST, except:

- `var` is bound to successive sublists of `list` (similar to MAPL, LOOP..ON)
- `var` can be a lambda-list
"
  #>%%%>
  (defmacro dosublists ((var list &optional (result nil result?)) &body body)
    %%DOC
    `(loop :for ,var :on ,list :do ,@body ,@(when result? `(:finally (return ,result)))))
  %%%)

(defutil doesublists (:version (1 . 0)
                      :category lists)
  "Executes `body` once for each sublist of `list`, with `var` bound to the sublist,
and `count` bound to increasing integer values starting from 0.  Then `result`
is returned.

Note: it's possible to change the count start value by passing in a LIST,
instad of a symbol, where the first element is the name of count variable, and
the second is the count start value.

Note: DOESUBLISTS expands to a LOOP form, so `var` can either be a symbol, or a
lambda-list.

Examples:

    ;; Count starting from 0
    (doeseq (i x '(a b c d)) (prin1 i) (princ \" \") (prin1 x) (princ \" \"))
    >> 0 A 1 B 2 C 3 D
    => NIL

    ;; Custom count start
    (doeseq ((i 1) x '(a b c d)) (prin1 i) (princ \" \") (prin1 x) (princ \" \"))
    >> 1 A 2 B 3 C 4 D
    => NIL
"
  #>%%%>
  (defmacro doesublists ((count var list &optional (result nil result?)) &body body)
    %%DOC
    (once-only (list)
      (let ((count-var (if (atom count) count (car count)))
            (count-start (if (atom count) 0 (cadr count))))
        `(loop
           :for ,count-var :from ,count-start
           :for ,var :on ,list :do
           ,@body
           ,@(when result? `(:finally (return ,result)))))))
  %%%)


(defutil plist-keys (:version (1 . 0)
                     :category (lists plists))
  "Return all the keys of `plist`."
  #>%%%>
  (defun plist-keys (plist)
    %%DOC
    (loop for k in plist by #'cddr collect k))
  %%%)

(defutil plist-values (:version (1 . 0)
                       :category (lists plists))
  "Return all the values of `plist`."
  #>%%%>
  (defun plist-values (plist)
    %%DOC
    (loop for v in (cdr plist) by #'cddr collect v))
  %%%)


(defutil doplist (:version (1 . 0)
                  :depends-on (once-only)
                  :category (lists plists))
  "Iterates over the elements of `plist`."
  #>%%%>
  (defmacro doplist ((key val plist &optional (result nil result?)) &body body)
    %%DOC
    (once-only (plist)
      `(loop
         :for ,key :in ,plist :by #'cddr
         :for ,val :in (cdr ,plist) :by #'cddr
         :do ,@body ,@(when result? `(:finally (return ,result))))))
  %%%)

(defutil alist (:version (1 . 0)
                :category (lists alists))
  "Create an association list from a flat list of values."
  #>%%%>
  (defun alist (key value &rest key-values)
    (list* (cons key value)
           (loop :for (key value) :on key-values :by #'cddr
                 :collect (cons key value))))
  %%%)

(defutil alist-keys (:version (1 . 0)
                     :category (lists alists))
  "Return all the keys of `alist`."
  #>%%%>
  (defun alist-keys (alist)
    %%DOC
    (mapcar #'car alist))
  %%%)

(defutil alist-values (:version (1 . 0)
                       :category (lists alists))
  "Return all the values of `alist`."
  #>%%%>
  (defun alist-values (alist)
    %%DOC
    (mapcar #'cdr alist))
  %%%)


(defutil doalist (:version (1 . 0)
                  :depends-on (once-only)
                  :category (lists alists))
  "Iterates over the elements of `alist`."
  #>%%%>
  (defmacro doalist ((key val alist &optional (result nil result?)) &body body)
    %%DOC
    (once-only (alist)
      `(loop :for (,key . ,val) :in ,alist :do ,@body ,@(when result? `(:finally (return ,result))))))
  %%%)

