(in-package #:quickutil-utilities.utilities)

;; Make these inlinable by declaiming them INLINE here and some of them
;; NOTINLINE at the end of the file. Exclude functions that have a compiler
;; macro, because NOTINLINE is required to prevent compiler-macro expansion.
#+#:ignore
(declaim (inline copy-sequence sequence-of-length-p))

(defutil sequence-of-length-p (:version (1 . 0)
                               :depends-on array-bounds
                               :category (alexandria sequences))
  #>%%%>
  (defun sequence-of-length-p (sequence length)
    "Return true if `sequence` is a sequence of length `length`. Signals an error if
`sequence` is not a sequence. Returns `nil` for circular lists."
    (declare (type array-index length)
             (inline length)
             (optimize speed))
    (etypecase sequence
      (null
       (zerop length))
      (cons
       (let ((n (1- length)))
         (unless (minusp n)
           (let ((tail (nthcdr n sequence)))
             (and tail
                  (null (cdr tail)))))))
      (vector
       (= length (length sequence)))
      (sequence
       (= length (length sequence)))))
  %%%)

(defutil rotate (:version (1 . 0)
                 :depends-on proper-list-length/last-car
                 :category (alexandria sequences))
  "Returns a sequence of the same type as `sequence`, with the elements of
`sequence` rotated by `n`: `n` elements are moved from the end of the sequence to
the front if `n` is positive, and `-n` elements moved from the front to the end if
`n` is negative. `sequence` must be a proper sequence. `n` must be an integer,
defaulting to `1`.

If absolute value of `n` is greater then the length of the sequence, the results
are identical to calling `rotate` with

    (* (signum n) (mod n (length sequence))).

Note: the original sequence may be destructively altered, and result sequence may
share structure with it."
  #>%%%>
  (defun rotate-tail-to-head (sequence n)
    (declare (type (integer 1) n))
    (if (listp sequence)
        (let ((m (mod n (proper-list-length sequence))))
          (if (null (cdr sequence))
              sequence
              (let* ((tail (last sequence (+ m 1)))
                     (last (cdr tail)))
                (setf (cdr tail) nil)
                (nconc last sequence))))
        (let* ((len (length sequence))
               (m (mod n len))
               (tail (subseq sequence (- len m))))
          (replace sequence sequence :start1 m :start2 0)
          (replace sequence tail)
          sequence)))

  (defun rotate-head-to-tail (sequence n)
    (declare (type (integer 1) n))
    (if (listp sequence)
        (let ((m (mod (1- n) (proper-list-length sequence))))
          (if (null (cdr sequence))
              sequence
              (let* ((headtail (nthcdr m sequence))
                     (tail (cdr headtail)))
                (setf (cdr headtail) nil)
                (nconc tail sequence))))
        (let* ((len (length sequence))
               (m (mod n len))
               (head (subseq sequence 0 m)))
          (replace sequence sequence :start1 0 :start2 m)
          (replace sequence head :start1 (- len m))
          sequence)))

  (defun rotate (sequence &optional (n 1))
    %%DOC
    (if (plusp n)
        (rotate-tail-to-head sequence n)
        (if (minusp n)
            (rotate-head-to-tail sequence (- n))
            sequence)))
  %%%)

(defutil shuffle (:version (1 . 0)
                  :depends-on proper-list-length/last-car
                  :category (alexandria random sequences))
  "Returns a random permutation of `sequence` bounded by `start` and `end`.
Original sequece may be destructively modified, and share storage with
the original one. Signals an error if `sequence` is not a proper
sequence."
  #>%%%>
  (defun shuffle (sequence &key (start 0) end)
    %%DOC
    (declare (type fixnum start)
             (type (or fixnum null) end))
    (etypecase sequence
      (list
       (let* ((end (or end (proper-list-length sequence)))
              (n (- end start)))
         (do ((tail (nthcdr start sequence) (cdr tail)))
             ((zerop n))
           (rotatef (car tail) (car (nthcdr (random n) tail)))
           (decf n))))
      (vector
       (let ((end (or end (length sequence))))
         (loop for i from start below end
               do (rotatef (aref sequence i)
                           (aref sequence (+ i (random (- end i))))))))
      (sequence
       (let ((end (or end (length sequence))))
         (loop for i from (- end 1) downto start
               do (rotatef (elt sequence i)
                           (elt sequence (+ i (random (- end i)))))))))
    sequence)
  %%%)

(defutil random-elt (:version (1 . 0)
                     :depends-on (proper-list-length/last-car)
                     :category (alexandria random sequences))
  "Returns a random element from `sequence` bounded by `start` and
`end`. Signals an error if the `sequence` is not a proper non-empty
sequence, or if `end` and `start` are not proper bounding index
designators for `sequence`."
  #>%%%>
  (defun random-elt (sequence &key (start 0) end)
    %%DOC
    (declare (sequence sequence) (fixnum start) (type (or fixnum null) end))
    (let* ((size (if (listp sequence)
                     (proper-list-length sequence)
                     (length sequence)))
           (end2 (or end size)))
      (cond ((zerop size)
             (error 'type-error
                    :datum sequence
                    :expected-type `(and sequence (not (satisfies emptyp)))))
            ((not (and (<= 0 start) (< start end2) (<= end2 size)))
             (error 'simple-type-error
                    :datum (cons start end)
                    :expected-type `(cons (integer 0 (,end2))
                                          (or null (integer (,start) ,size)))
                    :format-control "~@<~S and ~S are not valid bounding index designators for ~
                                   a sequence of length ~S.~:@>"
                    :format-arguments (list start end size)))
            (t
             (let ((index (+ start (random (- end2 start)))))
               (elt sequence index))))))
  %%%)

(defutil removef (:version (1 . 0)
                  :category (alexandria sequences))
  "Modify-macro for `remove`. Sets place designated by the first argument to
the result of calling `remove` with `item`, place, and the `keyword-arguments`."
  #>%%%>
  (declaim (inline remove/swapped-arguments))
  (defun remove/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'remove item sequence keyword-arguments))

  (define-modify-macro removef (item &rest remove-keywords)
    remove/swapped-arguments
    %%DOC)
  %%%)

(defutil deletef (:version (1 . 0)
                  :category (alexandria sequences))
  "Modify-macro for `delete`. Sets place designated by the first argument to
the result of calling `delete` with `item`, place, and the `keyword-arguments`."
  #>%%%>
  (declaim (inline delete/swapped-arguments))
  (defun delete/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'delete item sequence keyword-arguments))

  (define-modify-macro deletef (item &rest remove-keywords)
    delete/swapped-arguments
    %%DOC)
  %%%)

(defutil proper-sequence (:version (1 . 0)
                          :depends-on proper-list
                          :category (alexandria sequences types))
  "Type designator for proper sequences, that is proper lists and sequences
that are not lists."
  #>%%%>
  (deftype proper-sequence ()
    %%DOC
    `(or proper-list
         (and (not list) sequence)))
  %%%)

#+#:ignore
(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if SEQUENCE
is not a sequence."
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defutil length= (:version (1 . 0)
                  :depends-on (array-bounds sequence-of-length-p with-gensyms)
                  :category (alexandria sequences))
  "Takes any number of sequences or integers in any order. Returns true iff
the length of all the sequences and the integers are equal.

Hint: there's a compiler macro that expands into more efficient code
if the first argument is a literal integer."
  #>%%%>
  (defun length= (&rest sequences)
    %%DOC
    (declare (dynamic-extent sequences)
             (inline sequence-of-length-p)
             (optimize speed))
    (unless (cdr sequences)
      (error "You must call LENGTH= with at least two arguments"))
    ;; There's room for optimization here: multiple list arguments could be
    ;; traversed in parallel.
    (let* ((first (pop sequences))
           (current (if (integerp first)
                        first
                        (length first))))
      (declare (type array-index current))
      (dolist (el sequences)
        (if (integerp el)
            (unless (= el current)
              (return-from length= nil))
            (unless (sequence-of-length-p el current)
              (return-from length= nil)))))
    t)

  (define-compiler-macro length= (&whole form length &rest sequences)
    (cond
      ((zerop (length sequences))
       form)
      (t
       (let ((optimizedp (integerp length)))
         (with-unique-names (tmp current)
           (declare (ignorable current))
           `(locally
                (declare (inline sequence-of-length-p))
              (let ((,tmp)
                    ,@(unless optimizedp
                        `((,current ,length))))
                ,@(unless optimizedp
                    `((unless (integerp ,current)
                        (setf ,current (length ,current)))))
                (and
                 ,@(loop
                     :for sequence :in sequences
                     :collect `(progn
                                 (setf ,tmp ,sequence)
                                 (if (integerp ,tmp)
                                     (= ,tmp ,(if optimizedp
                                                  length
                                                  current))
                                     (sequence-of-length-p ,tmp ,(if optimizedp
                                                                     length
                                                                     current)))))))))))))
  %%%)

(defutil copy-sequence (:version (1 . 0)
                        :category (alexandria sequences))
  "Returns a fresh sequence of `type`, which has the same elements as
`sequence`."
  #>%%%>
  (defun copy-sequence (type sequence)
    %%DOC
    (if (typep sequence type)
        (copy-seq sequence)
        (coerce sequence type)))
  %%%)

(defutil first-elt (:version (1 . 0)
                    :depends-on emptyp
                    :category (alexandria sequences setters))
  "Getter and setter for the first element of a sequence."
  #>%%%>
  (defun first-elt (sequence)
    "Returns the first element of SEQUENCE. Signals a type-error if SEQUENCE is
not a sequence, or is an empty sequence."
    ;; Can't just directly use ELT, as it is not guaranteed to signal the
    ;; type-error.
    (cond  ((consp sequence)
            (car sequence))
           ((and (typep sequence '(and sequence (not list))) (plusp (length sequence)))
            (elt sequence 0))
           (t
            (error 'type-error
                   :datum sequence
                   :expected-type '(and sequence (not (satisfies emptyp)))))))

  (defun (setf first-elt) (object sequence)
    "Sets the first element of SEQUENCE. Signals a type-error if SEQUENCE is
not a sequence, is an empty sequence, or if OBJECT cannot be stored in SEQUENCE."
    ;; Can't just directly use ELT, as it is not guaranteed to signal the
    ;; type-error.
    (cond ((consp sequence)
           (setf (car sequence) object))
          ((and (typep sequence '(and sequence (not list)))
                (plusp (length sequence)))
           (setf (elt sequence 0) object))
          (t
           (error 'type-error
                  :datum sequence
                  :expected-type '(and sequence (not (satisfies emptyp)))))))
  %%%)

(defutil last-elt (:version (1 . 0)
                   :depends-on (proper-sequence emptyp proper-list-length/last-car)
                   :category (alexandria sequences))
  "Getter and setter for the last element of a sequence."
  #>%%%>
  (defun last-elt (sequence)
    "Returns the last element of SEQUENCE. Signals a type-error if SEQUENCE is
not a proper sequence, or is an empty sequence."
    ;; Can't just directly use ELT, as it is not guaranteed to signal the
    ;; type-error.
    (let ((len 0))
      (cond ((consp sequence)
             (lastcar sequence))
            ((and (typep sequence '(and sequence (not list))) (plusp (setf len (length sequence))))
             (elt sequence (1- len)))
            (t
             (error 'type-error
                    :datum sequence
                    :expected-type '(and proper-sequence (not (satisfies emptyp))))))))

  (defun (setf last-elt) (object sequence)
    "Sets the last element of SEQUENCE. Signals a type-error if SEQUENCE is not a proper
sequence, is an empty sequence, or if OBJECT cannot be stored in SEQUENCE."
    (let ((len 0))
      (cond ((consp sequence)
             (setf (lastcar sequence) object))
            ((and (typep sequence '(and sequence (not list))) (plusp (setf len (length sequence))))
             (setf (elt sequence (1- len)) object))
            (t
             (error 'type-error
                    :datum sequence
                    :expected-type '(and proper-sequence (not (satisfies emptyp))))))))
  %%%)

(defutil starts-with-subseq (:version (1 . 0)
                             :depends-on (remove-from-plist)
                             :category (alexandria sequences))
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the functions returns, as a second value, a
displaced array pointing to the sequence after PREFIX."
  #>%%%>
  (defun starts-with-subseq (prefix sequence &rest args &key (return-suffix nil) &allow-other-keys)
    %%DOC
    (remove-from-plistf args :return-suffix)
    (let ((sequence-length (length sequence))
          (prefix-length (length prefix)))
      (if (<= prefix-length sequence-length)
          (let ((mismatch (apply #'mismatch prefix sequence args)))
            (if mismatch
                (if (< mismatch prefix-length)
                    (values nil nil)
                    (values t (when return-suffix
                                (make-array (- sequence-length mismatch)
                                            :element-type (array-element-type sequence)
                                            :displaced-to sequence
                                            :displaced-index-offset prefix-length
                                            :adjustable nil))))
                (values t (when return-suffix
                            (make-array 0 :element-type (array-element-type sequence)
                                          :adjustable nil)))))
          (values nil nil))))
  %%%)

(defutil ends-with-subseq (:version (1 . 0)
                           :category (alexandria sequences))
  "Test whether `sequence` ends with `suffix`. In other words: return true if
the last `(length suffix`) elements of `sequence` are equal to `suffix`."
  #>%%%>
  (defun ends-with-subseq (suffix sequence &key (test #'eql))
    %%DOC
    (let ((sequence-length (length sequence))
          (suffix-length (length suffix)))
      (when (< sequence-length suffix-length)
        ;; if SEQUENCE is shorter than SUFFIX, then SEQUENCE can't end with SUFFIX.
        (return-from ends-with-subseq nil))
      (loop for sequence-index from (- sequence-length suffix-length) below sequence-length
            for suffix-index from 0 below suffix-length
            when (not (funcall test (elt sequence sequence-index) (elt suffix suffix-index)))
              do (return-from ends-with-subseq nil)
            finally (return t))))
  %%%)

(defutil starts-with (:version (1 . 0)
                      :category (alexandria sequences))
  "Returns true if `sequence` is a sequence whose first element is `eql` to `object`.
Returns `nil` if the `sequence` is not a sequence or is an empty sequence."
  #>%%%>
  (defun starts-with (object sequence &key (test #'eql) (key #'identity))
    %%DOC
    (funcall test
             (funcall key
                      (typecase sequence
                        (cons (car sequence))
                        (sequence
                         (if (plusp (length sequence))
                             (elt sequence 0)
                             (return-from starts-with nil)))
                        (t
                         (return-from starts-with nil))))
             object))
  %%%)

(defutil ends-with (:version (1 . 0)
                    :depends-on proper-list-length/last-car
                    :category (alexandria sequences))
  "Returns true if `sequence` is a sequence whose last element is `eql` to `object`.
Returns `nil` if the `sequence` is not a sequence or is an empty sequence. Signals
an error if `sequence` is an improper list."
  #>%%%>
  (defun ends-with (object sequence &key (test #'eql) (key #'identity))
    %%DOC
    (funcall test
             (funcall key
                      (typecase sequence
                        (cons
                         ;; signals for improper lists
                         (lastcar sequence))
                        (sequence
                         ;; Can't use last-elt, as that signals an error
                         ;; for empty sequences
                         (let ((len (length sequence)))
                           (if (plusp len)
                               (elt sequence (1- len))
                               (return-from ends-with nil))))
                        (t
                         (return-from ends-with nil))))
             object))
  %%%)

(defutil map-combinations (:version (1 . 0)
                           :depends-on ensure-function
                           :category (alexandria sequences math))
  "Calls `function` with each combination of `length` constructable from the
elements of the subsequence of `sequence` delimited by `start` and `end`. `start`
defaults to `0`, `end` to length of `sequence`, and `length` to the length of the
delimited subsequence. (So unless `length` is specified there is only a single
combination, which has the same elements as the delimited subsequence.) If
`copy` is true (the default) each combination is freshly allocated. If `copy` is
false all combinations are `eq` to each other, in which case consequences are
specified if a combination is modified by `function`."
  #>%%%>
  (defun map-combinations (function sequence &key (start 0) end length (copy t))
    %%DOC
    (let* ((end (or end (length sequence)))
           (size (- end start))
           (length (or length size))
           (combination (subseq sequence 0 length))
           (function (ensure-function function)))
      (if (= length size)
          (funcall function combination)
          (flet ((call ()
                   (funcall function (if copy
                                         (copy-seq combination)
                                         combination))))
            (etypecase sequence
              ;; When dealing with lists we prefer walking back and
              ;; forth instead of using indexes.
              (list
               (labels ((combine-list (c-tail o-tail)
                          (if (not c-tail)
                              (call)
                              (do ((tail o-tail (cdr tail)))
                                  ((not tail))
                                (setf (car c-tail) (car tail))
                                (combine-list (cdr c-tail) (cdr tail))))))
                 (combine-list combination (nthcdr start sequence))))
              (vector
               (labels ((combine (count start)
                          (if (zerop count)
                              (call)
                              (loop for i from start below end
                                    do (let ((j (- count 1)))
                                         (setf (aref combination j) (aref sequence i))
                                         (combine j (+ i 1)))))))
                 (combine length start)))
              (sequence
               (labels ((combine (count start)
                          (if (zerop count)
                              (call)
                              (loop for i from start below end
                                    do (let ((j (- count 1)))
                                         (setf (elt combination j) (elt sequence i))
                                         (combine j (+ i 1)))))))
                 (combine length start)))))))
    sequence)
  %%%)

(defutil map-permutations (:version (1 . 0)
                           :depends-on map-combinations
                           :category (alexandria sequences math))
  "Calls function with each permutation of `length` constructable
from the subsequence of `sequence` delimited by `start` and `end`. `start`
defaults to `0`, `end` to length of the sequence, and `length` to the
length of the delimited subsequence."
  #>%%%>
  (defun map-permutations (function sequence &key (start 0) end length (copy t))
    %%DOC
    (let* ((end (or end (length sequence)))
           (size (- end start))
           (length (or length size)))
      (labels ((permute (seq n)
                 (let ((n-1 (- n 1)))
                   (if (zerop n-1)
                       (funcall function (if copy
                                             (copy-seq seq)
                                             seq))
                       (loop for i from 0 upto n-1
                             do (permute seq n-1)
                                (if (evenp n-1)
                                    (rotatef (elt seq 0) (elt seq n-1))
                                    (rotatef (elt seq i) (elt seq n-1)))))))
               (permute-sequence (seq)
                 (permute seq length)))
        (if (= length size)
            ;; Things are simple if we need to just permute the
            ;; full START-END range.
            (permute-sequence (subseq sequence start end))
            ;; Otherwise we need to generate all the combinations
            ;; of LENGTH in the START-END range, and then permute
            ;; a copy of the result: can't permute the combination
            ;; directly, as they share structure with each other.
            (let ((permutation (subseq sequence 0 length)))
              (flet ((permute-combination (combination)
                       (permute-sequence (replace permutation combination))))
                (declare (dynamic-extent #'permute-combination))
                (map-combinations #'permute-combination sequence
                                  :start start
                                  :end end
                                  :length length
                                  :copy nil)))))))
  %%%)

(defutil map-derangements (:version (1 . 0)
                           :category (alexandria sequences math))
  "Calls `function` with each derangement of the subsequence of `sequence` denoted
by the bounding index designators `start` and `end`. Derangement is a permutation
of the sequence where no element remains in place. `sequence` is not modified,
but individual derangements are `eq` to each other. Consequences are unspecified
if calling `function` modifies either the derangement or `sequence`."
  #>%%%>
  (defun map-derangements (function sequence &key (start 0) end (copy t))
    %%DOC
    (let* ((end (or end (length sequence)))
           (size (- end start))
           ;; We don't really care about the elements here.
           (derangement (subseq sequence 0 size))
           ;; Bitvector that has 1 for elements that have been deranged.
           (mask (make-array size :element-type 'bit :initial-element 0)))
      (declare (dynamic-extent mask))
      ;; ad hoc algorith
      (labels ((derange (place n)
                 ;; Perform one recursive step in deranging the
                 ;; sequence: PLACE is index of the original sequence
                 ;; to derange to another index, and N is the number of
                 ;; indexes not yet deranged.
                 (if (zerop n)
                     (funcall function (if copy
                                           (copy-seq derangement)
                                           derangement))
                     ;; Itarate over the indexes I of the subsequence to
                     ;; derange: if I != PLACE and I has not yet been
                     ;; deranged by an earlier call put the element from
                     ;; PLACE to I, mark I as deranged, and recurse,
                     ;; finally removing the mark.
                     (loop for i from 0 below size
                           do
                              (unless (or (= place (+ i start)) (not (zerop (bit mask i))))
                                (setf (elt derangement i) (elt sequence place)
                                      (bit mask i) 1)
                                (derange (1+ place) (1- n))
                                (setf (bit mask i) 0))))))
        (derange start size)
        sequence)))
  %%%)

#+#:ignore
(declaim (notinline sequence-of-length-p))

#+#:ignore
(define-condition no-extremum (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Empty sequence in ~S." 'extremum))))

(defutil extremum (:version (1 . 0)
                   :depends-on ensure-function
                   :category (alexandria sequences math))
  "Returns the element of `sequence` that would appear first if the subsequence
bounded by `start` and `end` was sorted using `predicate` and `key`.

`extremum` determines the relationship between two elements of `sequence` by using
the `predicate` function. `predicate` should return true if and only if the first
argument is strictly less than the second one (in some appropriate sense). Two
arguments `x` and `y` are considered to be equal if `(funcall predicate x y)`
and `(funcall predicate y x)` are both false.

The arguments to the `predicate` function are computed from elements of `sequence`
using the `key` function, if supplied. If `key` is not supplied or is `nil`, the
sequence element itself is used.

If `sequence` is empty, `nil` is returned."
  #>%%%>
  (defun extremum (sequence predicate &key key (start 0) end)
    %%DOC
    (let* ((pred-fun (ensure-function predicate))
           (key-fun (unless (or (not key) (eq key 'identity) (eq key #'identity))
                      (ensure-function key)))
           (real-end (or end (length sequence))))
      (cond ((> real-end start)
             (if key-fun
                 (flet ((reduce-keys (a b)
                          (if (funcall pred-fun
                                       (funcall key-fun a)
                                       (funcall key-fun b))
                              a
                              b)))
                   (declare (dynamic-extent #'reduce-keys))
                   (reduce #'reduce-keys sequence :start start :end real-end))
                 (flet ((reduce-elts (a b)
                          (if (funcall pred-fun a b)
                              a
                              b)))
                   (declare (dynamic-extent #'reduce-elts))
                   (reduce #'reduce-elts sequence :start start :end real-end))))
            ((= real-end start)
             nil)
            (t
             (error "Invalid bounding indexes for sequence of length ~S: ~S ~S, ~S ~S"
                    (length sequence)
                    :start start
                    :end end)))))
  %%%)
