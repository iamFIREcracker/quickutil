;;;; quickutil-utilities.asd

(asdf:defsystem #:quickutil-utilities
  :serial t
  :description "The utilities and backend to Quickutil."
  :author "Robert Smith <quad@symbo1ics.com> & contributors"
  :license "See LICENSE file."
  :depends-on ("cl-heredoc")
  :components (
               ;; License: BSD 3-clause
               (:static-file "LICENSE")
               (:file "package")
               (:file "queue")
               (:file "defutil")
               (:file "setup-reader")
               ;; License: None. (Public Domain)
               (:module utilities
                :serial t
                :components ((:static-file "LICENSE")
                             (:file "types")
                             (:file "arithmetic")
                             (:file "numbers")
                             (:file "primes")
                             (:file "trigonometry")
                             (:file "sequences")
                             (:file "lists")
                             (:file "strings")
                             (:file "combinators")
                             (:file "arrays")
                             (:file "instrumentation")
                             (:file "trees")
                             (:file "symbols")
                             (:file "language")
                             (:file "anaphoric")
                             (:file "printing")
                             (:file "constants")
                             (:file "control")
                             (:file "hash-tables")
                             (:file "random")
                             (:file "generic")
                             (:file "conses")
                             (:file "conditions")
                             (:file "synonyms")
                             (:file "io")
                             (:file "syntax")
                             (:file "abbr")
                             (:module split-sequence
                              :serial t
                              :components ((:file "split-sequence")))
                             (:module alexandria
                              :serial t
                              :components ((:file "definitions")
                                           (:file "binding")
                                           (:file "strings")
                                           (:file "conditions")
                                           (:file "hash-tables")
                                           (:file "symbols")
                                           (:file "macros")
                                           (:file "control-flow")
                                           (:file "features")
                                           (:file "functions")
                                           (:file "types")
                                           (:file "arrays")
                                           (:file "lists")
                                           (:file "io")
                                           (:file "sequences")
                                           (:file "numbers")))))))
