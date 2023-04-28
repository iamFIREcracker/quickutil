(in-package #:quickutil-utilities.utilities)

(defutil pr (:version (1 . 0)
             :category printing)
  "Prints all its `args` to screen. Returns the first arg."
  #>%%%>
  (defun pr (&rest args)
    %%DOC
    (format t "~{~A~}" args)
    (finish-output)
    (first args))
  %%%)

(defutil prn (:version (1 . 0)
              :category printing)
  "Prints all its `args` to screen, separated by a newline. Returns the first arg."
  #>%%%>
  (defun prn (&rest args)
    %%DOC
    (format t "~{~A~^~%~}" args)
    (finish-output)
    (first args))
  %%%)

(defutil prs (:version (1 . 0)
              :category printing)
  "Print all its `args` to screen, separated by a space. Returns the first arg."
  #>%%%>
  (defun prs (&rest args)
    %%DOC
    (format t "~{~A~^ ~}" args)
    (finish-output)
    (first args))
  %%%)

(defutil spr (:version (1 . 0)
              :category printing)
  "Prints all its `args` into a string, and return it."
  #>%%%>
  (defun spr (&rest args)
    %%DOC
    (format nil "~{~A~}" args))
  %%%)

(defutil sprn (:version (1 . 0)
               :category printing)
  "Prints all its `args` into a string, separated by a newline, and return it."
  #>%%%>
  (defun sprn (&rest args)
    %%DOC
    (format nil "~{~A~^~%~}" args))
  %%%)

(defutil sprs (:version (1 . 0)
               :category printing)
  "Print all its `args` into a string, separated by a space, and return it."
  #>%%%>
  (defun sprs (&rest args)
    %%DOC
    (format nil "~{~A~^ ~}" args))
  %%%)
