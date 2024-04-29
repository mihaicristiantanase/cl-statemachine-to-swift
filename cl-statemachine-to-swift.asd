;;;; cl-statemachine-to-swift.asd

(asdf:defsystem #:cl-statemachine-to-swift
  :description "Generate a StateMachine class writted in Swift from a state machine described in Common Lisp."
  :author "Mihai Cristian Tănase <mihaicristian.tanase@gmail.com>"
  :license  "Specify license here"
  :version "0.9"
  :serial t
  :depends-on (:shell :cl-change-case)
  :components ((:file "package")
               (:file "utils")
               (:file "cl-statemachine-to-swift")))
