;;;; ablisp.asd

(asdf:defsystem #:ablisp
  :serial t
  :description "Describe ablisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:trivial-utf-8 #:meta-sexp #:cl-fad #:flexi-streams)
  :components ((:file "package")
               (:file "util/util")
               (:file "ablisp")))
