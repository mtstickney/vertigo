;;;; vertigo.asd

(asdf:defsystem #:vertigo
  :serial t
  :description "Describe vertigo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:meta-sexp #:lisp-unit #:alexandria #:cl-persist #:optima)
  :components ((:file "package")
               (:file "util")
               (:file "parser")
               (:file "ast")
               (:module parser-rules
                        :pathname "parser"
                        :components ((:file "other")
                                     (:file "functions")
                                     (:file "operators")
                                     (:file "phrases")
                                     (:file "statements")))
               (:file "tests")))

(defmethod asdf:perform :around ((op asdf:compile-op) (component asdf:cl-source-file))
  ;; Need a lot of macroexpansion room for meta-sexp rules
  (let (#+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
        #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))
    (call-next-method)))
