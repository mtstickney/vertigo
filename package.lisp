;;;; package.lisp

(defpackage #:ablisp
  (:use #:cl))

(defpackage #:vertigo
  (:use #:cl))

(defpackage #:vertigo-test
  (:use #:cl #:vertigo #:lisp-unit))

(defpackage #:vertigo-runtime
  (:use #:cl)
  (:nicknames #:vrt)
  (:export #:make-padded-string))
