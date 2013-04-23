(in-package #:vertigo-test)

(defun vertigo::run-all-tests ()
  (lisp-unit:run-tests :all (find-package 'vertigo-test)))

(defun parse (func input)
  (funcall func (meta-sexp:create-parser-context input)))

(defun remove-test (symb &optional (pkg '#:vertigo-test))
  (remhash symb (lisp-unit::package-table pkg t))
  (unintern symb))

;; lisp-unit configuration
(setf lisp-unit:*print-errors* t
      lisp-unit:*print-failures* t)

;;; Integer literal tests
;;; Note that negation is part of expression parsing, not the literals
(define-test integer-literal
  (assert-equalp (vertigo::make-int-value :val 123)
                 (parse #'vertigo::integer-literal? "123")))

(define-test integer-literal-leading-zero
  (assert-equalp (vertigo::make-int-value :val 123)
                 (parse #'vertigo::integer-literal? "0123")))

;;; Decimal literal tests
(define-test decimal-literal-zero-decimal
  (assert-equalp (vertigo::make-rational-value :int 123
                                               :frac 0
                                               :decimals 1)
                 (parse #'vertigo::decimal-literal? "123.0")))

(define-test decimal-literal-leading-zero-decimal
  (assert-equalp (vertigo::make-rational-value :int 123
                                               :frac 567
                                               :decimals 4)
                 (parse #'vertigo::decimal-literal? "123.0567")))

(define-test decimal-literal-trailing-zero-decimal
  (assert-equalp (vertigo::make-rational-value :int 123
                                               :frac 56700
                                               :decimals 6)
                 (parse #'vertigo::decimal-literal? "123.056700")))

(define-test decimal-only-decimal-literal
  (assert-equalp (vertigo::make-rational-value :int 0
                                               :frac 567
                                               :decimals 4)
                 (parse #'vertigo::decimal-literal? ".0567")))

(define-test decimal-only-trailing-zero-decimal-literal
  (assert-equalp (vertigo::make-rational-value :int 0
                                               :frac 56700
                                               :decimals 6)
                 (parse #'vertigo::decimal-literal? ".056700")))

(define-test integral-decimal-end-of-statement
  (assert-equal nil
                (parse #'vertigo::decimal-literal? "123. ")))

(define-test integral-numeric-literal
  (assert-equalp (vertigo::make-int-value :val 123)
                 (parse #'vertigo::numeric-literal? "123")))

(define-test decimal-numeric-literal
  (assert-equalp (vertigo::make-rational-value :int 123 :frac 456 :decimals 3)
                 (parse #'vertigo::numeric-literal? "123.456")))
