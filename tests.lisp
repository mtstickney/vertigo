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

(define-test date-literal
  (let ((date-node (parse #'vertigo::date-literal? "1234/3456/5678")))
    (assert-equalp 1234
                   (vertigo::date-value-month date-node))
    (assert-equalp 3456
                   (vertigo::date-value-day date-node))
    (assert-equal 5678
                  (vertigo::date-value-year date-node))))

(define-test iso-time-literal
  (let ((time-value (parse #'vertigo::iso8601-time-tz-literal? "01:23:45.0678+55:66")))
    (assert-equal 01 (vertigo::time-value-hour time-value))
    (assert-equal 23 (vertigo::time-value-minute time-value))
    (assert-equal 45 (vertigo::time-value-second time-value))
    (assert-equal 678 (vertigo::time-value-sec-frac time-value))
    (assert-equal 4 (vertigo::time-value-sec-decimals time-value))
    (assert-equal 55 (vertigo::time-value-tz-hr time-value))
    (assert-equal 66 (vertigo::time-value-tz-min time-value))
    (assert-equal t (vertigo::time-value-tz-present time-value))))

(define-test iso-time-literal-no-tz-min
  (let ((time-value (parse #'vertigo::iso8601-time-tz-literal? "01:23:45.0678+55")))
    (assert-equal 01 (vertigo::time-value-hour time-value))
    (assert-equal 23 (vertigo::time-value-minute time-value))
    (assert-equal 45 (vertigo::time-value-second time-value))
    (assert-equal 678 (vertigo::time-value-sec-frac time-value))
    (assert-equal 4 (vertigo::time-value-sec-decimals time-value))
    (assert-equal 55 (vertigo::time-value-tz-hr time-value))
    (assert-equal 0 (vertigo::time-value-tz-min time-value))
    (assert-equal nil (vertigo::time-value-tz-present time-value))))

(define-test iso-time-literal-no-tz-hr
  (let ((time-value (parse #'vertigo::iso8601-time-tz-literal? "01:23:45.0678")))
    (assert-equal 01 (vertigo::time-value-hour time-value))
    (assert-equal 23 (vertigo::time-value-minute time-value))
    (assert-equal 45 (vertigo::time-value-second time-value))
    (assert-equal 678 (vertigo::time-value-sec-frac time-value))
    (assert-equal 4 (vertigo::time-value-sec-decimals time-value))
    (assert-equal 0 (vertigo::time-value-tz-hr time-value))
    (assert-equal 0 (vertigo::time-value-tz-min time-value))
    (assert-equal nil (vertigo::time-value-tz-present time-value))))

(define-test iso-time-literal-no-sec-frac
  (let ((time-value (parse #'vertigo::iso8601-time-tz-literal? "01:23:45")))
    (assert-equal 01 (vertigo::time-value-hour time-value))
    (assert-equal 23 (vertigo::time-value-minute time-value))
    (assert-equal 45 (vertigo::time-value-second time-value))
    (assert-equal 0 (vertigo::time-value-sec-frac time-value))
    (assert-equal 0 (vertigo::time-value-sec-decimals time-value))
    (assert-equal 0 (vertigo::time-value-tz-hr time-value))
    (assert-equal 0 (vertigo::time-value-tz-min time-value))
    (assert-equal nil (vertigo::time-value-tz-present time-value))))

(define-test iso-time-literal-no-sec
  (let ((time-value (parse #'vertigo::iso8601-time-tz-literal? "01:23")))
    (assert-equal 01 (vertigo::time-value-hour time-value))
    (assert-equal 23 (vertigo::time-value-minute time-value))
    (assert-equal 0 (vertigo::time-value-second time-value))
    (assert-equal 0 (vertigo::time-value-sec-frac time-value))
    (assert-equal 0 (vertigo::time-value-sec-decimals time-value))
    (assert-equal 0 (vertigo::time-value-tz-hr time-value))
    (assert-equal 0 (vertigo::time-value-tz-min time-value))
    (assert-equal nil (vertigo::time-value-tz-present time-value))))

(define-test iso-datetime-literal
  (let ((date-value (vertigo::make-date-value :month 10
                                              :day 13
                                              :year 87))
        (time-value (vertigo::make-time-value :hour 01
                                              :minute 23
                                              :second 45
                                              :sec-frac 678
                                              :sec-decimals 4
                                              :tz-hr 55
                                              :tz-min 66
                                              :tz-present t)))
    (assert-equalp (vertigo::make-datetime-value :date date-value :time time-value)
                   (parse #'vertigo::iso8601-datetime-tz-literal? "87-10-13T01:23:45.0678+55:66"))))

(define-test string-datetime-literal
  (let ((date-value (vertigo::make-date-value :month 874
                                              :day 846
                                              :year 903))
        (time-value (vertigo::make-time-value :hour 01
                                              :minute 23
                                              :second 45
                                              :sec-frac 678
                                              :sec-decimals 4
                                              :tz-hr 55
                                              :tz-min 66
                                              :tz-present t)))
    (assert-equalp (vertigo::make-datetime-value :date date-value
                                                 :time time-value)
                   (parse #'vertigo::string-datetime-tz-literal?
                          "\"874-846-903   01:23:45.0678+55:66\""))))

(define-test datetime-literal
  (let ((date-value (vertigo::make-date-value :month 874
                                              :day 846
                                              :year 903))
        (time-value (vertigo::make-time-value :hour 01
                                              :minute 23
                                              :second 45
                                              :sec-frac 678
                                              :sec-decimals 4
                                              :tz-hr 55
                                              :tz-min 66
                                              :tz-present t)))
    (assert-equalp (vertigo::make-datetime-value :date date-value
                                                 :time time-value)
                   (parse #'vertigo::datetime-literal?
                          "\"874-846-903   01:23:45.0678+55:66\""))
    (assert-equalp (vertigo::make-datetime-value :date date-value
                                                 :time time-value)
                   (parse #'vertigo::datetime-literal?
                          "903-874-846T01:23:45.0678+55:66"))))
