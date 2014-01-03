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

(define-test integer-literal-hex
    (assert-equalp (vertigo::make-int-value :val 255)
                   (parse #'vertigo::integer-literal? "0xFF")))

;;; Decimal literal tests
(define-test decimal-literal-zero-decimal
  (assert-equalp (vertigo::make-rational-value :val 123/1)
                 (parse #'vertigo::decimal-literal? "123.0")))

(define-test decimal-literal-leading-zero-decimal
  (assert-equalp (vertigo::make-rational-value :val 1230567/10000)
                 (parse #'vertigo::decimal-literal? "123.0567")))

(define-test decimal-literal-trailing-zero-decimal
  (assert-equalp (vertigo::make-rational-value :val 1230567/10000)
                 (parse #'vertigo::decimal-literal? "123.056700")))

(define-test decimal-only-decimal-literal
  (assert-equalp (vertigo::make-rational-value :val 567/10000)
                 (parse #'vertigo::decimal-literal? ".0567")))

(define-test decimal-only-trailing-zero-decimal-literal
  (assert-equalp (vertigo::make-rational-value :val 567/10000)
                 (parse #'vertigo::decimal-literal? ".056700")))

(define-test integral-decimal-end-of-statement
  (assert-equal nil
                (parse #'vertigo::decimal-literal? "123. ")))

(define-test integral-numeric-literal
  (assert-equalp (vertigo::make-int-value :val 123)
                 (parse #'vertigo::numeric-literal? "123")))

(define-test decimal-numeric-literal
  (assert-equalp (vertigo::make-rational-value :val 123456/1000)
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

(define-test string-literal-no-opts
  (assert-equalp (vertigo::make-string-value :str "abc"
                                    :justify :none
                                    :translatable t
                                    :reserved nil)
                 (parse #'vertigo::string-literal? "\"abc\""))
  (assert-equalp (vertigo::make-string-value :str "abc"
                                    :justify :none
                                    :translatable t
                                    :reserved nil)
                 (parse #'vertigo::string-literal? "'abc'")))

;; TODO: Do we want flags to be case-insensitive?
(define-test string-literal-flags
  (assert-equal :left
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':l")))
  (assert-equal :left
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':L")))
  (assert-equal :right
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':r")))
  (assert-equal :right
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':R")))
  (assert-equal :center
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':c")))
  (assert-equal :center
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':C")))
  (assert-equal :trim
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':t")))
  (assert-equal :trim
                (vertigo::string-value-justify (parse #'vertigo::string-literal? "'abc':T"))))

(define-test string-literal-translate
  (assert-equal nil
                (vertigo::string-value-translatable (parse #'vertigo::string-literal? "'abc':Lu")))
  (assert-equal nil
                (vertigo::string-value-translatable (parse #'vertigo::string-literal? "'abc':LU")))
  (assert-equal t
                (vertigo::string-value-translatable (parse #'vertigo::string-literal? "'abc':L")))
  (assert-equal t
                (vertigo::string-value-translatable (parse #'vertigo::string-literal? "'abc':L45"))))

(define-test string-literal-reserved
  (assert-equal nil
                (vertigo::string-value-reserved (parse #'vertigo::string-literal? "'abc':LU")))
  (assert-equal 50
                (vertigo::string-value-reserved (parse #'vertigo::string-literal? "'abc':TU50"))))

(define-test string-literal-escapes
  (let ((str (format nil "~A~A~A~A~A~A~A~A~A"
                     #\Newline
                     #\Tab
                     #\Return
                     #\Escape
                     #\Backspace
                     #\Page
                     (code-char #o025)
                     #\G
                     #\~)))
    (assert-equal str (vertigo::string-value-str
                       (parse #'vertigo::string-literal? "'~n~t~r~E~b~f~025~G~~'")))))

(define-test boolean-true-literal
  (let ((boolean-value (vertigo::make-boolean-value :val t)))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "yes"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "YES"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "true"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "TRUE"))))

(define-test boolean-false-literal
  (let ((boolean-value (vertigo::make-boolean-value :val nil)))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "no"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "NO"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "false"))
    (assert-equalp boolean-value
                   (parse #'vertigo::boolean-literal? "FALSE"))))

;; The literal rule should accept all these things, try a basic test
;; for each
(define-test literal
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
    (assert-equalp (vertigo::make-string-value
                    :str "foo"
                    :justify :left
                    :translatable t
                    :reserved nil)
                   (parse #'vertigo::literal? "\"foo\":L"))
    (assert-equalp (vertigo::make-boolean-value :val t)
                   (parse #'vertigo::literal? "YES"))
    (assert-equalp (vertigo::make-datetime-value
                    :date date-value
                    :time time-value)
                   (parse #'vertigo::literal? "903-874-846T01:23:45.0678+55:66"))
    (assert-equalp (vertigo::make-string-value :str "874-846-903   01:23:45.0678+55:66")
                   (parse #'vertigo::literal? "\"874-846-903   01:23:45.0678+55:66\""))
    (assert-equalp (vertigo::make-date-value
                    :month 10
                    :day 3
                    :year 900)
                   (parse #'vertigo::literal? "10/3/900"))
    (assert-equalp (vertigo::make-rational-value :val 4087/1000)
                   (parse #'vertigo::literal? "4.087"))
    (assert-equalp (vertigo::make-int-value :val 123)
                   (parse #'vertigo::literal? "123"))))

(define-test identifier
  ;; Note: Native ABL require the first character to be
  ;; alphabetic. Should we have the same restriction? Starting with a
  ;; number is a bit problematic re. literals...
  ;; (assert-equalp (vertigo::make-ident :name "_fo0-bar987_!")
  ;;                (parse #'vertigo::identifier? "_fo0-bar987_!"))
  ;; (assert-equalp (vertigo::make-ident :name "04Foo")
  ;;                (parse #'vertigo::identifier? "04Foo"))
  (assert-equalp (vertigo::make-ident :name "foo-")
                 (parse #'vertigo::identifier? "foo-"))
  (assert-equalp (vertigo::make-ident :name "fo04of")
                 (parse #'vertigo::identifier? "fo04of"))
  (assert-equalp (vertigo::make-ident :name "fo0-bar987_!")
                 (parse #'vertigo::identifier? "fo0-bar987_!")))

(define-test buffer-field
  (assert-equalp (vertigo::make-op-node :op "."
                                        :lhs (vertigo::make-ident :name "flob_4")
                                        :rhs (vertigo::make-ident :name "pram-pro"))
                 (parse #'vertigo::buffer-field? "flob_4.pram-pro")))

(define-test unary-nospace
  (assert-equalp (vertigo::make-unary-op-node :op "-"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "-foo"))
  (assert-equalp (vertigo::make-unary-op-node :op "+"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "+foo"))
  (assert-equalp (vertigo::make-ident :name "foo")
                 (parse #'vertigo::unary-value? "(foo)")))

(define-test unary-space
  (assert-equalp (vertigo::make-unary-op-node :op "-"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "- foo"))
  (assert-equalp (vertigo::make-unary-op-node :op "+"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "+ foo"))
  (assert-equalp (vertigo::make-ident :name "foo")
                 (parse #'vertigo::unary-value? "( foo )"))
  (assert-equalp (vertigo::make-unary-op-node :op "NOT"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "NOT foo"))
  (assert-equalp (vertigo::make-unary-op-node :op "NOT"
                                              :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::unary-value? "not foo")))

(define-test unary-atom
  (assert-equalp (vertigo::make-ident :name "foo")
                 (parse #'vertigo::unary-value? "foo")))

(define-test single-binary-op
  (loop for binop in '(":"
                       "::"
                       "MODULO"
                       "/"
                       "*"
                       "+"
                       "-"
                       "AND"
                       "OR"
                       "<" "LT"
                       ">" "GT"
                       "<=" "LE"
                       ">=" "GE"
                       "=" "EQ"
                       "<>" "NE")
     do (assert-equalp (vertigo::make-op-node
                        :op binop
                        :lhs (vertigo::make-ident :name "foo")
                        :rhs (vertigo::make-ident :name "bar"))
                       (parse #'vertigo::expression? (format nil "foo ~A bar" binop)))))

(define-test sigle-binary-op-nospace
  (loop for binop in '(":"
                       "::"
                       "/"
                       "*"
                       "+"
                       "-"
                       "<"
                       ">"
                       "<="
                       ">="
                       "="
                       "<>")
     do (assert-equalp (vertigo::make-op-node
                        :op binop
                        :lhs (vertigo::make-ident :name "foo")
                        :rhs (vertigo::make-ident :name "bar"))
                       (parse #'vertigo::expression? (format nil "foo ~A bar" binop)))))

(define-test operator-associativity
  (assert-equalp (vertigo::make-op-node
                  :op "-"
                  :lhs (vertigo::make-op-node
                        :op "-"
                        :lhs (vertigo::make-symb :name "foo")
                        :rhs (vertigo::make-symb :name "bar"))
                  :rhs (vertigo::make-symb :name "baz"))
                 (parse #'vertigo::expression? "foo - bar - baz"))
  (assert-equalp (vertigo::make-op-node
                  :op "AND"
                  :lhs (vertigo::make-symb :name "foo")
                  :rhs (vertigo::make-op-node
                        :op "AND"
                        :lhs (vertigo::make-symb :name "bar")
                        :rhs (vertigo::make-symb :name "baz")))
                 (parse #'vertigo::expression? "foo AND bar AND baz")))

(define-test operator-precedence
  (assert-equalp (vertigo::make-op-node
                  :op "-"
                  :lhs (vertigo::make-op-node
                        :op "+"
                        :lhs (vertigo::make-int-value :val 1)
                        :rhs (vertigo::make-op-node
                              :op "/"
                              :lhs (vertigo::make-op-node
                                    :op "*"
                                    :lhs (vertigo::make-int-value :val 2)
                                    :rhs (vertigo::make-int-value :val 3))
                              :rhs (vertigo::make-int-value :val 4)))
                  :rhs (vertigo::make-int-value :val 5))
                 (parse #'vertigo::expression? "1 + 2 * 3 / 4 - 5"))
  (assert-equalp (vertigo::make-op-node
                  :op "/"
                  :lhs (vertigo::make-op-node
                        :op "*"
                        :lhs (vertigo::make-op-node
                              :op "+"
                              :lhs (vertigo::make-int-value :val 1)
                              :rhs (vertigo::make-int-value :val 2))
                        :rhs (vertigo::make-int-value :val -3))
                  :rhs (vertigo::make-op-node
                        :op "-"
                        :lhs (vertigo::make-int-value :val 4)
                        :rhs (vertigo::make-int-value :val 5)))
                 (parse #'vertigo::expression? "(1 + 2) * -3 / (4 - 5)")))

;;; TODO: Add tests for function-call, since we're compiling that in now

(define-test parameter
  (assert-equalp (vertigo::make-param :type :input
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "INPUT foo"))
  (assert-equalp (vertigo::make-param :type :input
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "input foo"))
  (assert-equalp (vertigo::make-param :type :input
                                      :val (vertigo::make-op-node
                                            :op "+"
                                            :lhs (vertigo::make-int-value :val 1)
                                            :rhs (vertigo::make-int-value :val 2)))
                 (parse #'vertigo::param-spec? "INPUT 1 + 2"))
  (assert-equalp (vertigo::make-param :type :output
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "OUTPUT foo"))
  (assert-equalp (vertigo::make-param :type :output
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "output foo"))
  (assert-equalp (vertigo::make-param :type :output
                                      :val (vertigo::make-op-node
                                            :op "+"
                                            :lhs (vertigo::make-int-value :val 1)
                                            :rhs (vertigo::make-int-value :val 2)))
                 (parse #'vertigo::param-spec? "output 1 + 2"))
  (assert-equalp (vertigo::make-param :type :input-output
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "INPUT-OUTPUT foo"))
  (assert-equalp (vertigo::make-param :type :input-output
                                      :val (vertigo::make-op-node
                                            :op "+"
                                            :lhs (vertigo::make-int-value :val 1)
                                            :rhs (vertigo::make-int-value :val 2)))
                 (parse #'vertigo::param-spec? "INPUT-output 1 + 2"))
  (assert-equalp (vertigo::make-param :type :input-output
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "input-output foo")))

(define-test default-parameter
  (assert-equalp (vertigo::make-param :type :input
                                      :val (vertigo::make-ident :name "foo"))
                 (parse #'vertigo::param-spec? "foo"))
  (assert-equalp (vertigo::make-param :type :input
                                      :val (vertigo::make-op-node
                                            :op "+"
                                            :lhs (vertigo::make-int-value :val 1)
                                            :rhs (vertigo::make-int-value :val 2)))
                 (parse #'vertigo::param-spec? "1 + 2")))

(define-test empty-parameter-list
  (let ((empty-plist (vertigo::make-list-box :list '())))
    (assert-equalp empty-plist
                   (parse #'vertigo::param-list? "()"))
    (assert-equalp empty-plist
                   (parse #'vertigo::param-list? "(  )"))))

(define-test single-parameter-list
  (let ((single-plist (vertigo::make-list-box
                       :list (list (vertigo::make-param
                                    :type :input
                                    :val (vertigo::make-ident :name "foo"))))))
    (assert-equalp single-plist
                   (parse #'vertigo::param-list? "(INPUT foo)"))
    (assert-equalp single-plist
                   (parse #'vertigo::param-list? "( INPUT foo )"))))

(define-test multi-parameter-list
  (let ((multi-plist (vertigo::make-list-box
                      :list
                      (list (vertigo::make-param :type :input-output
                                                 :val (vertigo::make-ident :name "foo"))
                            (vertigo::make-param :type :input
                                                 :val (vertigo::make-ident :name "bar"))
                            (vertigo::make-param :type :output
                                                 :val (vertigo::make-ident :name "baz"))))))
    (assert-equalp multi-plist
                   (parse #'vertigo::param-list? "(INPUT-OUTPUT foo, bar, OUTPUT baz)"))
    (assert-equalp multi-plist
                   (parse #'vertigo::param-list? "( INPUT-OUTPUT foo, bar , OUTPUT baz)"))))

(define-test empty-funcall
  (let ((foo-call (vertigo::make-call :func (vertigo::make-ident :name "Foo")
                                      :params '()
                                      :type :function)))
    (assert-equalp foo-call
                   (parse #'vertigo::function-call? "Foo()"))
    (assert-equalp foo-call
                   (parse #'vertigo::function-call? "Foo ()"))))

(define-test non-empty-funcall
  (let* ((params (list (vertigo::make-param :type :output
                                            :val (vertigo::make-ident :name "bar"))
                       (vertigo::make-param :type :input
                                            :val (vertigo::make-ident :name "baz"))))
         (call (vertigo::make-call :func (vertigo::make-ident :name "Foo")
                                   :params params
                                   :type :function)))
    (assert-equalp call
                   (parse #'vertigo::function-call? "Foo(OUTPUT bar, baz)"))
    (assert-equalp call
                   (parse #'vertigo::function-call? "Foo (OUTPUT bar, baz)"))))

(define-test unquoted-event
  (assert-equalp (vertigo::make-ident :name "BUTTON-1")
                 (parse #'vertigo::event? "BUTTON-1")))

(define-test quoted-event
  (assert-equalp (vertigo::make-string-value :str "BUTTON-1")
                 (parse #'vertigo::event? "\"BUTTON-1\"")))

(define-test whitespace-event-list
  (assert-equalp (vertigo::make-list-box
                  :list (list (vertigo::make-ident :name "foo")
                              (vertigo::make-string-value :str "bar")
                              (vertigo::make-ident :name "baz")))
                 (parse #'vertigo::event-list? "foo \"bar\" baz")))

(define-test comma-event-list
  (assert-equalp (vertigo::make-list-box
                  :list (list (vertigo::make-ident :name "foo")
                              (vertigo::make-string-value :str "bar")
                              (vertigo::make-ident :name "baz")))
                 (parse #'vertigo::event-list? "foo,\"bar\" , baz")))

(define-test mixed-event-list
  (assert-equalp (vertigo::make-list-box
                  :list (list (vertigo::make-ident :name "foo")
                              (vertigo::make-string-value :str "bar")
                              (vertigo::make-ident :name "baz")))
                 (parse #'vertigo::event-list? "foo,\"bar\"  baz")))

;;; Widget-phrase
(define-test frame-widget-phrase
  (assert-equalp (vertigo::make-widget
                  :type :frame
                  :widget (vertigo::make-op-node
                           :op "::"
                           :lhs (vertigo::make-ident :name "foo")
                           :rhs (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::widget-phrase? "FRAME foo::bar")))

(define-test column-widget-phrase
  (let ((expr (vertigo::make-op-node
               :op "::"
               :lhs (vertigo::make-ident :name "foo")
               :rhs (vertigo::make-ident :name "bar"))))
    (assert-equalp expr
                   (parse #'vertigo::widget-phrase? "foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :browse-column
                    :widget expr
                    :parent expr)
                   (parse #'vertigo::widget-phrase? "foo::bar IN BROWSE foo::bar"))))

(define-test menu-widget-phrase
  (let ((expr (vertigo::make-op-node
               :op "::"
               :lhs (vertigo::make-ident :name "foo")
               :rhs (vertigo::make-ident :name "bar"))))
    (assert-equalp (vertigo::make-widget
                    :type :menu
                    :widget expr)
                   (parse #'vertigo::widget-phrase? "MENU foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :menu
                    :widget expr)
                   (parse #'vertigo::widget-phrase? "SUB-MENU foo::bar"))))

(define-test menu-item-widget-phrase
  (let ((expr (vertigo::make-op-node
               :op "::"
               :lhs (vertigo::make-ident :name "foo")
               :rhs (vertigo::make-ident :name "bar"))))
    (assert-equalp (vertigo::make-widget
                    :type :menu-item
                    :widget expr)
                   (parse #'vertigo::widget-phrase? "MENU-ITEM foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :menu-item
                    :widget expr
                    :parent expr)
                   (parse #'vertigo::widget-phrase? "MENU-ITEM foo::bar IN MENU foo::bar"))))

(define-test field-or-handle-widget-phrase
  (let ((expr (vertigo::make-op-node
               :op "::"
               :lhs (vertigo::make-ident :name "foo")
               :rhs (vertigo::make-ident :name "bar"))))
    (assert-equalp expr
                   (parse #'vertigo::widget-phrase? "foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :field-level
                    :widget expr)
                   (parse #'vertigo::widget-phrase? "FIELD foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :field-level
                    :widget expr
                    :parent expr)
                   (parse #'vertigo::widget-phrase? "foo::bar IN FRAME foo::bar"))
    (assert-equalp (vertigo::make-widget
                    :type :field-level
                    :widget expr
                    :parent expr)
                   (parse #'vertigo::widget-phrase? "FIELD foo::bar IN FRAME foo::bar"))))

(define-test wait-for-web-notify
  (assert-equalp (vertigo::make-statement
                  :type :wait-for-web-notify)
                 (parse #'vertigo::wait-for-statement? "WAIT-FOR web-notify OF DEFAULT-WINDOW"))
  (assert-equalp (vertigo::make-statement
                  :type :wait-for-web-notify
                  :data (vertigo::dict :pause (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::wait-for-statement? "WAIT-FOR \"WEB-NOTIFY\" OF DEFAULT-WINDOW
PAUSE bar"))
  (assert-equalp (vertigo::make-statement
                  :type :wait-for-web-notify
                  :data (vertigo::dict :exclusive-web-user t))
                 (parse #'vertigo::wait-for-statement? "WAIT-FOR WEB-NOTIFY OF DEFAULT-WINDOW EXCLUSIVE-WEB-USER"))
  (assert-equalp (vertigo::make-statement
                  :type :wait-for-web-notify
                  :data (vertigo::dict :pause (vertigo::make-ident :name "bar")
                                       :exclusive-web-user t))
                 (parse #'vertigo::wait-for-statement? "wait-for WEB-notify of default-window pause bar exclusive-web-user")))

(define-test view-statement
  (assert-equalp (vertigo::make-statement
                  :type :view
                  :data (vertigo::dict))
                 (parse #'vertigo::view-statement? "VIEW"))
  (assert-equalp (vertigo::make-statement
                  :type :view
                  :data (vertigo::dict :stream (vertigo::make-ident :name "foo")
                                       :window (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::view-statement? "view stream foo in window bar"))
  (assert-equalp (vertigo::make-statement
                  :type :view
                  :data (vertigo::dict :widget (vertigo::make-widget
                                                :type :field-level
                                                :widget (vertigo::make-ident :name "foo"))
                                       :window (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::view-statement? "view FIELD foo IN window bar")))

(define-test validate-statement
  (assert-equalp (vertigo::make-statement
                  :type :validate
                  :data (vertigo::dict :record (vertigo::make-ident :name "foo")))
                 (parse #'vertigo::validate-statement? "VALIDATE foo"))
  (assert-equalp (vertigo::make-statement
                  :type :validate
                  :data (vertigo::dict :record (vertigo::make-op-node
                                                :op "::"
                                                :lhs (vertigo::make-ident :name "foo")
                                                :rhs (vertigo::make-ident :name "bar"))))
                 (parse #'vertigo::validate-statement? "validate foo::bar")))

(define-test using-statement
  (assert-equalp (vertigo::make-statement
                  :type :using
                  :data (vertigo::dict :namespace '(:absolute "foo" "bar" "baz")))
                 (parse #'vertigo::using-statement? "USING foo.bar.baz"))
  (assert-equalp (vertigo::make-statement
                  :type :using
                  :data (vertigo::dict :namespace '(:wild "foo" "bar" "baz")))
                 (parse #'vertigo::using-statement? "using foo.bar.baz.*")))

(define-test use-statement
  (let ((result (vertigo::make-statement
                 :type :use
                 :data (vertigo::dict :env (vertigo::make-op-node
                                            :op "::"
                                            :lhs (vertigo::make-ident :name "foo")
                                            :rhs (vertigo::make-ident :name "bar"))))))
    (assert-equalp result
                   (parse #'vertigo::use-statement? "USE foo::bar"))
    (assert-equalp result
                   (parse #'vertigo::use-statement? "use foo::bar"))))

(define-test at-location-phrase
  (assert-equalp (vertigo::make-int-value :val 3)
                 (parse #'vertigo::at-phrase? "AT 3"))
  (assert-equalp (vertigo::make-int-value :val 3)
                 (parse #'vertigo::at-phrase? "at 3")))

(define-test at-rect-phrase
  (let ((x (vertigo::make-int-value :val 1))
        (y (vertigo::make-int-value :val 3)))
    (assert-equalp (vertigo::dict :x x :x-type :x :y y :y-type :y)
                   (parse #'vertigo::at-phrase? "AT X 1 Y 3"))
    (assert-equalp (vertigo::dict :x x :x-type :x :y y :y-type :y-of)
                   (parse #'vertigo::at-phrase? "at x 1 y-of 3"))
    (assert-equalp (vertigo::dict :x x :x-type :x-of :y y :y-type :y)
                   (parse #'vertigo::at-phrase? "AT X-OF 1 y 3"))
    (assert-equalp (vertigo::dict :x x :x-type :x-of :y y :y-type :y-of)
                   (parse #'vertigo::at-phrase? "at x-of 1 Y-OF 3"))
    (assert-equalp (vertigo::dict :x x :x-type :column :y y :y-type :row)
                   (parse #'vertigo::at-phrase? "at column 1 row 3"))
    (assert-equalp (vertigo::dict :x x :x-type :x-of :y y :y-type :y-of :align :colon)
                   (parse #'vertigo::at-phrase? "at x-of 1 Y-OF 3 colon-aligned"))
    (assert-equalp (vertigo::dict :x x :x-type :x-of :y y :y-type :y-of :align :left)
                   (parse #'vertigo::at-phrase? "at x-of 1 Y-OF 3 left-aligned"))
    (assert-equalp (vertigo::dict :x x :x-type :x-of :y y :y-type :y-of :align :right)
                   (parse #'vertigo::at-phrase? "at x-of 1 Y-OF 3 right-aligned"))))

(define-test color-phrase
  (assert-equalp :normal
                 (parse #'vertigo::color-phrase? "NORMAL"))
  (assert-equalp :input
                 (parse #'vertigo::color-phrase? "input"))
  (assert-equalp :messages
                 (parse #'vertigo::color-phrase? "MESSAGES"))
  (assert-equalp (vertigo::make-int-value :val #xFF)
                 (parse #'vertigo::color-phrase? "0xff"))
  (assert-equalp (vertigo::dict :bright t
                                :foreground "red/black")
                 (parse #'vertigo::color-phrase? "bright-red/black"))
  (assert-equalp (vertigo::dict :blink t
                                :foreground "red/black")
                 (parse #'vertigo::color-phrase? "blink-red/black"))
  (assert-equalp (vertigo::dict :blink t
                                :bright t
                                :foreground "red/black")
                 (parse #'vertigo::color-phrase? "blink-bright-red/black"))
  (assert-equalp (vertigo::dict :bright t
                                :foreground "red/black"
                                :background "green")
                 (parse #'vertigo::color-phrase? "bright-red/black-green"))
  (assert-equalp (vertigo::make-op-node
                  :op "::"
                  :lhs (vertigo::make-ident :name "foo")
                  :rhs (vertigo::make-ident :name "bar"))
                 (parse #'vertigo::color-phrase? "VALUE( foo::bar )"))
  (assert-equalp nil
                 (parse #'vertigo::color-phrase? "")))

(define-test frame-color-spec
  (let ((expr (vertigo::make-op-node
                  :op "::"
                  :lhs (vertigo::make-ident :name "foo")
                  :rhs (vertigo::make-ident :name "bar"))))
    (assert-equalp (vertigo::dict :bgcolor expr)
                   (parse #'vertigo::frame-color-spec? "BGCOLOR foo::bar"))
    (assert-equalp (vertigo::dict :fgcolor expr)
                   (parse #'vertigo::frame-color-spec? "FGCOLOR foo::bar"))
    (assert-equalp (vertigo::dict :dcolor expr)
                   (parse #'vertigo::frame-color-spec? "DCOLOR foo::bar"))
    (assert-equalp (vertigo::dict :dcolor expr
                                  :pfcolor expr)
                   (parse #'vertigo::frame-color-spec? "DCOLOR foo::bar PFCOLOR foo::bar"))
    (assert-equalp (vertigo::dict :dcolor (vertigo::make-int-value :val #xFF)
                                  :pfcolor expr)
                   (parse #'vertigo::frame-color-spec? "COLOR 0xFF PROMPT VALUE( foo::bar )"))
    (assert-equalp (vertigo::dict :dcolor (vertigo::make-int-value :val #xFF)
                                  :pfcolor expr)
                   (parse #'vertigo::frame-color-spec? "COLOR DISPLAY 0xFF PROMPT VALUE(foo::bar)"))))

(define-test size-phrase
  (assert-equalp (vertigo::dict :x (vertigo::make-int-value :val 1)
                                :y (vertigo::make-int-value :val 3)
                                :size-type :character)
                 (parse #'vertigo::size-phrase? "SIZE-CHARS 1 BY 3"))
  (assert-equalp (vertigo::dict :x (vertigo::make-int-value :val 1)
                                :y (vertigo::make-int-value :val 3)
                                :size-type :character)
                 (parse #'vertigo::size-phrase? "size 1 by 3"))
  (assert-equalp (vertigo::dict :x (vertigo::make-int-value :val 1)
                                :y (vertigo::make-int-value :val 3)
                                :size-type :pixel)
                 (parse #'vertigo::size-phrase? "size-pixels 1 BY 3")))

(define-test title-phrase
  (assert-equalp (vertigo::dict :title-color (vertigo::make-int-value :val 1)
                                :font (vertigo::make-ident :name "foo")
                                :title (vertigo::make-op-node
                                        :op "::"
                                        :lhs (vertigo::make-ident :name "foo")
                                        :rhs (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::title-phrase? "TITLE dcolor 1 FONT foo foo::bar"))
  (assert-equalp (vertigo::dict :title-color (vertigo::make-int-value :val 1)
                                :bg-color (vertigo::make-int-value :val 2)
                                :fg-color (vertigo::make-int-value :val 3)
                                :font (vertigo::make-ident :name "foo")
                                :title (vertigo::make-op-node
                                        :op "::"
                                        :lhs (vertigo::make-ident :name "foo")
                                        :rhs (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::title-phrase? "TITLE bgcolor 2 dcolor 1 fgcolor 3 FONT foo foo::bar"))
  (assert-equalp (vertigo::dict :color (vertigo::make-ident :name "foo")
                                :font (vertigo::make-ident :name "foo")
                                :title (vertigo::make-op-node
                                        :op "::"
                                        :lhs (vertigo::make-ident :name "foo")
                                        :rhs (vertigo::make-ident :name "bar")))
                 (parse #'vertigo::title-phrase? "TITLE color value(foo) FONT foo foo::bar")))

(defun flat-map (func form &key (test (constantly t)))
  "Return form with each sub-form matching TEST replaced with (FUNC sub-form), by in-order traversal."
  (cond
    ((funcall test form)
     (funcall func form))
    ((listp form)
     (mapcar (lambda (f)
               (flat-map func f :test test))
             form))
    (t (if (funcall test form)
           (funcall func form)
           form))))

(defun punch-hole (item hole)
  "Return the form item with the HOLEth form beginning with :?
replaced by NIL, as if traversing a flattened list."
  (let ((pos 0))
    (labels ((test (form)
               (and (listp form)
                    (eq (car form) :?)))
             (sub (form)
               (incf pos)
               (if (eql (1- pos) hole)
                   (let ((matches (count :? (alexandria:flatten (cdr form)))))
                     (incf pos matches)
                     nil)
                   (mapcar (lambda (f) (flat-map #'sub f :test #'test))
                           form))))
      (flat-map #'sub item :test #'test))))

;; Used to auto-munge test frame phrases
(defun make-holes (form)
  "Return form with a random selection of (:? ...) forms replaced by NIL."
  (check-type form list)
  (let* ((max-holes (count :? (alexandria:flatten form)))
         ;; [1, max-holes]
         (hole-count (1+ (random (min 5 max-holes))))
         (hole-locs (subseq (alexandria:shuffle (loop for i from 0 to hole-count collect i))
                            0
                            hole-count)))
    (format *debug-io* "Holes at ~S~%" hole-locs)
    (reduce #'punch-hole
            hole-locs
            :initial-value form)))

(defun resolve-choices (form)
  (labels ((test (form)
             (and (listp form)
                  (eq (car form) :or)))
           (sub (form)
             (flat-map #'sub (alexandria:random-elt (cdr form)) :test #'test)))
    (flat-map #'sub form :test #'test)))

(defun strip-markers (form)
  (labels ((test (form)
             (and (listp form)
                  (or (eq (car form) :and)
                      (eq (car form) :?))))
           (sub (form)
             (mapcar (lambda (f)
                       (flat-map #'sub f :test #'test))
                     (cdr form))))
    (flat-map #'sub form :test #'test)))

(defun make-test-form (form)
  (let ((munged (vertigo::-> form #'make-holes #'resolve-choices #'strip-markers #'alexandria:flatten)))
    ;; Random case keywords and join
    (format nil "~{~A~^ ~}" (mapcar (lambda (i)
                                      (if (keywordp i)
                                          (if (> (random 2) 0)
                                              (string-downcase (symbol-name i))
                                              (symbol-name i))
                                          i))
                                    munged))))
(defun make-frame-phrase-test ()
  (let ((form '(:with (:? :accum (:? "3 + 4"))
                (:? "") ;; TODO: at-phrase
                (:? (:or :attr-space :no-attr-space))
                (:? :cancel-button "canc_button")
                (:? :centered)
                (:? "") ;; TODO: color-spec
                (:? :column "foo::bar")
                (:? "4 * 3" :columns)
                (:? :context-help)
                (:? :context-help-file "C:\\users\\mts\\desktop.hlp")
                (:? :default-button "otherbutton")
                (:? :drop-target)
                (:? (:? "3 -4") :down)
                (:? :export)
                (:? :widget-id "foo")
                (:? :font "3 / 2")
                (:? :frame "floop_doo")
                (:? (:or :inherit-bgcolor :no-inherit-bgcolor))
                (:? (:or :inherit-fgcolor :no-inherit-fgcolor))
                (:? :keep-tab-order)
                (:? :no-box)
                (:? :no-hide)
                (:? :no-labels)
                (:? :use-dict-exps)
                (:? :no-validate)
                (:? :no-auto-validate)
                (:? :no-help)
                (:? :no-underline)
                (:? :overlay)
                (:? (:or :page-bottom :page-top))
                (:? :retain "17")
                (:? :row "foo::bar")
                (:? (:or :screen-io :stream-io))
                (:? :scroll "foo")
                (:? :scrollable)
                (:? :side-labels)
                (:?) ;; TODO: size-phrase
                (:? :stream "foo.bar")
                (:? :three-d)
                (:? ) ;; TODO: title-phrase
                (:? :top-only)
                (:? :use-text)
                (:? :v6frame
                 (:? (:or :use-revvideo :use-underline)))
                (:? :view-as :dialog-box)
                (:? :width "20")
                (:? :in :window "foo::bar"))))
    (make-test-form form)))

(define-test frame-phrase
  (let ((frame-phrase ;(make-frame-phrase-test)
         "WITH  attr-space CENTERED 4 * 3 columns CONTEXT-HELP context-help-file C:\\users\\mts\\desktop.hlp DEFAULT-BUTTON otherbutton drop-target 3 -4 down export widget-id foo FONT 3 / 2 FRAME floop_doo NO-INHERIT-BGCOLOR no-inherit-fgcolor KEEP-TAB-ORDER no-box NO-HIDE no-labels use-dict-exps NO-VALIDATE NO-AUTO-VALIDATE no-help no-underline overlay PAGE-TOP RETAIN 17 ROW foo::bar stream-io SCROLL foo scrollable SIDE-LABELS stream foo.bar THREE-D top-only USE-TEXT V6FRAME use-revvideo VIEW-AS DIALOG-BOX WIDTH 20 IN WINDOW foo::bar")
        (expected (vertigo::pdict :attr-space t
                                  :centered t
                                  :num-columns (vertigo::make-op-node
                                                :op "*"
                                                :lhs (vertigo::make-int-value :val 4)
                                                :rhs (vertigo::make-int-value :val 3))
                                  :context-help t
                                  :context-help-file (vertigo::make-symb :name "C:\\users\\mts\\desktop.hlp")
                                  :default-button (vertigo::make-symb :name "otherbutton")
                                  :drop-target t
                                  :max-duplicate-records (vertigo::make-op-node
                                                          :op "-"
                                                          :lhs (vertigo::make-int-value :val 3)
                                                          :rhs (vertigo::make-int-value :val 4))
                                  :export t
                                  :widget-id (vertigo::make-symb :name "foo")
                                  :font (vertigo::make-op-node :op "/"
                                                               :lhs (vertigo::make-int-value :val 3)
                                                               :rhs (vertigo::make-int-value :val 2))
                                  :frame-id (vertigo::make-symb :name "floop_do")
                                  :inherit-bgcolor nil
                                  :inherit-fgcolor nil
                                  :keep-tab-order t
                                  :no-box t
                                  :no-hide t
                                  :no-labels t
                                  :use-dict-help-strings t
                                  :use-dict-validation-string t
                                  :no-validate t
                                  :no-auto-validate t
                                  :no-help t
                                  :no-underline t
                                  :overlay t
                                  :page-type :top
                                  :scroll-retain (vertigo::make-int-value :val 17)
                                  :row (vertigo::make-op-node :op "::"
                                                              :lhs (vertigo::make-symb :name "foo")
                                                              :rhs (vertigo::make-symb "bar"))
                                  :scroll-by (vertigo::make-symb :name "foo")
                                  :scrollable t
                                  :side-labels t
                                  :stream (vertigo::make-op-node :op "."
                                                                 :lhs (vertigo::make-symb :name "foo")
                                                                 :rhs (vertigo::make-symb :name "bar"))
                                  :lickable t
                                  :top-only t
                                  :use-text t
                                  :v6frame t
                                  :v6-frame-opt :reverse-video
                                  :dialog-box t
                                  :width (vertigo::make-int-value :val 20)
                                  :parent-window (vertigo::make-op-node :op "::"
                                                                        :lhs (vertigo::make-symb :name "foo")
                                                                        :rhs (vertigo::make-symb :name "bar")))))
    (assert-equality #'cl-persist:equals expected (parse #'vertigo::frame-phrase? frame-phrase))))
