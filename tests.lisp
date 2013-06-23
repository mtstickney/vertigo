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
    (assert-equalp (vertigo::make-string-value :str "foo"
                                               :justify :left
                                               :translatable t
                                               :reserved nil)
                   (parse #'vertigo::literal? "\"foo\":L"))
    (assert-equalp (vertigo::make-boolean-value :val t)
                   (parse #'vertigo::literal? "YES"))
    (assert-equalp (vertigo::make-datetime-value :date date-value
                                                 :time time-value)
                   (parse #'vertigo::literal? "903-874-846T01:23:45.0678+55:66"))
    (assert-equalp (vertigo::make-string-value :str "874-846-903   01:23:45.0678+55:66")
                   (parse #'vertigo::literal? "\"874-846-903   01:23:45.0678+55:66\""))
    (assert-equalp (vertigo::make-date-value :month 10
                                             :day 3
                                             :year 900)
                   (parse #'vertigo::literal? "10/3/900"))
    (assert-equalp (vertigo::make-rational-value :int 4 :frac 87 :decimals 3)
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

(define-test operator-associativity
  (assert-equalp (vertigo::make-op-node
                  :op "-"
                  :lhs (vertigo::make-op-node
                        :op "-"
                        :lhs (vertigo::make-ident :name "foo")
                        :rhs (vertigo::make-ident :name "bar"))
                  :rhs (vertigo::make-ident :name "baz"))
                 (parse #'vertigo::expression? "foo - bar - baz"))
  (assert-equalp (vertigo::make-op-node
                  :op "AND"
                  :lhs (vertigo::make-ident :name "foo")
                  :rhs (vertigo::make-op-node
                        :op "AND"
                        :lhs (vertigo::make-ident :name "bar")
                        :rhs (vertigo::make-ident :name "baz")))
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
                        :rhs (vertigo::make-unary-op-node
                              :op "-"
                              :val (vertigo::make-int-value :val 3)))
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
