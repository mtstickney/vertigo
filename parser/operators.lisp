(in-package #:vertigo)

;;; Custom numeric expression rule
;; value = number-literal | var-place | ( expression ) | - value
;;       | + value | funcall
;; mult-expression = value '*' value | value
;; div-expression = mult-expression '/' mult-expression
;;                | mult-expression
;; add-expression = div-expression '+' div-expression
;;                | div-expression
;; minus-expression = add-expression '-' add-expression
;;                  | add-expression

(meta-sexp:defrule integer? (&aux (digits 0) (result 0) d) ()
  (:+ (:assign d (:type meta-sexp:digit?))
      (setf result (+ (* result 10) (digit-char-p d)))
      (incf digits))
  (:return result digits))

(meta-sexp:defrule integer-literal? (&aux match (result 0)) ()
  (:with-stored-match (match)
    (:assign result (:rule integer?)))
  (:return (make-int-value :val result)))

(defun digits (n &optional (base 10))
  (check-type n integer)
  (if (= n 0)
      (1+ (floor (log n base)))))

;; Note: decimal literal doesn't do integral literals (type promotion
;; handles that)
(meta-sexp:defrule decimal-literal? (&aux match numerator) ()
  (:with-stored-match (match)
    (:? (:assign numerator (:rule integer?)))
    #\.
    (multiple-value-bind (num digits) (meta-sexp:meta (:rule integer?))
      (and num
           (meta-sexp:meta (:return (make-rational-value
                                     :int (or numerator 0)
                                     :frac num
                                     :decimals digits)))))))

(meta-sexp:defrule numeric-literal? (&aux match number) ()
  (:with-stored-match (match)
    (:assign number
             (:or (:rule decimal-literal?)
                  (:rule integer-literal?))))
  (:return number))

(meta-sexp:defrule date-literal? (&aux match month day year) ()
  (:with-stored-match (match)
    (:assign month (:rule integer?))
    #\/
    (:assign day (:rule integer?))
    #\/
    (:assign year (:rule integer?)))
  (:return (make-date-value :month month
                            :day day
                            :year year)))

(meta-sexp:defrule iso8601-time-tz-literal? (&aux match
                                                  hour
                                                  min
                                                  sec
                                                  sec-frac
                                                  (sec-decimals 0)
                                                  tz-hr
                                                  tz-min)
    ()
  (:with-stored-match (match)
    (:assign hour (:rule integer?))
    #\:
    (:assign min (:rule integer?))
    (:? #\:
        (:assign sec (:rule integer?))
        (:? #\.
            (multiple-value-bind (n digits) (meta-sexp:meta (:rule integer?))
              (setf sec-frac n
                    sec-decimals digits))))
    ;; TODO: we're making tz-min optional here, which is probably wrong
    (:? #\+
        (:assign tz-hr (:rule integer?))
        #\:
        (:assign tz-min (:rule integer?))))
  (:return (make-time-value :hour hour
                            :minute min
                            :second (or sec 0)
                            :sec-frac (or sec-frac 0)
                            :sec-decimals sec-decimals
                            :tz-hr (or tz-hr 0)
                            :tz-min (or tz-min 0)
                            ;; Note the conversion to t/nil
                            :tz-present (if (and tz-hr tz-min)
                                            t
                                            nil))))

;; Should probably be converted to the stricter ISO8601 format
(meta-sexp:defrule iso8601-datetime-tz-literal? (&aux match
                                                      month
                                                      day
                                                      year
                                                      time-part)
    ()
  (:with-stored-match (match)
    (:assign year (:rule integer?))
    #\-
    (:assign month (:rule integer?))
    #\-
    (:assign day (:rule integer?))
    #\T
    (:assign time-part (:rule iso8601-time-tz-literal?)))
  (:return (make-datetime-value :date (make-date-value :year year
                                                       :month month
                                                       :day day)
                                :time time-part)))

(meta-sexp:defrule string-datetime-tz-literal? (&aux match
                                                     month
                                                     day
                                                     year
                                                     time-part)
    ()
  (:with-stored-match (match)
    #\"
    (:assign month (:rule integer?))
    #\-
    (:assign day (:rule integer?))
    #\-
    (:assign year (:rule integer?))
    (:+ (:type meta-sexp:space?))
    (:assign time-part (:rule iso8601-time-tz-literal?))
    #\")
  (:return (make-datetime-value :date (make-date-value :year year
                                                       :month month
                                                       :day day)
                                :time time-part)))

(meta-sexp:defrule datetime-literal? ()
    (:or (:rule iso8601-datetime-tz-literal?)
         (:rule string-datetime-tz-literal?)))

(meta-sexp:defrule string-literal? (&aux match
                                         (str (meta-sexp:make-char-accum))
                                         (code 0)
                                         digit
                                         char
                                         quote
                                         (justify :none)
                                         reserved
                                         (translatable t))
    ()
  (:with-stored-match (match)
    (:assign quote (:or #\" #\'))
    (:* (:or (:and #\~ (:or (:and #\n (:char-push #\Newline str))
                            (:and #\t (:char-push #\Tab str))
                            (:and #\r (:char-push #\Return str))
                            (:and #\E (:char-push (code-char #o033) str))
                            (:and #\b (:char-push #\Backspace str))
                            (:and #\f (:char-push (code-char #o014) str))
                            (:and (:n-times 3 (:assign digit (:type meta-sexp:digit?))
                                            ;; Octal digits
                                            (:assign code (+ (* code 8) (digit-char-p digit))))
                                  (:char-push (code-char code) str))
                            (:char-push str)))
             ;; Any unescaped char that isn't the quote
             (:checkpoint
              (:and (:assign char (:type character))
                    (:not (eql char quote))
                    (:char-push char str)))))
    ;; Accept the quote char
    (eql (meta-sexp:meta (:type character)) quote)
    (:? #\:
        (:?
         (:icase (:or (:and "R"
                            (:assign justify :right))
                      (:and "L"
                            (:assign justify :left))
                      (:and "C"
                            (:assign justify :center))
                      (:and "T"
                            (:assign justify :trim)))))
        (:? (:icase "U")
            (:or (:assign translatable nil) t))
        (:? (:assign reserved (:rule integer?)))))
  (:return (make-string-value :str str
                              :justify justify
                              :reserved reserved
                              :translatable translatable)))

(meta-sexp:defrule boolean-literal? (&aux match val) ()
  (:with-stored-match (match)
    (:or (:and (:icase (:or "YES" "TRUE"))
               (:assign val t))
         (:and (:icase (:or "NO" "FALSE"))
               ;; ensure the value of :assign doesn't end the match
               (:or (:assign val nil) t))))
  (:return (make-boolean-value :val val)))

(meta-sexp:defrule literal? () ()
  (:or (:rule string-literal?)
       (:rule boolean-literal?)
       (:rule datetime-literal?)
       (:rule date-literal?)
       (:rule boolean-literal?)))

;; '.' isn't treated as an operator here because we need to use
;; whitespace to distinguish between field reference and statement separation
(meta-sexp:defrule buffer-field? (&aux match buf field) ()
  (:with-stored-match (match)
    (:assign buf (:rule identifier?))
    #\.
    (:assign field (:rule identifier?)))
  (:return (make-op-node :op "."
                         :lhs buf
                         :rhs field)))

(meta-sexp:defrule atom? (&aux val) ()
  (:assign val (:or (:rule literal?)
                    ;; Needs to come before identifier
                    (:rule buffer-field?)
                    (:rule identifier?)
                    (:rule function-call?))))

(defun right-binding-power (op arity)
  (cond
    ;; Statement terminator, non-binding (should maybe remove this?)
    ((equal op ".")
     0)
    ((equalp op "OR")
     1)
    ((equalp op "AND")
     2)
    ((equalp op "NOT")
     3)
    ((member op '("<" "LT" "<=" "LE" ">" "GT" ">=" "GE" "=" "EQ" "<>" "NE") :test #'equalp)
     4)
    ((and (or (equal op "+")
              (equal op "-"))
          (eql arity 2))
     5)
    ((or (equal op "MODULO")
         (equal op "/")
         (equal op "*"))
     6)
    ((and (or (equal op "+")
              (equal op "-"))
          (eql arity 1))
     7)
    ((equal op "[") ;; array reference operator
     8)
    ((or (equal op ":")
         (equal op "::"))
     9)
    (t (error "Unknown operator ~A" op))))

;; TODO: how does associativity work with separators like "."?
(defun op-associativity (op)
  (if (or (equalp op "AND")
          (equalp op "OR"))
      :right
      :left))

;; TODO: Add the function-form of IF in here (the ternary)
(meta-sexp:defrule unary-value? (&aux match op) ()
  (:or (:rule atom?)
       (:and (:assign op (:or "+" "-" (:icase "NOT")))
             (:? (:rule whitespace?))
             (:rule expression? (right-binding-power op 1)))
       (:delimited (:rule whitespace?)
                   "(" (:rule expression?) ")")))

(meta-sexp:defrule operator? (&aux match) ()
  (:with-stored-match (match)
    (:or ":"
         "::"
         "MODULO"
         "/"
         "*"
         "+"
         "-"
         "NOT"
         "AND"
         "OR"
         "["
         "<" "LT"
         ">" "GT"
         "<=" "LE"
         ">=" "GE"
         "=" "EQ"
         "<>" "NE")))

(meta-sexp:defrule expression? (&optional (bind-power 0) &aux match op lhs) ()
  (:assign lhs (:rule unary-value?))
  (:*
   (:? (:rule whitespace?))
   ;; While lookahead token is a binary op with binding power >= BIND-POWER
   (:checkpoint
    (:assign op (:rule operator?))
    (>= (right-binding-power op 2) bind-power))
   (let* ((rbp (right-binding-power op 2))
          (rest-rbp (if (eq (op-associativity op) :left)
                        (1+ rbp)
                        rbp))
          (rhs (meta-sexp:meta (:rule expression? rest-rbp))))
     (meta-sexp:meta
      (:and rhs
            (setf lhs (make-op-node :op op :lhs lhs :rhs rhs)))))))

;;; Numeric precedence parser
(meta-sexp:defrule numeric-expression? (&optional (bind-power 0) &aux match op lhs) ()
  (:assign lhs (:rule numeric-primary?))
  (:*
   (:? (:rule whitespace?))
   (:assign op (:or "+" "-" "/" "*"))
   (:? (:rule whitespace))
   (if (<= (numeric-binding-power op) bind-power)
       (setf lhs (make-op-node :op op :lhs lhs :rhs (:rule numeric-primary?)))
       ;; Parse up everything with a binding power > op's for the rhs
       (setf lhs (make-op-node :op op :lhs lhs :rhs (:rule numeric-expression?
                                                           (numeric-binding-power op))))))
  (:return lhs))

;;; Boolean precedence parser
(defun boolean-binding-power (op)
  (cond
    ((equalp op "OR")
     1)
    ((equalp op "AND")
     2)))

(meta-sexp:defrule boolean-primary? (&aux match) ()
  (:with-stored-match (match)
    (:? (:icase "NOT"))
    (:or (:icase "YES")
         (:icase "NO")
         (:icase "TRUE")
         (:icase "FALSE")
         (:rule place?))))

(meta-sexp:defrule boolean-expression? (&optional (bind-power 0) &aux match op lhs) ()
  (:assign lhs (:rule boolean-primary?))
  (:*
   (:? (:rule whitespace?))
   (:assign op (:or (:icase "AND") (:icase "OR")))
   (:? (:rule whitespace?))
   (if (<= (boolean-binding-power op) bind-power)
       (setf lhs (make-op-node :op op :lhs lhs :rhs (:rule boolean-primary?)))
       ;; Parse up everything with a binding power > op's for the rhs
       (setf lhs (make-op-node :op op :lhs lhs :rhs (:rule boolean-expression?
                                                           (boolean-binding-power op))))))
  (:return lhs))

;;; + Unary positive operator
;; Form no. 1
(meta-sexp:defrule rule3331? () ()
  (:and "+" (:rule rule3330?)))

;; expression
(meta-sexp:defrule rule3330? () ()
)

;;; + Addition operator
;; Form no. 1
(meta-sexp:defrule rule3334? () ()
  (:and (:rule rule3332?) "+" (:rule rule3333?)))

;; expression
(meta-sexp:defrule rule3332? () ()
)

;; expression
(meta-sexp:defrule rule3333? () ()
)

;;; + Concatenation operator
;; Form no. 1
(meta-sexp:defrule rule3337? () ()
  (:and (:rule rule3335?) "+" (:rule rule3336?)))

;; expression
(meta-sexp:defrule rule3335? () ()
)

;; expression
(meta-sexp:defrule rule3336? () ()
)

;;; + Date addition operator
;; Form no. 1
(meta-sexp:defrule rule3340? () ()
  (:and (:rule rule3338?) "+" (:rule rule3339?)))

;; date
(meta-sexp:defrule rule3338? () ()
)

;; days
(meta-sexp:defrule rule3339? () ()
)

;;; – Unary negative operator
;; Form no. 1
(meta-sexp:defrule rule3342? () ()
  (:and "-" (:rule rule3341?)))

;; expression
(meta-sexp:defrule rule3341? () ()
)

;;; – Subtraction operator
;; Form no. 1
(meta-sexp:defrule rule3345? () ()
  (:and (:rule rule3343?) (:? (:rule whitespace?)) "-" (:? (:rule whitespace?))
   (:rule rule3344?)))

;; expression
(meta-sexp:defrule rule3343? () ()
)

;; expression
(meta-sexp:defrule rule3344? () ()
)

;;; – Date subtraction operator
;; Form no. 1
(meta-sexp:defrule rule3349? () ()
  (:and (:rule rule3346?) "-" (:or (:rule rule3347?) (:rule rule3348?))))

;; date
(meta-sexp:defrule rule3346? () ()
)

;; days
(meta-sexp:defrule rule3347? () ()
)

;; date
(meta-sexp:defrule rule3348? () ()
)

;;; – Datetime subtraction operator
;; Form no. 1
(meta-sexp:defrule rule3352? () ()
  (:and (:rule rule3350?) "-" (:rule rule3351?)))

;; datetime-tz
(meta-sexp:defrule rule3350? () ()
)

;; datetime-tz
(meta-sexp:defrule rule3351? () ()
)


;; Form no. 2
(meta-sexp:defrule rule3355? () ()
  (:and (:rule rule3353?) "-" (:rule rule3354?)))

;; datetime
(meta-sexp:defrule rule3353? () ()
)

;; datetime
(meta-sexp:defrule rule3354? () ()
)

;;; * Multiplication operator
;; Form no. 1
(meta-sexp:defrule rule3358? () ()
  (:and (:rule rule3356?) (:? (:rule whitespace?)) "*" (:? (:rule whitespace?))
   (:rule rule3357?)))

;; expression
(meta-sexp:defrule rule3356? () ()
)

;; expression
(meta-sexp:defrule rule3357? () ()
)

;;; / Division operator
;; Form no. 1
(meta-sexp:defrule rule3361? () ()
  (:and (:rule rule3359?) (:? (:rule whitespace?)) "/" (:? (:rule whitespace?))
   (:rule rule3360?)))

;; expression
(meta-sexp:defrule rule3359? () ()
)

;; expression
(meta-sexp:defrule rule3360? () ()
)

;;; AND operator
;; Form no. 1
(meta-sexp:defrule rule3364? () ()
  (:and (:rule rule3362?) "AND" (:rule rule3363?)))

;; expression
(meta-sexp:defrule rule3362? () ()
)

;; expression
(meta-sexp:defrule rule3363? () ()
)

;;; BEGINS operator
;; Form no. 1
(meta-sexp:defrule rule3367? () ()
  (:and (:rule rule3365?) "BEGINS" (:rule rule3366?)))

;; expression1
(meta-sexp:defrule rule3365? () ()
)

;; expression2
(meta-sexp:defrule rule3366? () ()
)

;;; EQ or = operator
;; Form no. 1
(meta-sexp:defrule rule3370? () ()
  (:and (:rule rule3368?) (:or "EQ" "=") (:rule rule3369?)))

;; expression
(meta-sexp:defrule rule3368? () ()
)

;; expression
(meta-sexp:defrule rule3369? () ()
)

;;; GE or >= operator
;; Form no. 1
(meta-sexp:defrule rule3372? () ()
  (:rule rule3371?))

;; expression
(meta-sexp:defrule rule3371? () ()
)

;;; GT or > operator
;; Form no. 1
(meta-sexp:defrule rule3374? () ()
  (:rule rule3373?))

;; expression
(meta-sexp:defrule rule3373? () ()
)

;;; LE or < = operator
;; Form no. 1
(meta-sexp:defrule rule3376? () ()
  (:rule rule3375?))

;; expression
(meta-sexp:defrule rule3375? () ()
)

;;; LT or < operator
;; Form no. 1
(meta-sexp:defrule rule3378? () ()
  (:rule rule3377?))

;; expression
(meta-sexp:defrule rule3377? () ()
)

;;; MATCHES operator
;; Form no. 1
(meta-sexp:defrule rule3381? () ()
  (:and (:rule rule3379?) "MATCHES" (:rule rule3380?)))

;; expression
(meta-sexp:defrule rule3379? () ()
)

;; pattern
(meta-sexp:defrule rule3380? () ()
)

;;; MODULO operator
;; Form no. 1
(meta-sexp:defrule rule3384? () ()
  (:and (:rule rule3382?) "MODULO" (:rule rule3383?)))

;; expression
(meta-sexp:defrule rule3382? () ()
)

;; base
(meta-sexp:defrule rule3383? () ()
)

;;; NE or <> operator
;; Form no. 1
(meta-sexp:defrule rule3386? () ()
  (:rule rule3385?))

;; expression
(meta-sexp:defrule rule3385? () ()
)

;;; NOT operator
;; Form no. 1
(meta-sexp:defrule rule3388? () ()
  (:and "NOT" (:rule rule3387?)))

;; expression
(meta-sexp:defrule rule3387? () ()
)

;;; OR operator
;; Form no. 1
(meta-sexp:defrule rule3391? () ()
  (:and (:rule rule3389?) "OR" (:rule rule3390?)))

;; expression
(meta-sexp:defrule rule3389? () ()
)

;; expression
(meta-sexp:defrule rule3390? () ()
)
