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

(deftype terminating-char () '(satisfies terminating-char-p))

(meta-sexp:defrule parse-string? (quote-char &aux match
                                             (str (meta-sexp:make-char-accum))
                                             (code 0)
                                             digit
                                             char
                                             string-args
                                             reserved
                                             options-p)
    ()
  (:with-stored-match (match)
    (:type character) ; will be (eql quote-char)
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
                    (:not (eql char quote-char))
                    (:char-push char str)))))
    ;; Accept the quote char
    (eql (meta-sexp:meta (:type character)) quote-char)
    (:?
     (:checkpoint #\:
                  (:?
                   (:icase (:or (:and "R"
                                      (setf (getf string-args :justify) :right))
                                (:and "L"
                                      (setf (getf string-args :justify) :left))
                                (:and "C"
                                      (setf (getf string-args :justify) :center))
                                (:and "T"
                                      (setf (getf string-args :justify) :trim))))
                   (:assign options-p t))
                  (:? (:icase "U")
                      (:or (setf (getf string-args :translatable) nil) t)
                      (:assign options-p t))
                  (:? (:assign reserved (:rule integer?))
                      (setf (getf string-args :reserved) reserved)
                      (:assign options-p t))
                  options-p
                  ;; Must be followed by a terminating character,
                  ;; ignore these options otherwise
                  (:and (:not (:rule terminating-char?))
                        (setf string-args '())
                        nil))))
  (apply #'make-string-value :str str (and options-p string-args)))

; "Run of digits in base BASE. Return an INT-VALUE of the integer and the number of digits as multiple values."
(meta-sexp:defrule digits? (&optional (base 10) &aux char (digits 0) (result 0) d) ()
  (:+ (:checkpoint (:assign char (:type character))
                   (:assign d (digit-char-p char base))
                   (setf result (+ (* result base) d))
                   (incf digits)))
  (:return (make-int-value :val result) digits))

; "Base-10 digits followed by a symbol-terminating character."
(meta-sexp:defrule integer? (&optional (base 10) &aux symb) ()
  (:assign symb (:rule symbol?))
  (:with-context ((symb-name symb))
    (multiple-value-bind (result digits) (meta-sexp:meta (:rule digits? base))
      (values)
      (meta-sexp:meta
       (:eof)
       (:return result digits)))))

; "A hexadecimal integer literal. '0x' followed by digits base 16 followed by a symbol-terminating character."
(meta-sexp:defrule hex-integer? () ()
  #\0
  (:icase #\x)
  (:rule integer? 16))

;; Note: decimal literal doesn't do integral literals (type promotion
;; handles that)
(meta-sexp:defrule decimal-literal? (&aux match numerator (sign 1)) ()
  (:with-stored-match (match)
    (:? (:assign numerator (:rule integer?)))
    #\.
    (multiple-value-bind (num digits) (meta-sexp:meta (:rule integer?))
      (and num
           (meta-sexp:meta (:return (make-rational-value
                                     :val (* sign (+ (if numerator (int-value-val numerator) 0)
                                                     (/ (int-value-val num) (expt 10 digits)))))))))))

(meta-sexp:defrule number? () ()
  (:or (:rule hex-integer?)
       (:rule decimal-literal?)
       (:rule integer?)))

;; Too low-level, defer to signed-number
(meta-sexp:defrule signed-number? (&aux (sign 1) num) ()
  (:? (:or (:and #\- (setf sign -1))
           #\+))
  (:assign num (:rule number?))
  (let ((new-num (copy-number-value num)))
    (setf (number-value-val num)
          (* (number-value-val num) sign))
    new-num))

(meta-sexp:defrule parse-number (char) ()
  (:rule signed-number?))

(meta-sexp:defrule date-literal? (&aux symb match month day year) ()
  (:with-stored-match (match)
    (:assign symb (:rule symbol?))
    (:with-context ((symb-name symb))
      (:whole-match
       (:assign month (:rule digits?))
       #\/
       (:assign day (:rule digits?))
       #\/
       (:assign year (:rule digits?)))))
  (:return (make-date-value :month (int-value-val month)
                            :day (int-value-val day)
                            :year (int-value-val year))))

(meta-sexp:defrule iso8601-time-tz-literal? (&aux match
                                                  hour
                                                  min
                                                  sec
                                                  tz-hr
                                                  tz-min)
    ()
  (:with-stored-match (match)
    (:assign hour (:rule integer?))
    #\:
    (:assign min (:rule integer?))
    (:? #\:
        (:assign sec (:or (:rule decimal-literal?)
                          (:rule integer?))))
    ;; TODO: we're making tz-min optional here, which is probably wrong
    (:? #\+
        (:assign tz-hr (:rule integer?))
        #\:
        (:assign tz-min (:rule integer?))))
  (:return (make-time-value :hour hour
                            :minute min
                            :second (or sec 0)
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
                                                     str
                                                     month
                                                     day
                                                     year
                                                     time-part)
    ()
  (:with-stored-match (match)
    (:assign str (:rule string-literal?))
    (:with-context (str)
      (:assign month (:rule integer?))
      #\-
      (:assign day (:rule integer?))
      #\-
      (:assign year (:rule integer?))
      (:rule whitespace?)
      (:assign time-part (:rule iso8601-time-tz-literal?))))
  (:return (make-datetime-value :date (make-date-value :year year
                                                       :month month
                                                       :day day)
                                :time time-part)))

(meta-sexp:defrule datetime-literal? () ()
  (:or (:rule string-datetime-tz-literal?)
       (:rule iso8601-datetime-tz-literal?)))

(meta-sexp:defrule parse-numeric-or-symbol (char) ()
  (:or (:rule iso8601-datetime-tz-literal?)
       (:rule iso8601-time-tz-literal?)
       (:rule date-literal?)
       (:rule number?)
       (:rule symbol?)))

(meta-sexp:defrule parse-comment (&aux (depth 0) match) ()
  (:and
   (:with-stored-match (match)
     (:and "/*" (incf depth))
     (loop while (and (> depth 0)
                      (not (meta-sexp:meta (:eof))))
        do (cond
             ((meta-sexp:meta "*/") (decf depth))
             ((meta-sexp:meta "/*") (incf depth))
             (t (meta-sexp:meta (:type t))))
        finally (return t))
     ;; If the depth isn't 0 here, we ran out of input
     (= depth 0))
   (:return (make-comment :str match))))

(meta-sexp:defrule parse-comment-or-symbol (char) ()
  (:or (:rule parse-comment)
       (:rule symbol?)))

(meta-sexp:defrule string-literal? () ()
  (:or (:rule parse-string? #\")
       (:rule parse-string? #\')))

(meta-sexp:defrule parse-colon-token (char &aux double) ()
  #\:
  (:? #\:
      (:assign double t))
  (if double
      (make-token :type :double-colon
                  :value "::")
      (make-token :type :colon
                  :value ":")))

(defstruct (parse-readtable (:constructor %make-parse-readtable))
  (terminating-chars)
  (non-terminating-chars)
  (single-escapes)
  (multiple-escapes))

(defun make-parse-readtable ()
  (%make-parse-readtable :terminating-chars (make-hash-table)
                         :non-terminating-chars (make-hash-table)
                         :single-escapes '()
                         :multiple-escapes '()))

;; TODO:
(defun macro-character (c &optional (readtable *parse-readtable*))
  (check-type c character)
  (check-type readtable parse-readtable)
  (let ((terminating (parse-readtable-terminating-chars readtable))
        (non-terminating (parse-readtable-non-terminating-chars readtable)))
    (multiple-value-bind (term-func term-present)
        (gethash c terminating)
      (multiple-value-bind (nonterm-func nonterm-present)
          (gethash c non-terminating)
        (cond
          (term-present (values term-func nil))
          (nonterm-present (values nonterm-func t))
          (t (values nil nil)))))))

(defun (setf macro-character) (func char &optional (readtable *parse-readtable*) (non-terminatingp nil))
  (check-type func (or function null))
  (check-type char character)
  (check-type readtable parse-readtable)
  (if non-terminatingp
      (setf (gethash char (parse-readtable-non-terminating-chars readtable))
            func)
      (setf (gethash char (parse-readtable-terminating-chars readtable))
            func)))

(defun terminating-char-p (char &optional (readtable *parse-readtable*))
  (declare (special *parse-readtable*))
  (nth-value 1 (gethash char (parse-readtable-terminating-chars readtable))))

(defun standard-readtable ()
  (macrolet ((set-macro-chars ((readtable &key non-terminating) &body entries)
               (let ((rt-var (gensym "READTABLE"))
                     (non-terminating-var (gensym "NT-P")))
                 `(let ((,rt-var ,readtable)
                        (,non-terminating-var ,non-terminating))
                    ,@(loop for entry in entries
                         collect (if (listp entry)
                                     `(setf (macro-character ,(first entry) ,rt-var ,non-terminating-var)
                                            ,(second entry))
                                     `(setf (macro-character ,entry ,rt-var ,non-terminating-var) nil)))))))
    (let ((readtable (make-parse-readtable)))
      ;; TODO: unquoted pathnames screw a lot of these up.
      (set-macro-chars (readtable)
                       (#\" #'parse-string?)
                       (#\' #'parse-string?)
                       (#\: #'parse-colon-token)
                       #\.
                       #\(
                       #\)
                       #\[
                       #\]
                       #\,
                       #\;)
      (loop for c in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         do (set-macro-chars (readtable :non-terminating t)
                             (c #'parse-numeric-or-symbol)))
      (set-macro-chars (readtable :non-terminating t)
                       (#\- #'parse-number)
                       (#\+ #'parse-number)
                       (#\/ #'parse-comment-or-symbol))
      readtable)))

(defvar *parse-readtable* (standard-readtable))

(meta-sexp:defrule parse-object (&aux char) ()
  (tagbody
   initial
     (cond
       ((meta-sexp:meta (:eof))
        ;; TODO: maybe do some eof-error-p stuff here
        (return-from parse-object nil))
       ((meta-sexp:meta (:rule whitespace?))
        (go initial))
       ((meta-sexp:meta (:assign char (:peek-atom)))
        (go dispatch-char)))
   dispatch-char
     (let ((macro-func (macro-character char)))
       (cond
         (macro-func
          (let ((vals (multiple-value-list (funcall macro-func
                                                    (meta-sexp:meta (:context))
                                                    char))))
            (if (null vals)
                (go initial)
                (return-from parse-object (first vals)))))
         ;; NOTE: omitting escape-related stuff, since we don't use it yet
         ((not (terminating-char-p char))
          (go accumulate-constituent-chars))
         (t (go read-terminating-token))))
   accumulate-constituent-chars
     ;; The reader macros should have parsed everything that isn't a
     ;; symbol
     (return-from parse-object (meta-sexp:meta (:rule symbol?)))
   read-terminating-token
     ;; The reader macros should have parsed anything that isn't a
     ;; single-character token
     (return-from parse-object (meta-sexp:meta (:type terminating-char)))))

;; (meta-sexp:defrule non-symbol-token? (&aux val) ()
;;   (:or (:and (:assign val (:rule string-literal?))
;;              (make-token :type :string :value val))
;;        ;; block delimiter (colon plus whitespace/EOF)
;;        (:and (:assign val (:checkpoint #\: (:or (:type whitespace-char)
;;                                                 :eof)))
;;              (make-token :type :colon-terminator :value val))
;;        (:and (:assign val "::")
;;              (make-token :type :double-colon :value val))
;;        (:and (:assign val #\:)
;;              (make-token :type :colon :value val))
;;        ;; Statement delimited (period plus whitespace/EOF)
;;        (:and (:assign val (:checkpoint #\. (:or (:type whitespace-char)
;;                                                 :eof)))
;;              (make-token :type :dot-terminator :value val))
;;        (:and (:assign val #\.)
;;              (make-token :type :dot :value val))
;;        (:and (:assign val #\()
;;              (make-token :type :lparen :value val))
;;        (:and (:assign val #\))
;;              (make-token :type :rparen :value val))
;;        (:and (:assign val #\[)
;;              (make-token :type :lbracket :value val))
;;        (:and (:assign val #\])
;;              (make-token :type :rbracket :value val))
;;        (:and (:assign val #\,)
;;              (make-token :type :comma :value val))
;;        (:and (:assign val #\;)
;;              (make-token :type :semicolon :value val))))





(meta-sexp:defrule boolean-literal? (&aux match val) ()
  (:with-stored-match (match)
    (:and (:or (:and (:icase (:or "YES" "TRUE"))
                     (:assign val t))
               (:and (:icase (:or "NO" "FALSE"))
                     ;; ensure the value of :assign doesn't end the match
                     (:or (:assign val nil) t)))
          ;; boolean followed by another character isn't a boolean
          (:not (:type identifier-char))
          (:return (make-boolean-value :val val)))))



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

;; Essentially token delimiters.
;; (meta-sexp:defrule non-symbol-token? (&aux val) ()
;;   (:or (:and (:assign val (:rule string-literal?))
;;              (make-token :type :string :value val))
;;        ;; block delimiter (colon plus whitespace/EOF)
;;        (:and (:assign val (:checkpoint #\: (:or (:type whitespace-char)
;;                                                 :eof)))
;;              (make-token :type :colon-terminator :value val))
;;        (:and (:assign val "::")
;;              (make-token :type :double-colon :value val))
;;        (:and (:assign val #\:)
;;              (make-token :type :colon :value val))
;;        ;; Statement delimited (period plus whitespace/EOF)
;;        (:and (:assign val (:checkpoint #\. (:or (:type whitespace-char)
;;                                                 :eof)))
;;              (make-token :type :dot-terminator :value val))
;;        (:and (:assign val #\.)
;;              (make-token :type :dot :value val))
;;        (:and (:assign val #\()
;;              (make-token :type :lparen :value val))
;;        (:and (:assign val #\))
;;              (make-token :type :rparen :value val))
;;        (:and (:assign val #\[)
;;              (make-token :type :lbracket :value val))
;;        (:and (:assign val #\])
;;              (make-token :type :rbracket :value val))
;;        (:and (:assign val #\,)
;;              (make-token :type :comma :value val))
;;        (:and (:assign val #\;)
;;              (make-token :type :semicolon :value val))))

(meta-sexp:defrule terminating-char? () ()
  (:or (:eof)
       (:type terminating-char)
       (:type whitespace-char)))

;; NOTE: this subsumes some numbers, too. use with caution.
(meta-sexp:defrule symbol? (&aux val) ()
  (:and (:with-stored-match (val)
          (:+ (:not (:rule terminating-char?))
              (:type character)))
        (make-symb :name val)))

(meta-sexp:defrule symbol-or-number? (&aux match val) ()
  (:or (:checkpoint (:assign val (:rule numeric-literal?))
                    ;; can't be any symbol-like tokens afterwards
                    (:not (:rule symbol?))
                    (:or (format *debug-io* "Winning number~%") t)
                    val)
       (:assign val (:with-stored-match (match)
                      (:rule symbol?)))))

(meta-sexp:defrule scanner (&aux val match item) ()
  (:? (:rule whitespace?))
  (:or (:rule non-symbol-token?)
       (:rule symbol-or-number?)))

(meta-sexp:defrule token (type &aux tok) ()
  (:assign tok (:rule scanner))
  (eq (token-type tok) type)
  (:return (token-value tok)))

(meta-sexp:defrule keyword? (&optional value &aux val) ()
  (:assign val (:rule token :identifier))
  (let ((kword (ident-name val)))
    (or (not value)
        (and (equalp kword value)
             kword))))

(meta-sexp:defrule literal? () ()
  (:or (:rule token :string)
       (:rule token :datetime)
       (:rule token :date)
       (:rule token :number)))

;; TODO: does funcall really have to go in here? (no. no it does not.)
(meta-sexp:defrule atom? () ()
  (:or (:rule token :string)
       (:rule token :number)
       (:rule token :symbol)
       (:rule function-call?)))

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
(meta-sexp:defrule unary-value? (&aux match op expr) ()
  (:or
   ;; Unary operator followed by expression
   (:checkpoint
    (:assign op (:rule operator?))
    (or (equalp op "+")
        (equalp op "-")
        (equalp op "NOT"))
    (:? (:rule whitespace?))
    (:assign expr (:rule expression? (right-binding-power op 1)))
    (:return (make-unary-op-node :op op :val expr)))
   (:rule atom?)
   (:and (:delimited (:? (:rule whitespace?))
                     "(" (:assign expr (:rule expression?)) ")")
         (:return expr))))

(meta-sexp:defrule operator? (&aux val match) ()
  (:with-stored-match (match)
    (:assign val (:rule token :symbol))
    (let ((name (symb-name val)))
      (meta-sexp:meta
       (:with-context (name)
         (:whole-match (:or "::"
                            ":"
                            "/"
                            "*"
                            "+"
                            "-"
                            "["
                            "<>"
                            "<="
                            ">="
                            "<"
                            ">"
                            "="
                            (:icase "MODULO")
                            (:icase "NE")
                            (:icase "LE")
                            (:icase "GE")
                            (:icase "LT")
                            (:icase "GT")
                            (:icase "EQ")
                            (:icase "NOT")
                            (:icase "AND")
                            (:icase "OR"))))))))

(meta-sexp:defrule expression? (&optional (bind-power 0) &aux match op lhs) ()
  (:assign lhs (:rule unary-value?))
  (:*
   ;; While lookahead token is a binary op with binding power >= BIND-POWER
   (:checkpoint
    (:? (:rule whitespace?))
    (:assign op (:rule operator?))
    (>= (right-binding-power op 2) bind-power)

    (:? (:rule whitespace?))
    (let* ((rbp (right-binding-power op 2))
           (rest-rbp (if (eq (op-associativity op) :left)
                         (1+ rbp)
                         rbp))
           (rhs (meta-sexp:meta (:rule expression? rest-rbp))))
      (meta-sexp:meta
       (:and rhs
             (setf lhs (make-op-node :op op :lhs lhs :rhs rhs)))))))
  (:return  lhs))

(meta-sexp:defrule statement? (&aux (parts (meta-sexp:make-list-accum)) item) ()
  (:+ (:not (:rule token :dot-terminator))
      (:assign item (:rule expression?))
      (:list-push item parts))
  (:rule token :dot-terminator)
  (:return (make-statement :parts (reverse parts))))

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
