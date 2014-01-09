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

(deftype terminating-char () '(and character (or (satisfies terminating-char-p)
                                              whitespace-char)))
(deftype non-terminating-char () '(and character (not terminating-char)))

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
                  (:or (:not (:not (:rule terminating-char?)))
                       ;; Erase the args and fail
                       (setf string-args '())))))
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

;; Matches symbols that begin a block
(defun block-symbol-p (sym)
  (optima:match sym
    ((symb- (name (or (equalp "PROCEDURE")
                      (equalp "METHOD")
                      (equalp "INTERFACE")
                      (equalp "FUNCTION")
                      (equalp "DO")
                      (equalp "DESTRUCTOR")
                      (equalp "CONSTRUCTOR")
                      (equalp "CLASS")
                      (equalp "CASE")
                      (equalp "FOR")
                      (equalp "REPEAT")
                      (equalp "COMPARES") ; From BUFFER-COMPARE
                      (equalp "EDITING")
                      (equalp "TRIGGERS")))) t)))

(meta-sexp:defrule block-symbol? (&aux obj) ()
  (:assign obj (:rule any-symbol))
  (block-symbol-p obj)
  obj)

(meta-sexp:defrule parse-colon-token (char) ()
  #\:
  (:or (:and #\:
             (make-token :type :double-colon
                         :value "::"))
       (:and (:not (:not (:or (:eof)
                              (:type whitespace-char))))
             (make-token :type :colon-terminator
                         :value ":"))
       (make-token :type :colon
                   :value ":")))

(meta-sexp:defrule parse-dot-token (char) ()
  #\.
  (:or (:and (:not (:not (:or (:eof)
                              (:type whitespace-char))))
             (make-token :type :dot-terminator
                         :value "."))
       (make-token :type :dot
                   :value ".")))

(meta-sexp:defrule parse-char-token (char type &aux c) ()
  (:and (:assign c (:type character))
        (eql c char))
  (make-token :type type
              :value (format nil "~C" char)))

(defun char-parser (type)
  (lambda (ctx char)
    (parse-char-token ctx char type)))

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
                       (#\. #'parse-dot-token)
                       (#\( (char-parser :lparen))
                       (#\) (char-parser :rparen))
                       (#\[ (char-parser :lbracket))
                       (#\] (char-parser :rbracket))
                       (#\, (char-parser :comma))
                       (#\; (char-parser :semicolon)))
      (loop for c in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         do (set-macro-chars (readtable :non-terminating t)
                             (c #'parse-numeric-or-symbol)))
      (set-macro-chars (readtable :non-terminating t)
                       (#\- #'parse-numeric-or-symbol)
                       (#\+ #'parse-numeric-or-symbol)
                       (#\/ #'parse-comment-or-symbol))
      readtable)))

(defvar *parse-readtable* (standard-readtable))

(meta-sexp:defrule parse-object (&aux char obj) ()
  (tagbody
   initial
     (cond
       ((meta-sexp:meta (:eof))
        ;; TODO: maybe do some eof-error-p stuff here
        (meta-sexp:meta
         (:return nil)))
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
                (meta-sexp:meta
                 (:return (first vals))))))
         ;; NOTE: omitting escape-related stuff, since we don't use it yet
         ((not (terminating-char-p char))
          (go accumulate-constituent-chars))
         (t (go read-terminating-token))))
   accumulate-constituent-chars
     ;; The reader macros should have parsed everything that isn't a
     ;; symbol
     (meta-sexp:meta
      (:and (:assign obj (:rule symbol?))
            (:return obj)))
   read-terminating-token
     ;; The reader macros should have parsed anything that isn't a
     ;; single-character token
     (meta-sexp:meta
      (:and (:assign obj (:type terminating-char))
            (:return obj)))))

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
    ;; ;; Statement terminator, non-binding (should maybe remove this?)
    ;; ((equal op ".")
    ;;  0)
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
    ;; ((equal op "[") ;; array reference operator
    ;;  8)
    ((or (equal op ":")
         (equal op "::")
         (equal op "[")
         (equal op ".")
         (equal op "("))
     8)
    (t (error "Unknown operator ~A" op))))

;; TODO: how does associativity work with separators like "."?
(defun op-associativity (op)
  (if (or (equalp op "AND")
          (equalp op "OR"))
      :right
      :left))

(meta-sexp:defrule token (type &aux obj) ()
  (:assign obj (:rule parse-object))
  (and (typep obj 'token)
       (eq type (token-type obj)))
  obj)

(meta-sexp:defrule the-symbol (name &aux obj) ()
  (:assign obj (:rule parse-object))
  (and (typep obj 'symb)
       (equalp (symb-name obj)
               (etypecase name
                 (string name)
                 (symbol (symbol-name name)))))
  obj)

(meta-sexp:defrule any-symbol (&aux obj) ()
  (:assign obj (:rule parse-object))
  (typep obj 'symb)
  obj)

(meta-sexp:defrule array-ref-list? (&aux expr (subscripts '())) ()
  (:delimited* (:rule token :comma)
               (:not (:rule token :rbracket))
               (:assign expr (:rule expression?))
               (:list-push expr subscripts))
  (:rule token :rbracket)
  (make-list-box :list (reverse subscripts)))

;; TODO: Add the function-form of IF in here (the ternary)
(meta-sexp:defrule unary-value? (&aux match op expr) ()
  (:or
   ;; Unary operator followed by expression
   (:checkpoint
    (:assign op (:rule operator?))
    (or (equalp op "+")
        (equalp op "-")
        (equalp op "NOT"))
    (:assign expr (:rule expression? (right-binding-power op 1)))
    (:return (make-unary-op-node :op op :val expr)))
   (:checkpoint (:rule token :lparen)
                (:assign expr (:rule expression?))
                (:rule token :rparen)
                (:return expr))
   (:checkpoint (:assign expr (:rule parse-object))
                (not (typep expr 'token))
                expr)))

(meta-sexp:defrule operator? (&aux obj match) ()
  (:with-stored-match (match)
    (:assign obj (:rule parse-object))
    (typecase obj
      (symb (meta-sexp:meta
             (:with-context ((symb-name obj))
               (:whole-match
                (:or (:icase "MODULO")
                     (:icase "NE")
                     (:icase "LE")
                     (:icase "GE")
                     (:icase "LT")
                     (:icase "GT")
                     (:icase "EQ")
                     (:icase "NOT")
                     (:icase "AND")
                     (:icase "OR")
                     "/"
                     "*"
                     "+"
                     "-"
                     "<>"
                     "<="
                     ">="
                     "<"
                     ">"
                     "=")))))
      (token (case (token-type obj)
               ((:double-colon :colon :dot :lbracket :lparen) (token-value obj))
               (t nil))))))

(meta-sexp:defrule expression? (&optional (bind-power 0) &aux match op lhs) ()
  (:assign lhs (:rule unary-value?))
  (:*
   ;; While lookahead token is a binary op with binding power >= BIND-POWER
   (:checkpoint
    (:assign op (:rule operator?))
    (>= (right-binding-power op 2) bind-power)
    (let* ((rbp (right-binding-power op 2))
           (rest-rbp (if (eq (op-associativity op) :left)
                         (1+ rbp)
                         rbp))
           (rhs (cond
                  ((equal op "[") (let ((box (meta-sexp:meta (:rule array-ref-list?))))
                                    (and box (list-box-list box))))
                  ((equal op "(") (let ((box (meta-sexp:meta (:rule param-list?))))
                                    (and box (list-box-list box))))
                  (t (meta-sexp:meta (:rule expression? rest-rbp))))))
      (meta-sexp:meta
       (:and rhs
             (setf lhs (make-op-node :op op :lhs lhs :rhs rhs)))))))
  (:return  lhs))

(meta-sexp:defrule test (&aux (objs '()) thing) ()
  (:* (:assign thing (:or (:rule expression?)
                          (:rule parse-object)))
      (:list-push thing objs))
  (reverse objs))

(meta-sexp:defrule unlabeled-statement? (&aux (parts (meta-sexp:make-list-accum)) item) ()
  (:+ (:not (:rule token :dot-terminator))
      (:assign item (:or (:rule expression?)
                         (:rule parse-object)))
      (:list-push item parts))
  (:rule token :dot-terminator)
  (:return (make-statement :parts (reverse parts))))

(meta-sexp:defrule labeled-block-statement? (&aux label block-stmt) ()
  (:not (:rule block-symbol?))
  (:assign label (:rule any-symbol))
  ;; don't want to let parse-object try to read a sequence
  #\:
  (:or (:rule whitespace?)
       (:eof))
  ;; label is followed by a block
  (:not (:not (:rule block-symbol?)))
  (:assign block-stmt (:rule unlabeled-statement?))
  (or (setf (statement-label block-stmt) label) t)
  block-stmt)

(meta-sexp:defrule statement? () ()
  (:or (:rule labeled-block-statement?)
       ;; Note that unlabeled blocks come from this rule
       (:rule unlabeled-statement?)))

(defun end-statement-p (statement)
  (optima:match statement
    ((statement- (parts (list* (symb- (name (equalp "END")))
                               (or (list (structure symb-))
                                   nil)))) t)))

;; Note: excludes END statements
(meta-sexp:defrule statement-block? (&aux s (list '())) ()
  (:* (:checkpoint (:assign s (:rule statement?))
                   (not (end-statement-p s))
                   (:list-push s list)))
  (make-statement-block :statements (reverse list)))
