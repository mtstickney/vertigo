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

(meta-sexp:defrule numeric-literal? (&aux match) ()
  (:with-stored-match (match)
    (:? (:* (:type digit))
        (:? #\.))
    (:+ (:type digit))))

(meta-sexp:defrule numeric-primary? () ()
  ;; Optional unary op
  (:? (:or (:and "+" (:? (:rule whitespace?)))
           (:and "-" (:? (:rule whitespace?)))))
  (:or (:rule numeric-literal?)
       (:rule place?)
       (:and "("
             (:? (:rule whitespace?))
             (:rule numeric-expression?)
             (:? (:rule whitespace?))
             ")")
       (:rule :function-call)))

(defun numeric-binding-power (op)
  (cond
    ((or (equal op "+")
         (equal op "-"))
     1)
    ((or (equal op "/")
         (equal op "*"))
     2)))

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
