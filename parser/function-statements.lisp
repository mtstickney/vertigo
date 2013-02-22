
;;; CURRENT-LANGUAGE statement
;; Form no. 1
(meta-sexp:defrule rule3129? () ()
  (:and "CURRENT-LANGUAGE" "=" (:rule rule3128?)))

;; string-expression
(meta-sexp:defrule rule3128? () ()
)

;;; CURRENT-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule3133? () ()
  (:and "CURRENT-VALUE" "(" (:rule rule3130?) (:? (:and "," (:rule rule3131?)))
   ")" "=" (:rule rule3132?)))

;; sequence
(meta-sexp:defrule rule3130? () ()
)

;; logical-dbname
(meta-sexp:defrule rule3131? () ()
)

;; expression
(meta-sexp:defrule rule3132? () ()
)

;;; DYNAMIC-CURRENT-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule3137? () ()
  (:and "DYNAMIC-CURRENT-VALUE" "(" (:rule rule3134?) "," (:rule rule3135?) ")"
   "=" (:rule rule3136?)))

;; sequence-exp
(meta-sexp:defrule rule3134? () ()
)

;; logical-dbname-exp
(meta-sexp:defrule rule3135? () ()
)

;; expression
(meta-sexp:defrule rule3136? () ()
)

;;; ENTRY statement
;; Form no. 1
(meta-sexp:defrule rule3142? () ()
  (:and "ENTRY" "(" (:rule rule3138?) "," (:rule rule3139?)
   (:? (:and "," (:rule rule3140?))) ")" "=" (:rule rule3141?)))

;; element
(meta-sexp:defrule rule3138? () ()
)

;; list
(meta-sexp:defrule rule3139? () ()
)

;; character
(meta-sexp:defrule rule3140? () ()
)

;; expression
(meta-sexp:defrule rule3141? () ()
)

;;; EXTENT statement
;; Form no. 1
(meta-sexp:defrule rule3145? () ()
  (:and "EXTENT" "(" (:rule rule3143?) ")" "=" (:rule rule3144?)
   (:? "NO-ERROR")))

;; array
(meta-sexp:defrule rule3143? () ()
)

;; expression
(meta-sexp:defrule rule3144? () ()
)

;;; FRAME-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule3147? () ()
  (:and "FRAME-VALUE" "=" (:rule rule3146?)))

;; expression
(meta-sexp:defrule rule3146? () ()
)

;;; IF...THEN...ELSE statement
;; Form no. 1
(meta-sexp:defrule rule3153? () ()
  (:and "IF" (:rule rule3148?) "THEN" (:or (:rule rule3149?) (:rule rule3150?))
   (:? (:and "ELSE" (:or (:rule rule3151?) (:rule rule3152?))))))

;; expression
(meta-sexp:defrule rule3148? () ()
)

;; block
(meta-sexp:defrule rule3149? () ()
)

;; statement
(meta-sexp:defrule rule3150? () ()
)

;; block
(meta-sexp:defrule rule3151? () ()
)

;; statement
(meta-sexp:defrule rule3152? () ()
)

;;; LENGTH statement
;; Form no. 1
(meta-sexp:defrule rule3156? () ()
  (:and "LENGTH" "(" (:rule rule3154?) ")" "=" (:rule rule3155?)))

;; variable
(meta-sexp:defrule rule3154? () ()
)

;; expression
(meta-sexp:defrule rule3155? () ()
)

;;; PROMSGS statement
;; Form no. 1
(meta-sexp:defrule rule3158? () ()
  (:and "PROMSGS" "=" (:rule rule3157?)))

;; string-expression
(meta-sexp:defrule rule3157? () ()
)

;;; PROPATH statement
;; Form no. 1
(meta-sexp:defrule rule3160? () ()
  (:and "PROPATH" "=" (:rule rule3159?)))

;; string-expression
(meta-sexp:defrule rule3159? () ()
)

;;; RAW statement
;; Form no. 1
(meta-sexp:defrule rule3165? () ()
  (:and "RAW" "(" (:rule rule3161?)
   (:? (:and "," (:rule rule3162?) (:? (:and "," (:rule rule3163?))))) ")" "="
   (:rule rule3164?)))

;; field
(meta-sexp:defrule rule3161? () ()
)

;; position
(meta-sexp:defrule rule3162? () ()
)

;; length
(meta-sexp:defrule rule3163? () ()
)

;; expression
(meta-sexp:defrule rule3164? () ()
)

;;; SEEK statement
;; Form no. 1
(meta-sexp:defrule rule3168? () ()
  (:and "SEEK" (:or "INPUT" "OUTPUT" (:and "STREAM" (:rule rule3166?))) "TO"
   (:or (:rule rule3167?) "END")))

;; stream
(meta-sexp:defrule rule3166? () ()
)

;; expression
(meta-sexp:defrule rule3167? () ()
)

;;; SUBSTRING statement
;; Form no. 1
(meta-sexp:defrule rule3174? () ()
  (:and "SUBSTRING" "(" (:rule rule3169?) "," (:rule rule3170?)
   (:? (:and "," (:rule rule3171?) (:? (:and "," (:rule rule3172?))))) ")" "="
   (:rule rule3173?)))

;; source
(meta-sexp:defrule rule3169? () ()
)

;; position
(meta-sexp:defrule rule3170? () ()
)

;; length
(meta-sexp:defrule rule3171? () ()
)

;; type
(meta-sexp:defrule rule3172? () ()
)

;; expression
(meta-sexp:defrule rule3173? () ()
)

;;; TERMINAL statement
;; Form no. 1
(meta-sexp:defrule rule3176? () ()
  (:and "TERMINAL" "=" (:rule rule3175?)))

;; termid
(meta-sexp:defrule rule3175? () ()
)
