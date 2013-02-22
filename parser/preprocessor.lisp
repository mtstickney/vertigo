
;;; &GLOBAL-DEFINE preprocessor directive
;; Form no. 1
(meta-sexp:defrule rule3394? () ()
  (:and "&" "GLOBAL-DEFINE" (:rule rule3392?) (:? (:rule whitespace?))
   (:rule rule3393?)))

;; preprocessor-name
(meta-sexp:defrule rule3392? () ()
)

;; definition
(meta-sexp:defrule rule3393? () ()
)

;;; &MESSAGE preprocessor directive
;; Form no. 1
(meta-sexp:defrule rule3396? () ()
  (:and "&" "MESSAGE" (:rule rule3395?)))

;; text-string
(meta-sexp:defrule rule3395? () ()
)

;;; &SCOPED-DEFINE preprocessor directive
;; Form no. 1
(meta-sexp:defrule rule3399? () ()
  (:and "&" "SCOPED-DEFINE" (:rule rule3397?) (:? (:rule whitespace?))
   (:rule rule3398?)))

;; preprocessor-name
(meta-sexp:defrule rule3397? () ()
)

;; definition
(meta-sexp:defrule rule3398? () ()
)

;;; &UNDEFINE preprocessor directive
;; Form no. 1
(meta-sexp:defrule rule3401? () ()
  (:and "&" "UNDEFINE" (:rule rule3400?)))

;; preprocessor-name
(meta-sexp:defrule rule3400? () ()
)
