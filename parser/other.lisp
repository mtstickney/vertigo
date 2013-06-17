(in-package #:vertigo)

;; TODO: Not sure about having #\* in here
(deftype identifier-char () `(or meta-sexp:alnum? (member #\_ #\! #\*)))

;;; Identifier rule
;;; Note that it is liberal and accepts "invalid" identifiers
;;; (e.g. leading numbers) as long as they don't conflict with other
;;; rules (e.g. the '-' operator rule)
(meta-sexp:defrule identifier? (&aux match) ()
  (:with-stored-match (match)
    (:type identifier-char)
    (:* (:or (:type identifier-char)
             #\-)))
  (:return (make-ident :name match)))

;;; “ ” Character-string literal
;; Form no. 1
(meta-sexp:defrule rule3608? () ()
  (:and "“" (:rule rule3606?) "“"
   (:? (:and ":" (:? (:or "R" "L" "C" "T")) (:? "U") (:? (:rule rule3607?))))))

;; characters
(meta-sexp:defrule rule3606? () ()
)

;; max-length
(meta-sexp:defrule rule3607? () ()
)

;;; { } Argument reference
;; Form no. 1
(meta-sexp:defrule rule3611? () ()
  (:or (:rule rule3609?) (:and "&" (:rule rule3610?))))

;; n
(meta-sexp:defrule rule3609? () ()
)

;; argument-name
(meta-sexp:defrule rule3610? () ()
)

;;; { } Include file reference
;; Form no. 1
(meta-sexp:defrule rule3616? () ()
  (:and (:rule rule3612?)
   (:?
    (:or (:and (:* (:rule rule3613?)) (:? (:rule whitespace?)))
     (:and
      (:*
       (:and "&" (:rule rule3614?) (:? (:rule whitespace?)) "="
        (:? (:rule whitespace?)) "\"" (:rule rule3615?) "\""))
      (:? (:rule whitespace?)))))))

;; include-file
(meta-sexp:defrule rule3612? () ()
)

;; argument
(meta-sexp:defrule rule3613? () ()
)

;; argument-name
(meta-sexp:defrule rule3614? () ()
)

;; argument-value
(meta-sexp:defrule rule3615? () ()
)

;;; { } Preprocessor name reference
;; Form no. 1
(meta-sexp:defrule rule3618? () ()
  (:and "&" (:rule rule3617?)))

;; preprocessor-name
(meta-sexp:defrule rule3617? () ()
)

;;; &IF, &THEN, &ELSEIF, &ELSE, and &ENDIF preprocessor directives
;; Form no. 1
(meta-sexp:defrule rule3624? () ()
  (:and "&" "IF" (:rule rule3619?) "&" "THEN" (:rule rule3620?)
   (:* (:? (:and "&" "ELSEIF" (:rule rule3621?) "&" "THEN" (:rule rule3622?))))
   (:? (:rule whitespace?)) (:? (:and "ELSE" (:rule rule3623?))) "&" "ENDIF"))

;; expression
(meta-sexp:defrule rule3619? () ()
)

;; block
(meta-sexp:defrule rule3620? () ()
)

;; expression
(meta-sexp:defrule rule3621? () ()
)

;; block
(meta-sexp:defrule rule3622? () ()
)

;; block
(meta-sexp:defrule rule3623? () ()
)

;;; /* Comments */
;; Form no. 1
(meta-sexp:defrule rule3626? () ()
  (:and "/*" (:rule rule3625?) "*/"))

;; comment
(meta-sexp:defrule rule3625? () ()
)

;;; CALL Statement
;; Form no. 1
(meta-sexp:defrule rule3629? () ()
  (:and "CALL" (:rule rule3627?) (:* (:? (:rule rule3628?)))
   (:? (:rule whitespace?))))

;; routine-identifier
(meta-sexp:defrule rule3627? () ()
)

;; argument
(meta-sexp:defrule rule3628? () ()
)

;;; Logical values
;; Form no. 1
(meta-sexp:defrule rule3630? () ()
  (:or (:? (:or "YES" "TRUE")) (:? (:or "NO" "FALSE"))))

;;; SUPER system reference
;; Form no. 1
(meta-sexp:defrule rule3634? () ()
  (:and "SUPER" ":" (:rule rule3631?) "("
   (:?
    (:and (:rule rule3632?) (:* (:? (:and "," (:rule rule3633?))))
     (:? (:rule whitespace?))))
   ")" (:? "NO-ERROR")))

;; method-name
(meta-sexp:defrule rule3631? () ()
)

;; parameter
(meta-sexp:defrule rule3632? () ()
)

;; parameter
(meta-sexp:defrule rule3633? () ()
)

;;; THIS-OBJECT system reference
;; Form no. 1
(meta-sexp:defrule rule3635? () ()
  "THIS-OBJECT")
