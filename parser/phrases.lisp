(in-package :vertigo)

;;; Aggregate phrase
;; Form no. 1
(meta-sexp:defrule rule3404? () ()
  (:and
   (:*
    (:or "AVERAGE" "COUNT" "MAXIMUM" "MINIMUM" "TOTAL" "SUB-AVERAGE"
     "SUB-COUNT" "SUB-MAXIMUM" "SUB-MINIMUM" "SUB-TOTAL"))
   (:? (:rule whitespace?)) (:? (:and "LABEL" (:rule rule3402?)))
   (:* (:? (:and "BY" (:rule rule3403?)))) (:? (:rule whitespace?))))

;; aggr-label
(meta-sexp:defrule rule3402? () ()
)

;; break-group
(meta-sexp:defrule rule3403? () ()
)

;;; AT phrase
(meta-sexp:defrule at-phrase? () ()
  (:or (:rule at-rect?)
       (:rule at-position?)))

;; Form no. 1
(meta-sexp:defrule at-position? (&aux n) ()
  (:and (:icase "AT") (:rule whitespace?) (:assign n (:rule expression?)))
  (:return n))

;; Form no. 2
(meta-sexp:defrule at-rect? (&aux opt (opts (dict))) ()
  (:icase "AT")
  (:rule whitespace?)
  (:or (:checkpoint (:icase "X")
                    (:rule whitespace?)
                    (:assign opt (:rule expression?))
                    (:or (setf (gethash :x opts) opt) t))
       (:checkpoint (:icase "X-OF")
                    (:rule whitespace?)
                    (:assign opt (:rule expression?))
                    (:or (setf (gethash :x-of opts) opt) t)))
  (:rule whitespace?)
  (:or (:checkpoint (:icase "Y")
                    (:rule whitespace?)
                    (:assign opt (:rule expression?))
                    (:or (setf (gethash :y opts) opt) t))
       (:checkpoint (:icase "Y-OF")
                    (:rule whitespace?)
                    (:assign opt (:rule expression?))
                    (:or (setf (gethash :y-of opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "COLON-ALIGNED")
                              (:or (setf (gethash :align opts) :colon) t))
                        (:and (:icase "LEFT-ALIGNED")
                              (:or (setf (gethash :align opts) :left) t))
                        (:and (:icase "RIGHT-ALIGNED")
                              (:or (setf (gethash :align opts) :right) t)))))
  (:return opts))

;; x
(meta-sexp:defrule rule3407? () ()
)

;; reference-point
(meta-sexp:defrule rule3408? () ()
)

;; y
(meta-sexp:defrule rule3409? () ()
)

;; reference-point
(meta-sexp:defrule rule3410? () ()
)


;; Form no. 3
(meta-sexp:defrule rule3416? () ()
  (:and "AT"
   (:or (:and "COLUMN" (:rule rule3412?)) (:and "COLUMN-OF" (:rule rule3413?)))
   (:or (:and "ROW" (:rule rule3414?)) (:and "ROW-OF" (:rule rule3415?)))
   (:? (:or "COLON-ALIGNED" "LEFT-ALIGNED" "RIGHT-ALIGNED"))))

;; column
(meta-sexp:defrule rule3412? () ()
)

;; reference-point
(meta-sexp:defrule rule3413? () ()
)

;; row
(meta-sexp:defrule rule3414? () ()
)

;; reference-point
(meta-sexp:defrule rule3415? () ()
)

;;; COLOR phrase
;; Form no. 1
(meta-sexp:defrule rule3423? () ()
  (:or "NORMAL" "INPUT" "MESSAGES" (:rule rule3417?) (:rule rule3418?)
   (:and (:? "BLINK-") (:? "BRIGHT-") (:? (:rule rule3419?))
    (:? (:rule rule3420?)))
   (:and (:? "BLINK-") (:? "RVV-") (:? "UNDERLINE-") (:? "BRIGHT-")
    (:? (:rule rule3421?)))
   (:and "VALUE" "(" (:rule rule3422?) ")")))

;; protermcap-attribute
(meta-sexp:defrule rule3417? () ()
)

;; dos-hex-attribute
(meta-sexp:defrule rule3418? () ()
)

;; fgnd-color
(meta-sexp:defrule rule3419? () ()
)

;; bgnd-color
(meta-sexp:defrule rule3420? () ()
)

;; fgnd-color
(meta-sexp:defrule rule3421? () ()
)

;; expression
(meta-sexp:defrule rule3422? () ()
)

;;; COMBO-BOX phrase
;; Form no. 1
(meta-sexp:defrule rule3428? () ()
  (:and "COMBO-BOX"
   (:?
    (:or (:and "LIST-ITEMS" (:rule rule3424?))
     (:and "LIST-ITEM-PAIRS" (:rule rule3425?))))
   (:? (:and "INNER-LINES" (:rule rule3426?))) (:? "SORT")
   (:? (:or "DROP-DOWN" "DROP-DOWN-LIST"))
   (:? (:and "MAX-CHARS" (:rule rule3427?)))
   (:? (:and "AUTO-COMPLETION" (:? "UNIQUE-MATCH")))))

;; item-list
(meta-sexp:defrule rule3424? () ()
)

;; item-pair-list
(meta-sexp:defrule rule3425? () ()
)

;; lines
(meta-sexp:defrule rule3426? () ()
)

;; characters
(meta-sexp:defrule rule3427? () ()
)


;; Form no. 2
(meta-sexp:defrule rule3435? () ()
  (:and "COMBO-BOX"
   (:?
    (:or (:and "LIST-ITEMS" (:rule rule3429?))
     (:and "LIST-ITEM-PAIRS" (:rule rule3430?))))
   (:? (:and "INNER-LINES" (:rule rule3431?))) (:? (:rule rule3432?))
   (:? "SORT") (:? (:and "TOOLTIP" (:rule rule3433?)))
   (:? (:or "SIMPLE" "DROP-DOWN" "DROP-DOWN-LIST"))
   (:? (:and "MAX-CHARS" (:rule rule3434?)))
   (:? (:and "AUTO-COMPLETION" (:? "UNIQUE-MATCH")))))

;; item-list
(meta-sexp:defrule rule3429? () ()
)

;; item-pair-list
(meta-sexp:defrule rule3430? () ()
)

;; lines
(meta-sexp:defrule rule3431? () ()
)

;; size-phrase
(meta-sexp:defrule rule3432? () ()
)

;; tooltip
(meta-sexp:defrule rule3433? () ()
)

;; characters
(meta-sexp:defrule rule3434? () ()
)

;;; EDITING phrase
;; Form no. 1
(meta-sexp:defrule rule3438? () ()
  (:and (:? (:and (:rule rule3436?) ":")) "EDITING" ":" (:* (:rule rule3437?))
   (:? (:rule whitespace?)) "END"))

;; label
(meta-sexp:defrule rule3436? () ()
)

;; statement
(meta-sexp:defrule rule3437? () ()
)

;;; EDITOR phrase
;; Form no. 1
(meta-sexp:defrule rule3446? () ()
  (:and "EDITOR"
   (:or (:rule rule3439?)
    (:and "INNER-CHARS" (:rule rule3440?) "INNER-LINES" (:rule rule3441?)))
   (:? (:and "BUFFER-CHARS" (:rule rule3442?)))
   (:? (:and "BUFFER-LINES" (:rule rule3443?))) (:? "LARGE")
   (:? (:and "MAX-CHARS" (:rule rule3444?))) (:? "NO-BOX") (:? "NO-WORD-WRAP")
   (:? "SCROLLBAR-HORIZONTAL") (:? "SCROLLBAR-VERTICAL")
   (:? (:and "TOOLTIP" (:rule rule3445?)))))

;; size-phrase
(meta-sexp:defrule rule3439? () ()
)

;; characters
(meta-sexp:defrule rule3440? () ()
)

;; lines
(meta-sexp:defrule rule3441? () ()
)

;; chars
(meta-sexp:defrule rule3442? () ()
)

;; lines
(meta-sexp:defrule rule3443? () ()
)

;; characters
(meta-sexp:defrule rule3444? () ()
)

;; tooltip
(meta-sexp:defrule rule3445? () ()
)

;;; Format phrase
;; Form no. 1
(meta-sexp:defrule rule3466? () ()
  (:and (:? (:rule rule3447?))
   (:? (:or (:and "AS" (:rule rule3448?)) (:and "LIKE" (:rule rule3449?))))
   (:? (:or "ATTR-SPACE" "NO-ATTR-SPACE")) (:? "AUTO-RETURN")
   (:? (:and "BGCOLOR" (:rule rule3450?))) (:? "BLANK")
   (:? (:or (:and "COLON" (:rule rule3451?)) (:and "TO" (:rule rule3452?))))
   (:? (:and "COLUMN-LABEL" (:rule rule3453?))) (:? "DEBLANK")
   (:? (:and "DCOLOR" (:rule rule3454?))) (:? "DISABLE-AUTO-ZAP")
   (:? (:and "FGCOLOR" (:rule rule3455?))) (:? (:and "FONT" (:rule rule3456?)))
   (:? (:and "FORMAT" (:rule rule3457?))) (:? (:and "HELP" (:rule rule3458?)))
   (:?
    (:or
     (:and "LABEL" (:rule rule3459?) (:* (:? (:and "," (:rule rule3460?))))
      (:? (:rule whitespace?)))
     "NO-LABELS"))
   (:? "NO-TAB-STOP") (:? (:and "PFCOLOR" (:rule rule3461?)))
   (:? (:and "VALIDATE" "(" (:rule rule3462?) "," (:rule rule3463?) ")"))
   (:? (:rule rule3464?)) (:? (:and "WIDGET-ID" (:rule rule3465?)))))

;; at-phrase
(meta-sexp:defrule rule3447? () ()
)

;; datatype
(meta-sexp:defrule rule3448? () ()
)

;; field
(meta-sexp:defrule rule3449? () ()
)

;; expression
(meta-sexp:defrule rule3450? () ()
)

;; n
(meta-sexp:defrule rule3451? () ()
)

;; n
(meta-sexp:defrule rule3452? () ()
)

;; label
(meta-sexp:defrule rule3453? () ()
)

;; expression
(meta-sexp:defrule rule3454? () ()
)

;; expression
(meta-sexp:defrule rule3455? () ()
)

;; expression
(meta-sexp:defrule rule3456? () ()
)

;; expression
(meta-sexp:defrule rule3457? () ()
)

;; string
(meta-sexp:defrule rule3458? () ()
)

;; label
(meta-sexp:defrule rule3459? () ()
)

;; label
(meta-sexp:defrule rule3460? () ()
)

;; expression
(meta-sexp:defrule rule3461? () ()
)

;; condition
(meta-sexp:defrule rule3462? () ()
)

;; msg-expression
(meta-sexp:defrule rule3463? () ()
)

;; view-as-phrase
(meta-sexp:defrule rule3464? () ()
)

;; id-number
(meta-sexp:defrule rule3465? () ()
)

;;; Frame phrase
;; Form no. 1
(meta-sexp:defrule rule3487? () ()
  (:and "WITH" (:? (:and "ACCUM" (:? (:rule rule3467?))))
   (:? (:rule rule3468?)) (:? (:or "ATTR-SPACE" "NO-ATTR-SPACE"))
   (:? (:and "CANCEL-BUTTON" (:rule rule3469?))) (:? "CENTERED")
   (:? (:rule rule3470?)) (:? (:and "COLUMN" (:rule rule3471?)))
   (:? (:and (:rule rule3472?) "COLUMNS")) (:? "CONTEXT-HELP")
   (:? (:and "CONTEXT-HELP-FILE" (:rule rule3473?)))
   (:? (:and "DEFAULT-BUTTON" (:rule rule3474?))) (:? "DROP-TARGET")
   (:? (:and (:? (:rule rule3475?)) "DOWN")) (:? "EXPORT")
   (:? (:and "WIDGET-ID" (:rule rule3476?)))
   (:? (:and "FONT" (:rule rule3477?))) (:? (:and "FRAME" (:rule rule3478?)))
   (:? (:or "INHERIT-BGCOLOR" "NO-INHERIT-BGCOLOR"))
   (:? (:or "INHERIT-FGCOLOR" "NO-INHERIT-FGCOLOR")) (:? "KEEP-TAB-ORDER")
   (:? "NO-BOX") (:? "NO-HIDE") (:? "NO-LABELS") (:? "USE-DICT-EXPS")
   (:? "NO-VALIDATE") (:? "NO-AUTO-VALIDATE") (:? "NO-HELP")
   (:? "NO-UNDERLINE") (:? "OVERLAY") (:? (:or "PAGE-BOTTOM" "PAGE-TOP"))
   (:? (:and "RETAIN" (:rule rule3479?))) (:? (:and "ROW" (:rule rule3480?)))
   (:? (:or "SCREEN-IO" "STREAM-IO")) (:? (:and "SCROLL" (:rule rule3481?)))
   (:? "SCROLLABLE") (:? "SIDE-LABELS") (:? (:rule rule3482?))
   (:? (:and "STREAM" (:rule rule3483?))) (:? "THREE-D") (:? (:rule rule3484?))
   (:? "TOP-ONLY") (:? "USE-TEXT")
   (:? (:and "V6FRAME" (:? (:or "USE-REVVIDEO" "USE-UNDERLINE"))))
   (:? (:and "VIEW-AS" "DIALOG-BOX")) (:? (:and "WIDTH" (:rule rule3485?)))
   (:? (:and "IN" "WINDOW" (:rule rule3486?)))))

;; max-length
(meta-sexp:defrule rule3467? () ()
)

;; at-phrase
(meta-sexp:defrule rule3468? () ()
)

;; button-name
(meta-sexp:defrule rule3469? () ()
)

;; color-specification
(meta-sexp:defrule rule3470? () ()
)

;; expression
(meta-sexp:defrule rule3471? () ()
)

;; n
(meta-sexp:defrule rule3472? () ()
)

;; help-file-name
(meta-sexp:defrule rule3473? () ()
)

;; button-name
(meta-sexp:defrule rule3474? () ()
)

;; expression
(meta-sexp:defrule rule3475? () ()
)

;; id-number
(meta-sexp:defrule rule3476? () ()
)

;; expression
(meta-sexp:defrule rule3477? () ()
)

;; frame
(meta-sexp:defrule rule3478? () ()
)

;; n
(meta-sexp:defrule rule3479? () ()
)

;; expression
(meta-sexp:defrule rule3480? () ()
)

;; n
(meta-sexp:defrule rule3481? () ()
)

;; size-phrase
(meta-sexp:defrule rule3482? () ()
)

;; stream
(meta-sexp:defrule rule3483? () ()
)

;; title-phrase
(meta-sexp:defrule rule3484? () ()
)

;; n
(meta-sexp:defrule rule3485? () ()
)

;; window
(meta-sexp:defrule rule3486? () ()
)

;;; Image phrase
;; Form no. 1
(meta-sexp:defrule rule3494? () ()
  (:and (:or "IMAGE-SIZE" "IMAGE-SIZE-CHARS" "IMAGE-SIZE-PIXELS")
   (:rule rule3488?) "BY" (:rule rule3489?)
   (:?
    (:and "FROM"
     (:or (:and "X" (:rule rule3490?) "Y" (:rule rule3491?))
      (:and "ROW" (:rule rule3492?) "COLUMN" (:rule rule3493?)))))))

;; width
(meta-sexp:defrule rule3488? () ()
)

;; height
(meta-sexp:defrule rule3489? () ()
)

;; n
(meta-sexp:defrule rule3490? () ()
)

;; n
(meta-sexp:defrule rule3491? () ()
)

;; n
(meta-sexp:defrule rule3492? () ()
)

;; n
(meta-sexp:defrule rule3493? () ()
)


;; Form no. 2
(meta-sexp:defrule rule3502? () ()
  (:and "FILE" (:rule rule3495?)
   (:?
    (:and (:or "IMAGE-SIZE" "IMAGE-SIZE-CHARS" "IMAGE-SIZE-PIXELS")
     (:rule rule3496?) "BY" (:rule rule3497?)))
   (:?
    (:and "FROM"
     (:or (:and "X" (:rule rule3498?) "Y" (:rule rule3499?))
      (:and "ROW" (:rule rule3500?) "COLUMN" (:rule rule3501?)))))))

;; name
(meta-sexp:defrule rule3495? () ()
)

;; width
(meta-sexp:defrule rule3496? () ()
)

;; height
(meta-sexp:defrule rule3497? () ()
)

;; n
(meta-sexp:defrule rule3498? () ()
)

;; n
(meta-sexp:defrule rule3499? () ()
)

;; n
(meta-sexp:defrule rule3500? () ()
)

;; n
(meta-sexp:defrule rule3501? () ()
)

;;; NEW phrase
;; Form no. 1
(meta-sexp:defrule rule3506? () ()
  (:and "NEW" (:rule rule3503?) "("
   (:?
    (:and (:rule rule3504?) (:* (:? (:and "," (:rule rule3505?))))
     (:? (:rule whitespace?))))
   ")"))

;; type-name
(meta-sexp:defrule rule3503? () ()
)

;; parameter
(meta-sexp:defrule rule3504? () ()
)

;; parameter
(meta-sexp:defrule rule3505? () ()
)

;;; ON ENDKEY phrase
;; Form no. 1
(meta-sexp:defrule rule3512? () ()
  (:and "ON" "ENDKEY" "UNDO" (:? (:rule rule3507?))
   (:?
    (:or (:and "," "LEAVE" (:? (:rule rule3508?)))
     (:and "," "NEXT" (:? (:rule rule3509?)))
     (:and "," "RETRY" (:? (:rule rule3510?)))
     (:and "," "RETURN" (:or "ERROR" "NO-APPLY") (:? (:rule rule3511?)))))))

;; label1
(meta-sexp:defrule rule3507? () ()
)

;; label2
(meta-sexp:defrule rule3508? () ()
)

;; label2
(meta-sexp:defrule rule3509? () ()
)

;; label1
(meta-sexp:defrule rule3510? () ()
)

;; return-string
(meta-sexp:defrule rule3511? () ()
)

;;; ON ERROR phrase
;; Form no. 1
(meta-sexp:defrule rule3518? () ()
  (:and "ON" "ERROR" "UNDO" (:? (:rule rule3513?))
   (:?
    (:or (:and "," "LEAVE" (:? (:rule rule3514?)))
     (:and "," "NEXT" (:? (:rule rule3515?)))
     (:and "," "RETRY" (:? (:rule rule3516?)))
     (:and "," "RETURN" (:or "ERROR" "NO-APPLY") (:? (:rule rule3517?)))))))

;; label1
(meta-sexp:defrule rule3513? () ()
)

;; label2
(meta-sexp:defrule rule3514? () ()
)

;; label2
(meta-sexp:defrule rule3515? () ()
)

;; label1
(meta-sexp:defrule rule3516? () ()
)

;; return-string
(meta-sexp:defrule rule3517? () ()
)

;;; ON QUIT phrase
;; Form no. 1
(meta-sexp:defrule rule3524? () ()
  (:and "ON" "QUIT" (:? (:and "UNDO" (:? (:rule rule3519?))))
   (:?
    (:or (:and "," "LEAVE" (:? (:rule rule3520?)))
     (:and "," "NEXT" (:? (:rule rule3521?)))
     (:and "," "RETRY" (:? (:rule rule3522?)))
     (:and "," "RETURN" (:or "ERROR" "NO-APPLY") (:? (:rule rule3523?)))))))

;; label1
(meta-sexp:defrule rule3519? () ()
)

;; label2
(meta-sexp:defrule rule3520? () ()
)

;; label2
(meta-sexp:defrule rule3521? () ()
)

;; label1
(meta-sexp:defrule rule3522? () ()
)

;; return-string
(meta-sexp:defrule rule3523? () ()
)

;;; ON STOP phrase
;; Form no. 1
(meta-sexp:defrule rule3530? () ()
  (:and "ON" "STOP" "UNDO" (:? (:rule rule3525?))
   (:?
    (:or (:and "," "LEAVE" (:? (:rule rule3526?)))
     (:and "," "NEXT" (:? (:rule rule3527?)))
     (:and "," "RETRY" (:? (:rule rule3528?)))
     (:and "," "RETURN" (:or "ERROR" "NO-APPLY") (:? (:rule rule3529?)))))))

;; label1
(meta-sexp:defrule rule3525? () ()
)

;; label2
(meta-sexp:defrule rule3526? () ()
)

;; label2
(meta-sexp:defrule rule3527? () ()
)

;; label1
(meta-sexp:defrule rule3528? () ()
)

;; return-string
(meta-sexp:defrule rule3529? () ()
)

;;; PRESELECT phrase
;; Form no. 1
(meta-sexp:defrule rule3537? () ()
  (:and "PRESELECT" (:? (:or "EACH" "FIRST" "LAST")) (:rule rule3531?)
   (:* (:? (:and "," (:? (:or "EACH" "FIRST" "LAST")) (:rule rule3532?))))
   (:? (:rule whitespace?))
   (:?
    (:and (:? "BREAK")
     (:*
      (:or (:and "BY" (:rule rule3533?) (:? "DESCENDING"))
       (:and "COLLATE" "(" (:rule rule3534?) "," (:rule rule3535?)
        (:? (:and "," (:rule rule3536?))) ")" (:? "DESCENDING"))))
     (:? (:rule whitespace?))))))

;; record-phrase
(meta-sexp:defrule rule3531? () ()
)

;; record-phrase
(meta-sexp:defrule rule3532? () ()
)

;; expression
(meta-sexp:defrule rule3533? () ()
)

;; string
(meta-sexp:defrule rule3534? () ()
)

;; strength
(meta-sexp:defrule rule3535? () ()
)

;; collation
(meta-sexp:defrule rule3536? () ()
)

;;; QUERY-TUNING phrase
;; Form no. 1
(meta-sexp:defrule rule3540? () ()
  (:and "QUERY-TUNING" "(" (:? (:or "ARRAY-MESSAGE" "NO-ARRAY-MESSAGE"))
   (:? (:or "BIND-WHERE" "NO-BIND-WHERE"))
   (:? (:and "CACHE-SIZE" (:rule rule3538?)))
   (:?
    (:or (:and "DEBUG" (:or "SQL" (:and "EXTENDED" (:rule rule3539?))))
     "NO-DEBUG"))
   (:? (:or "INDEX-HINT" "NO-INDEX-HINT"))
   (:? (:or "JOIN-BY-SQLDB" "NO-JOIN-BY-SQLDB"))
   (:? (:or "LOOKAHEAD" "NO-LOOKAHEAD")) (:? "ORDERED-JOIN")
   (:? "REVERSE-FROM")
   (:? (:or "SEPARATE-CONNECTION" "NO-SEPARATE-CONNECTION")) ")"))

;; integer
(meta-sexp:defrule rule3538? () ()
)

;; diag-option
(meta-sexp:defrule rule3539? () ()
)

;;; RADIO-SET phrase
;; Form no. 1
(meta-sexp:defrule rule3547? () ()
  (:and "RADIO-SET" (:? (:or (:and "HORIZONTAL" (:? "EXPAND")) "VERTICAL"))
   (:? (:rule rule3541?)) "RADIO-BUTTONS" (:rule rule3542?) ","
   (:rule rule3543?)
   (:* (:? (:and "," (:rule rule3544?) "," (:rule rule3545?))))
   (:? (:rule whitespace?)) (:? (:and "TOOLTIP" (:rule rule3546?)))))

;; size-phrase
(meta-sexp:defrule rule3541? () ()
)

;; label
(meta-sexp:defrule rule3542? () ()
)

;; value
(meta-sexp:defrule rule3543? () ()
)

;; label
(meta-sexp:defrule rule3544? () ()
)

;; value
(meta-sexp:defrule rule3545? () ()
)

;; tooltip
(meta-sexp:defrule rule3546? () ()
)

;;; Record phrase
;; Form no. 1
(meta-sexp:defrule rule3558? () ()
  (:and (:rule rule3548?) (:? (:rule rule3549?)) (:? (:rule rule3550?))
   (:? (:and (:? "LEFT") "OUTER-JOIN")) (:? (:and "OF" (:rule rule3551?)))
   (:? (:and "WHERE" (:rule rule3552?)))
   (:? (:and "USE-INDEX" (:rule rule3553?)))
   (:?
    (:and "USING" (:? (:and "FRAME" (:rule rule3554?))) (:rule rule3555?)
     (:*
      (:?
       (:and "AND" (:? (:and "FRAME" (:rule rule3556?))) (:rule rule3557?))))
     (:? (:rule whitespace?))))
   (:? (:or "SHARE-LOCK" "EXCLUSIVE-LOCK" "NO-LOCK")) (:? "NO-PREFETCH")))

;; record
(meta-sexp:defrule rule3548? () ()
)

;; field-list
(meta-sexp:defrule rule3549? () ()
)

;; constant
(meta-sexp:defrule rule3550? () ()
)

;; table
(meta-sexp:defrule rule3551? () ()
)

;; expression
(meta-sexp:defrule rule3552? () ()
)

;; index
(meta-sexp:defrule rule3553? () ()
)

;; frame
(meta-sexp:defrule rule3554? () ()
)

;; field
(meta-sexp:defrule rule3555? () ()
)

;; frame
(meta-sexp:defrule rule3556? () ()
)

;; field
(meta-sexp:defrule rule3557? () ()
)

;;; SELECTION-LIST phrase
;; Form no. 1
(meta-sexp:defrule rule3565? () ()
  (:and "SELECTION-LIST" (:? (:or "SINGLE" "MULTIPLE")) (:? "NO-DRAG")
   (:or (:and "LIST-ITEMS" (:rule rule3559?))
    (:and "LIST-ITEM-PAIRS" (:rule rule3560?)))
   (:? "SCROLLBAR-HORIZONTAL") (:? "SCROLLBAR-VERTICAL")
   (:or (:rule rule3561?)
    (:and "INNER-CHARS" (:rule rule3562?) "INNER-LINES" (:rule rule3563?)))
   (:? "SORT") (:? (:and "TOOLTIP" (:rule rule3564?)))))

;; item-list
(meta-sexp:defrule rule3559? () ()
)

;; item-pair-list
(meta-sexp:defrule rule3560? () ()
)

;; size-phrase
(meta-sexp:defrule rule3561? () ()
)

;; cols
(meta-sexp:defrule rule3562? () ()
)

;; rows
(meta-sexp:defrule rule3563? () ()
)

;; tooltip
(meta-sexp:defrule rule3564? () ()
)

;;; SIZE phrase
;; Form no. 1
(meta-sexp:defrule rule3568? () ()
  (:and (:or "SIZE" "SIZE-CHARS" "SIZE-PIXELS") (:rule rule3566?) "BY"
   (:rule rule3567?)))

;; width
(meta-sexp:defrule rule3566? () ()
)

;; height
(meta-sexp:defrule rule3567? () ()
)

;;; SLIDER phrase
;; Form no. 1
(meta-sexp:defrule rule3574? () ()
  (:and "VIEW-AS" "SLIDER" "MAX-VALUE" (:rule rule3569?) "MIN-VALUE"
   (:rule rule3570?) (:? (:or "HORIZONTAL" "VERTICAL")) (:? "NO-CURRENT-VALUE")
   (:? "LARGE-TO-SMALL")
   (:?
    (:and "TIC-MARKS" (:or "NONE" "TOP" "BOTTOM" "LEFT" "RIGHT" "BOTH")
     (:? (:and "FREQUENCY" (:rule rule3571?)))))
   (:? (:and "TOOLTIP" (:rule rule3572?))) (:? (:rule rule3573?))))

;; max-value
(meta-sexp:defrule rule3569? () ()
)

;; min-value
(meta-sexp:defrule rule3570? () ()
)

;; n
(meta-sexp:defrule rule3571? () ()
)

;; tooltip
(meta-sexp:defrule rule3572? () ()
)

;; size-phrase
(meta-sexp:defrule rule3573? () ()
)

;;; Trigger phrase
;; Form no. 1
(meta-sexp:defrule rule3580? () ()
  (:and "TRIGGERS" ":"
   (:*
    (:and "ON" (:rule rule3575?) (:? "ANYWHERE")
     (:or (:rule rule3576?)
      (:and "PERSISTENT" "RUN" (:rule rule3577?)
       (:? (:and "IN" (:rule rule3578?)))
       (:? (:and "(" (:rule rule3579?) ")"))))))
   (:? (:rule whitespace?)) "END" (:? "TRIGGERS")))

;; event-list
(meta-sexp:defrule rule3575? () ()
)

;; trigger-block
(meta-sexp:defrule rule3576? () ()
)

;; procedure
(meta-sexp:defrule rule3577? () ()
)

;; handle
(meta-sexp:defrule rule3578? () ()
)

;; input-parameters
(meta-sexp:defrule rule3579? () ()
)

;;; VIEW-AS phrase
;; Form no. 1
(meta-sexp:defrule rule3582? () ()
  (:or (:and "VIEW-AS" (:rule rule3581?)) "TOGGLE-BOX"))

;; combo-box-phrase
(meta-sexp:defrule rule3581? () ()
)


;; Form no. 2
(meta-sexp:defrule rule3594? () ()
  (:and "VIEW-AS"
   (:or (:rule rule3583?) (:rule rule3584?)
    (:and "FILL-IN" (:? "NATIVE") (:? (:rule rule3585?))
     (:? (:and "TOOLTIP" (:rule rule3586?))))
    (:rule rule3587?) (:rule rule3588?) (:rule rule3589?)
    (:and "TEXT" (:? (:rule rule3590?))
     (:? (:and "TOOLTIP" (:rule rule3591?))))
    (:and "TOGGLE-BOX" (:? (:rule rule3592?))
     (:? (:and "TOOLTIP" (:rule rule3593?)))))))

;; combo-box-phrase
(meta-sexp:defrule rule3583? () ()
)

;; editor-phrase
(meta-sexp:defrule rule3584? () ()
)

;; size-phrase
(meta-sexp:defrule rule3585? () ()
)

;; tooltip
(meta-sexp:defrule rule3586? () ()
)

;; radio-set-phrase
(meta-sexp:defrule rule3587? () ()
)

;; selection-list-phrase
(meta-sexp:defrule rule3588? () ()
)

;; slider-phrase
(meta-sexp:defrule rule3589? () ()
)

;; size-phrase
(meta-sexp:defrule rule3590? () ()
)

;; tooltip
(meta-sexp:defrule rule3591? () ()
)

;; size-phrase
(meta-sexp:defrule rule3592? () ()
)

;; tooltip
(meta-sexp:defrule rule3593? () ()
)

;;; Widget phrase
;; Form no. 1
(meta-sexp:defrule widget-phrase? (&aux item parent) ()
  ;; HANDLE and SYSTEM-HANDLE forms are taken care of by FIELD-WIDGET
  (:or (:rule frame-widget?)
       (:rule column-widget?)
       (:rule menu-widget?)
       (:rule menu-item-widget?)
       (:rule field-widget?)
       (:rule expression?)))


;; frame
(meta-sexp:defrule frame-widget? (&aux item) ()
  (:delimited (:rule whitespace?)
              (:icase "FRAME")
              (:assign item (:rule expression?)))
  (:return (make-widget :type :frame
                        :widget item)))

;; field
(meta-sexp:defrule field-widget? (&aux item parent) ()
  ;; Need either leading FIELD, "IN FRAME" tail, or both
  (:or (:checkpoint
        (:? (:icase "FIELD")
            (:rule whitespace?))
        (:delimited (:rule whitespace?)
                    (:assign item (:rule expression?))
                    (:icase "IN") (:icase "FRAME")
                    (:assign parent (:rule expression?))))
       (:delimited (:rule whitespace?)
                   (:icase "FIELD")
                   (:assign item (:rule expression?))))
  (:return (make-widget :type :field-level
                        :widget item
                        :parent parent)))

;; column
(meta-sexp:defrule column-widget? (&aux item parent) ()
  (:delimited (:? (:rule whitespace?))
              (:assign item (:rule expression?))
              (:delimited (:rule whitespace?)
                          (:icase "IN") (:icase "BROWSE")
                          (:assign parent (:rule expression?))))
  (:return (make-widget :type :browse-column
                        :widget item
                        :parent parent)))

;; menu
(meta-sexp:defrule menu-widget? (&aux item) ()
  (:delimited (:rule whitespace?)
              (:or (:icase "MENU") (:icase "SUB-MENU"))
              (:assign item (:rule expression?)))
  (:return (make-widget :type :menu
                        :widget item)))

;; menu-item
(meta-sexp:defrule menu-item-widget? (&aux item parent) ()
  (:delimited (:? (:rule whitespace?))
              (:icase "MENU-ITEM")
              (:assign item (:rule expression?))
              (:? (:checkpoint (:delimited (:rule whitespace?)
                                           (:icase "IN") (:icase "MENU")
                                           (:assign parent (:rule expression?))))))
  (:return (make-widget :type :menu-item
                        :widget item
                        :parent parent)))
