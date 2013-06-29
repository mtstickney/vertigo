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
  (:or (:rule at-2d?)
       (:rule at-linear?)))

;; Form no. 1
(meta-sexp:defrule at-linear? () ()
  (:k "AT")
  (:rule expression?))

;; Form no. 2
(meta-sexp:defrule at-2d? (&aux opt (opts (dict))) ()
  (:k "AT")
  (:or (:checkpoint (:k "X")
                    (:assign opt (:rule expression?))
                    (setf (gethash :x opts) opt
                          (gethash :x-type opts) :x))
       (:checkpoint (:k "X-OF")
                    (:assign opt (:rule expression?))
                    (setf (gethash :x opts) opt
                          (gethash :x-type opts) :x-of))
       (:checkpoint (:k "COLUMN")
                    (:rule whitespace?)
                    (:assign opt (:rule expression?))
                    (setf (gethash :x opts) opt
                          (gethash :x-type opts) :column)))
  (:or (:checkpoint (:k "Y")
                    (:assign opt (:rule expression?))
                    (setf (gethash :y opts) opt
                          (gethash :y-type opts) :y))
       (:checkpoint (:k "Y-OF")
                    (:assign opt (:rule expression?))
                    (setf (gethash :y opts) opt
                          (gethash :y-type opts) :y-of))
       (:checkpoint (:k "ROW")
                    (:assign opt (:rule expression?))
                    (setf (gethash :y opts) opt
                          (gethash :y-type opts) :row)))
  (:? (:or (:and (:k "COLON-ALIGNED")
                 (setf (gethash :align opts) :colon))
           (:and (:k "LEFT-ALIGNED")
                 (setf (gethash :align opts) :left))
           (:and (:k "RIGHT-ALIGNED")
                 (setf (gethash :align opts) :right))))
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
(meta-sexp:defrule color-phrase? (&aux word (opts (dict))) ()
  (:or (:and (:k "NORMAL")
             (:return :normal))
       (:and (:k "INPUT")
             (:return :input))
       (:and (:k "MESSAGES")
             (:return :messages))
       (:rule literal?)
       (:and (:k "VALUE")
             (:rule token :lparen)
             (:assign word (:rule expression?))
             (:rule token :rparen)
             (:return word))
       (:checkpoint (:? (:icase "BLINK-")
                           (setf (gethash :blink opts) t))
                    (:? (:icase "BRIGHT-")
                        (setf (gethash :bright opts) t))
                    (:and (:assign word (:rule exclude-chars? '(meta-sexp:white-space? #\-)))
                          (setf (gethash :foreground opts) word))
                    (:and #\-
                          (:assign word (:rule exclude-chars? '(meta-sexp:white-space? #\-)))
                          (setf (gethash :background opts) word))
                    ;; Need to have set *something*
                    (not (equalp opts (dict)))
                    (:return opts))
       (:checkpoint (:? (:icase "BLINK-")
                        (:or (setf (gethash :blink opts) t) t))
                    (:? (:icase "RVV-")
                        (:or (setf (gethash :reverse-video opts) t) t))
                    (:? (:icase "UNDERLINE-")
                        (:or (setf (gethash :underline opts) t) t))
                    (:? (:icase "BRIGHT-")
                        (:or (setf (gethash :bright opts) t) t))
                    (:? (:assign word (:rule exclude-chars? '(meta-sexp:white-space?)))
                        (:or (setf (gethash :foreground opts) word) t))
                    ;; Need to have set *something*
                    (not (equalp opts (dict)))
                    (:return opts))
       ;; Termcap color identifier
       (:rule exclude-chars? '(meta-sexp:white-space?))))

;; Note: members of excluded-chars may be characters or type designators
(meta-sexp:defrule exclude-chars? (excluded &aux match c) ()
  (:with-stored-match (match)
    (:+ (:checkpoint (:assign c (:type character))
                     (not (some (lambda (thing)
                                  (typecase thing
                                    (character (eql thing c))
                                    (t (typep c thing))))
                                excluded)))))
  (:return match))

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
(meta-sexp:defrule rule3487? (&aux opt (opts (dict))) ()
  (:icase "WITH")
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "ACCUM")
                   (:rule whitespace?)
                   (:? (:assign opt (:rule expression?))
                       (:or (setf (gethash :accum opts) opt) t))))
  (:? (:checkpoint (:rule whitespace?)
                   (:assign opt (:rule at-phrase?))
                   (:or (setf (gethash :at opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "ATTR-SPACE")
                              (:or (setf (gethash :attr-space opts) t) t))
                        (:and (:icase "NO-ATTR-SPACE")
                              (:or (setf (gethash :attr-space opts) nil) t)))))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "CANCEL-BUTTON")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :cancel-button opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "CENTERED")
                   (:rule whitespace?)
                   (:or (setf (gethash :centered opts) t) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:assign opt (:rule frame-color-spec?))
                   (:or (setf (gethash :color opts) opt) t)))
  ;; NOTE: the expression used here gets re-evaluated whenever the
  ;; frame comes into view, should be a closure
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "COLUMN")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :column opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:rule whitespace?)
                   (:icase "COLUMNS")
                   (:or (setf (gethash :columns opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "CONTEXT-HELP")
                   (:or (setf (gethash :context-help opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "CONTEXT-HELP-FILE")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :context-help-file opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "DEFAULT-BUTTON")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :default-button opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "DROP-TARGET")
                   (:or (setf (gethash :drop-target opts) t) t)))
  (:? (:checkpoint (:? (:rule whitespace?)
                       (:assign opt (:rule expression?))
                       (:or (setf (gethash :scroll-by opts) opt)))
                   (:rule whitespace?)
                   (:icase "DOWN")
                   (:or (setf (gethash :scrollable opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "EXPORT")
                   (:or (setf (gethash :export opts) t) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "WIDGET-ID")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :widget-id opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "FONT")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :font opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "FRAME")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :frame-id opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "INHERIT-BGCOLOR")
                              (:or (setf (gethash :inherit-bgcolor opts) t) t))
                        (:and (:icase "NO-INHERIT-BGCOLOR")
                              (:or (setf (gethash :inherit-bgcolor opts) nil) t)))))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "INHERIT-FGCOLOR")
                              (:or (setf (gethash :inherit-fgcolor opts) t) t))
                        (:and (:icase "NO-INHERIT-FGCOLOR")
                              (:or (setf (gethash :inherit-fgcolor opts) nil) t)))))
  (:? (setf (gethash :keep-tab-order opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "KEEP-TAB-ORDER")))))
  (:? (setf (gethash :box opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-BOX"))))))
  (:? (setf (gethash :hide opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-HIDE"))))))
  (:? (setf (gethash :labels opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-LABELS"))))))
  (:? (let ((val (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "USE-DICT-EXPS")))))
        (setf (gethash :use-dict-help-strings opts) val
              (gethash :use-dict-validation-string opts) val)))
  (:? (setf (gethash :validate opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-VALIDATE"))))))
  (:? (setf (gethash :auto-validate opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-AUTO-VALIDATE"))))))
  (:? (setf (gethash :help opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-HELP"))))))
  (:? (setf (gethash :underline opts)
            (not (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                              (:icase "NO-UNDERLINE"))))))
  (:? (setf (gethash :overlay opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "OVERLAY")))))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "PAGE-BOTTOM")
                              (:or (setf (gethash :paging opts) :bottom) t))
                        (:and (:icase "PAGE-TOP")
                              (:or (setf (gethash :paging opts) :top) t)))))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "RETAIN")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :retain opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "ROW")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :row opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:or (:and (:icase "SCREEN-IO")
                              (:or (setf (gethash :io-type opts) :screen) t))
                        (:and (:icase "STREAM-IO")
                              (:or (setf (gethash :io-type opts) :stream) t)))))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "SCROLL")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :scroll-by opts) opt) t)))
  (:? (setf (gethash :scrollable opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "SCROLLABLE")))))
  (:? (setf (gethash :side-labels opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "SIDE-LABELS")))))
  (:? (:checkpoint (:rule whitespace?)
                   (:assign opt (:rule size-phrase?))
                   (:or (setf (gethash :size opts) opt) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "STREAM")
                   (:assign opt (:rule expression?))
                   (:or (setf (gethash :stream opts) opt))))
  (:? (setf (gethash :lickable opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "THREE-D")))))
  (:? (:checkpoint (:rule whitespace?)
                   (:assign opt (:rule title-phrase?))
                   (:? (setf (gethash :title-opts opts) opt))))
  (:? (setf (gethash :top-only opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "TOP-ONLY")))))
  (:? (setf (gethash :use-text opts)
            (meta-sexp:meta (:checkpoint (:rule whitespace?)
                                         (:icase "USE-TEXT")))))
  (:? (:checkpoint (:rule whitespace?)
                   (setf (gethash :v6-frame opts)
                         (meta-sexp:meta (:icase "V6FRAME")))
                   (:? (:checkpoint (:rule whitespace?)
                                    (:or (:checkpoint (:icase "USE-REVVIDEO")
                                                      (setf (gethash :use-reverse-video opts) t))
                                         (:checkpoint (:icase "USE-UNDERLINE")
                                                      (setf (gethash :use-underline opts) t)))))))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "VIEW-AS")
                   (:rule whitespace?)
                   (:icase "DIALOG-BOX")
                   (setf (gethash :dialog-box opts) t)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "WIDTH")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (setf (gethash :width opts) opt)))
  (:? (:checkpoint (:rule whitespace?)
                   (:icase "IN")
                   (:rule whitespace?)
                   (:icase "WINDOW")
                   (:rule whitespace?)
                   (:assign opt (:rule expression?))
                   (setf (gethash :parent-window opts) opt))))

;; color-specification
(meta-sexp:defrule frame-color-spec? (&aux opt (opts (dict))) ()
  (:or (:and (:? (:checkpoint (:k "BGCOLOR")
                              (:assign opt (:rule expression?))
                              (setf (gethash :bgcolor opts) opt)))
             (:? (:checkpoint (:k "DCOLOR")
                              (:assign opt (:rule expression?))
                              (setf (gethash :dcolor opts) opt)))
             (:? (:checkpoint (:k "FGCOLOR")
                              (:assign opt (:rule expression?))
                              (setf (gethash :fgcolor opts) opt)))
             (:? (:checkpoint (:k "PFCOLOR")
                              (:assign opt (:rule expression?))
                              (setf (gethash :pfcolor opts) opt)))
             (not (equalp opts (dict))))
       (:checkpoint (:k "COLOR")
                    (:? (:k "DISPLAY"))
                    (:or (format *debug-io* "color display~%") t)
                    (:assign opt (:rule color-phrase?))
                    (:or (format *debug-io* "color phrase ~%") t)
                    (setf (gethash :dcolor opts) opt)
                    (:? (:checkpoint (:k "PROMPT")
                                     (:assign opt (:rule color-phrase?))
                                     (setf (gethash :pfcolor opts) opt)))))
  (:return opts))

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
(meta-sexp:defrule title-phrase? (&aux opt (opts (dict))) ()
  (:k "TITLE")
  (:? (:or (:and (:? (:checkpoint (:k "BGCOLOR")
                                  (:assign opt (:rule expression?))
                                  (setf (gethash :bg-color opts) opt)))
                 (:? (:checkpoint (:k "DCOLOR")
                                  (:assign opt (:rule expression?))
                                  (setf (gethash :title-color opts) opt)))
                 (:? (:checkpoint (:k "FGCOLOR")
                                  (:assign opt (:rule expression?))
                                  (setf (gethash :fg-color opts) opt)))
                 (not (equalp opts (dict))))
           (:checkpoint (:k "COLOR")
                        (:assign opt (:rule color-phrase?))
                        (setf (gethash :color opts) opt))))
  (:? (:checkpoint (:k "FONT")
                   (:assign opt (:rule expression?))
                   (setf (gethash :font opts) opt)))
  (:assign opt (:rule expression?))
  (setf (gethash :title opts) opt)
  (:return opts))

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
(meta-sexp:defrule size-phrase? (&aux x y (opts (dict))) ()
  (:or (:and (:k "SIZE-PIXELS")
             (setf (gethash :size-type opts) :pixel))
       (:and (:or (:k "SIZE-CHARS")
                  (:k "SIZE"))
             (setf (gethash :size-type opts) :character)))
  (:assign x (:rule expression?))
  (:k "BY")
  (:assign y (:rule expression?))
  (setf (gethash :x opts) x
        (gethash :y opts) y)
  (:return opts))

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
(meta-sexp:defrule frame-widget? () ()
  (:with-binds
      (:k "FRAME")
    (:bind (:rule expression?) widget)
    (:return (make-widget :type :frame
                          :widget widget))))

;; field
(meta-sexp:defrule field-widget? (&aux item parent) ()
  ;; Need either leading FIELD, "IN FRAME" tail, or both
  (:or (:checkpoint
        (:? (:k "FIELD"))
        (:assign item (:rule expression?))
        (:k "IN")
        (:k "FRAME")
        (:assign parent (:rule expression?)))
       (:checkpoint
        (:k "FIELD")
         (:assign item (:rule expression?))))
  (:return (make-widget :type :field-level
                        :widget item
                        :parent parent)))

;; column
(meta-sexp:defrule column-widget? () ()
  (:with-binds
      (:bind (:rule expression?) item)
    (:k "IN" "BROWSE")
    (:bind (:rule expression?) parent)
    (:return (make-widget :type :browse-column
                          :widget item
                          :parent parent))))

;; menu
(meta-sexp:defrule menu-widget? () ()
  (:with-binds
      (:or (:k "MENU")
           (:k "SUB-MENU"))
    (:bind (:rule expression?) item)
    (:return (make-widget :type :menu
                          :widget item))))

;; menu-item
(meta-sexp:defrule menu-item-widget? (&aux item parent) ()
  (:with-binds
      (:k "MENU-ITEM")
    (:bind (:rule expression?) item)
    (:? (:checkpoint (:k "IN" "MENU")
                     (:bind (:rule expression?) parent)))
    (:return (make-widget :type :menu-item
                          :widget item
                          :parent parent))))
