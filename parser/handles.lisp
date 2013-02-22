
;;; ACTIVE-WINDOW system handle
;; Form no. 1
(meta-sexp:defrule rule3178? () ()
  (:and "ACTIVE-WINDOW" (:? (:and ":" (:rule rule3177?)))))

;; attribute
(meta-sexp:defrule rule3177? () ()
)

;;; Asynchronous request object handle
;; Form no. 1
(meta-sexp:defrule rule3181? () ()
  (:and (:rule rule3179?) (:? (:and ":" (:rule rule3180?)))))

;; async-request-handle
(meta-sexp:defrule rule3179? () ()
)

;; attribute
(meta-sexp:defrule rule3180? () ()
)

;;; AUDIT-CONTROL system handle
;; Form no. 1
(meta-sexp:defrule rule3184? () ()
  (:and "AUDIT-CONTROL"
   (:? (:or (:and ":" (:rule rule3182?)) (:and ":" (:rule rule3183?))))))

;; attribute
(meta-sexp:defrule rule3182? () ()
)

;; method
(meta-sexp:defrule rule3183? () ()
)

;;; AUDIT-POLICY system handle
;; Form no. 1
(meta-sexp:defrule rule3186? () ()
  (:and "AUDIT-POLICY" (:? (:and ":" (:rule rule3185?)))))

;; method
(meta-sexp:defrule rule3185? () ()
)

;;; Buffer object handle
;; Form no. 1
(meta-sexp:defrule rule3190? () ()
  (:and (:rule rule3187?)
   (:? (:or (:and ":" (:rule rule3188?)) (:and ":" (:rule rule3189?))))))

;; buffer-handle
(meta-sexp:defrule rule3187? () ()
)

;; attribute
(meta-sexp:defrule rule3188? () ()
)

;; method
(meta-sexp:defrule rule3189? () ()
)

;;; Buffer-field object handle
;; Form no. 1
(meta-sexp:defrule rule3193? () ()
  (:and (:rule rule3191?) (:? (:and ":" (:rule rule3192?)))))

;; buffer-field-handle
(meta-sexp:defrule rule3191? () ()
)

;; attribute
(meta-sexp:defrule rule3192? () ()
)

;;; CALL object handle
;; Form no. 1
(meta-sexp:defrule rule3197? () ()
  (:and (:rule rule3194?)
   (:? (:or (:and ":" (:rule rule3195?)) (:and ":" (:rule rule3196?))))))

;; call-object-handle
(meta-sexp:defrule rule3194? () ()
)

;; attribute
(meta-sexp:defrule rule3195? () ()
)

;; method
(meta-sexp:defrule rule3196? () ()
)

;;; Client-principal object handle
;; Form no. 1
(meta-sexp:defrule rule3201? () ()
  (:and (:rule rule3198?)
   (:? (:or (:and ":" (:rule rule3199?)) (:and ":" (:rule rule3200?))))))

;; client-principal-handle
(meta-sexp:defrule rule3198? () ()
)

;; attribute
(meta-sexp:defrule rule3199? () ()
)

;; method
(meta-sexp:defrule rule3200? () ()
)

;;; CLIPBOARD system handle
;; Form no. 1
(meta-sexp:defrule rule3203? () ()
  (:and "CLIPBOARD" (:? (:and ":" (:rule rule3202?)))))

;; attribute
(meta-sexp:defrule rule3202? () ()
)

;;; CODEBASE-LOCATOR system handle
;; Form no. 1
(meta-sexp:defrule rule3205? () ()
  (:and "CODEBASE-LOCATOR" (:? (:and ":" (:rule rule3204?)))))

;; attribute
(meta-sexp:defrule rule3204? () ()
)

;;; COLOR-TABLE system handle
;; Form no. 1
(meta-sexp:defrule rule3208? () ()
  (:and "COLOR-TABLE"
   (:? (:or (:and ":" (:rule rule3206?)) (:and ":" (:rule rule3207?))))))

;; attribute
(meta-sexp:defrule rule3206? () ()
)

;; method
(meta-sexp:defrule rule3207? () ()
)

;;; COM-SELF system handle
;; Form no. 1
(meta-sexp:defrule rule3211? () ()
  (:and "COM-SELF"
   (:? (:or (:and ":" (:rule rule3209?)) (:and ":" (:rule rule3210?))))))

;; OCX-property-reference
(meta-sexp:defrule rule3209? () ()
)

;; OCX-method-reference
(meta-sexp:defrule rule3210? () ()
)

;;; COMPILER system handle
;; Form no. 1
(meta-sexp:defrule rule3213? () ()
  (:and "COMPILER" (:? (:and ":" (:rule rule3212?)))))

;; attribute
(meta-sexp:defrule rule3212? () ()
)

;;; CURRENT-WINDOW system handle
;; Form no. 1
(meta-sexp:defrule rule3215? () ()
  (:and "CURRENT-WINDOW" (:? (:and ":" (:rule rule3214?)))))

;; attribute
(meta-sexp:defrule rule3214? () ()
)

;;; Data-relation object handle
;; Form no. 1
(meta-sexp:defrule rule3218? () ()
  (:and (:rule rule3216?) (:? (:and ":" (:rule rule3217?)))))

;; data-relation-handle
(meta-sexp:defrule rule3216? () ()
)

;; attribute
(meta-sexp:defrule rule3217? () ()
)

;;; Data-source object handle
;; Form no. 1
(meta-sexp:defrule rule3222? () ()
  (:and (:rule rule3219?)
   (:? (:or (:and ":" (:rule rule3220?)) (:and ":" (:rule rule3221?))))))

;; data-source-handle
(meta-sexp:defrule rule3219? () ()
)

;; attribute
(meta-sexp:defrule rule3220? () ()
)

;; method
(meta-sexp:defrule rule3221? () ()
)

;;; DEBUGGER system handle
;; Form no. 1
(meta-sexp:defrule rule3225? () ()
  (:and "DEBUGGER"
   (:? (:or (:and ":" (:rule rule3223?)) (:and ":" (:rule rule3224?))))))

;; attribute
(meta-sexp:defrule rule3223? () ()
)

;; method
(meta-sexp:defrule rule3224? () ()
)

;;; DEFAULT-WINDOW system handle
;; Form no. 1
(meta-sexp:defrule rule3227? () ()
  (:and "DEFAULT-WINDOW" (:? (:and ":" (:rule rule3226?)))))

;; attribute
(meta-sexp:defrule rule3226? () ()
)

;;; ERROR-STATUS system handle
;; Form no. 1
(meta-sexp:defrule rule3230? () ()
  (:and "ERROR-STATUS"
   (:? (:or (:and ":" (:rule rule3228?)) (:and ":" (:rule rule3229?))))))

;; attribute
(meta-sexp:defrule rule3228? () ()
)

;; method
(meta-sexp:defrule rule3229? () ()
)

;;; FILE-INFO system handle
;; Form no. 1
(meta-sexp:defrule rule3232? () ()
  (:and "FILE-INFO" (:? (:and ":" (:rule rule3231?)))))

;; attribute
(meta-sexp:defrule rule3231? () ()
)

;;; FOCUS system handle
;; Form no. 1
(meta-sexp:defrule rule3234? () ()
  (:and "FOCUS" (:? (:and ":" (:rule rule3233?)))))

;; attribute
(meta-sexp:defrule rule3233? () ()
)

;;; FONT-TABLE system handle
;; Form no. 1
(meta-sexp:defrule rule3237? () ()
  (:and "FONT-TABLE"
   (:? (:or (:and ":" (:rule rule3235?)) (:and ":" (:rule rule3236?))))))

;; attribute
(meta-sexp:defrule rule3235? () ()
)

;; method
(meta-sexp:defrule rule3236? () ()
)

;;; LAST-EVENT system handle
;; Form no. 1
(meta-sexp:defrule rule3239? () ()
  (:and "LAST-EVENT" (:? (:and ":" (:rule rule3238?)))))

;; attribute
(meta-sexp:defrule rule3238? () ()
)

;;; LOG-MANAGER system handle
;; Form no. 1
(meta-sexp:defrule rule3242? () ()
  (:and "LOG-MANAGER"
   (:? (:or (:and ":" (:rule rule3240?)) (:and ":" (:rule rule3241?))))))

;; attribute
(meta-sexp:defrule rule3240? () ()
)

;; method
(meta-sexp:defrule rule3241? () ()
)

;;; Procedure object handle
;; Form no. 1
(meta-sexp:defrule rule3246? () ()
  (:and (:rule rule3243?)
   (:? (:or (:and ":" (:rule rule3244?)) (:and ":" (:rule rule3245?))))))

;; procedure-handle
(meta-sexp:defrule rule3243? () ()
)

;; attribute
(meta-sexp:defrule rule3244? () ()
)

;; method
(meta-sexp:defrule rule3245? () ()
)

;;; ProDataSet object handle
;; Form no. 1
(meta-sexp:defrule rule3250? () ()
  (:and (:rule rule3247?)
   (:? (:or (:and ":" (:rule rule3248?)) (:and ":" (:rule rule3249?))))))

;; dataset-object-handle
(meta-sexp:defrule rule3247? () ()
)

;; attribute
(meta-sexp:defrule rule3248? () ()
)

;; method
(meta-sexp:defrule rule3249? () ()
)

;;; Query object handle
;; Form no. 1
(meta-sexp:defrule rule3254? () ()
  (:and (:rule rule3251?)
   (:? (:or (:and ":" (:rule rule3252?)) (:and ":" (:rule rule3253?))))))

;; query-handle
(meta-sexp:defrule rule3251? () ()
)

;; attribute
(meta-sexp:defrule rule3252? () ()
)

;; method
(meta-sexp:defrule rule3253? () ()
)

;;; SAX-attributes object handle
;; Form no. 1
(meta-sexp:defrule rule3258? () ()
  (:and (:rule rule3255?)
   (:? (:or (:and ":" (:rule rule3256?)) (:and ":" (:rule rule3257?))))))

;; SAX-attributes-handle
(meta-sexp:defrule rule3255? () ()
)

;; attribute
(meta-sexp:defrule rule3256? () ()
)

;; method
(meta-sexp:defrule rule3257? () ()
)

;;; SAX-reader object handle
;; Form no. 1
(meta-sexp:defrule rule3262? () ()
  (:and (:rule rule3259?)
   (:? (:or (:and ":" (:rule rule3260?)) (:and ":" (:rule rule3261?))))))

;; sax-reader-handle
(meta-sexp:defrule rule3259? () ()
)

;; attribute
(meta-sexp:defrule rule3260? () ()
)

;; method
(meta-sexp:defrule rule3261? () ()
)

;;; SAX-writer object handle
;; Form no. 1
(meta-sexp:defrule rule3266? () ()
  (:and (:rule rule3263?)
   (:? (:or (:and ":" (:rule rule3264?)) (:and ":" (:rule rule3265?))))))

;; handle
(meta-sexp:defrule rule3263? () ()
)

;; attribute
(meta-sexp:defrule rule3264? () ()
)

;; method
(meta-sexp:defrule rule3265? () ()
)

;;; SECURITY-POLICY system handle
;; Form no. 1
(meta-sexp:defrule rule3269? () ()
  (:and "SECURITY-POLICY"
   (:? (:or (:and ":" (:rule rule3267?)) (:and ":" (:rule rule3268?))))))

;; attribute
(meta-sexp:defrule rule3267? () ()
)

;; method
(meta-sexp:defrule rule3268? () ()
)

;;; SELF system handle
;; Form no. 1
(meta-sexp:defrule rule3271? () ()
  (:and "SELF" (:? (:and ":" (:rule rule3270?)))))

;; attribute
(meta-sexp:defrule rule3270? () ()
)

;;; Server object handle
;; Form no. 1
(meta-sexp:defrule rule3275? () ()
  (:and (:rule rule3272?)
   (:? (:or (:and ":" (:rule rule3273?)) (:and ":" (:rule rule3274?))))))

;; server-handle
(meta-sexp:defrule rule3272? () ()
)

;; attribute
(meta-sexp:defrule rule3273? () ()
)

;; method
(meta-sexp:defrule rule3274? () ()
)

;;; Server socket object handle
;; Form no. 1
(meta-sexp:defrule rule3279? () ()
  (:and (:rule rule3276?)
   (:? (:or (:and ":" (:rule rule3277?)) (:and ":" (:rule rule3278?))))))

;; server-socket-handle
(meta-sexp:defrule rule3276? () ()
)

;; attribute
(meta-sexp:defrule rule3277? () ()
)

;; method
(meta-sexp:defrule rule3278? () ()
)

;;; SESSION system handle
;; Form no. 1
(meta-sexp:defrule rule3282? () ()
  (:and "SESSION"
   (:? (:or (:and ":" (:rule rule3280?)) (:and ":" (:rule rule3281?))))))

;; attribute
(meta-sexp:defrule rule3280? () ()
)

;; method
(meta-sexp:defrule rule3281? () ()
)

;;; SOAP-fault object handle
;; Form no. 1
(meta-sexp:defrule rule3285? () ()
  (:and (:rule rule3283?) (:? (:and ":" (:rule rule3284?)))))

;; soap-fault-handle
(meta-sexp:defrule rule3283? () ()
)

;; attribute
(meta-sexp:defrule rule3284? () ()
)

;;; SOAP-fault-detail object handle
;; Form no. 1
(meta-sexp:defrule rule3289? () ()
  (:and (:rule rule3286?)
   (:? (:or (:and ":" (:rule rule3287?)) (:and ":" (:rule rule3288?))))))

;; soap-fault-detail-handle
(meta-sexp:defrule rule3286? () ()
)

;; attribute
(meta-sexp:defrule rule3287? () ()
)

;; method
(meta-sexp:defrule rule3288? () ()
)

;;; SOAP-header object handle
;; Form no. 1
(meta-sexp:defrule rule3293? () ()
  (:and (:rule rule3290?)
   (:? (:or (:and ":" (:rule rule3291?)) (:and ":" (:rule rule3292?))))))

;; soap-header-handle
(meta-sexp:defrule rule3290? () ()
)

;; attribute
(meta-sexp:defrule rule3291? () ()
)

;; method
(meta-sexp:defrule rule3292? () ()
)

;;; SOAP-header-entryref object handle
;; Form no. 1
(meta-sexp:defrule rule3297? () ()
  (:and (:rule rule3294?)
   (:? (:or (:and ":" (:rule rule3295?)) (:and ":" (:rule rule3296?))))))

;; soap-header-entryref-handle
(meta-sexp:defrule rule3294? () ()
)

;; attribute
(meta-sexp:defrule rule3295? () ()
)

;; method
(meta-sexp:defrule rule3296? () ()
)

;;; Socket object handle
;; Form no. 1
(meta-sexp:defrule rule3301? () ()
  (:and (:rule rule3298?)
   (:? (:or (:and ":" (:rule rule3299?)) (:and ":" (:rule rule3300?))))))

;; socket-handle
(meta-sexp:defrule rule3298? () ()
)

;; attribute
(meta-sexp:defrule rule3299? () ()
)

;; method
(meta-sexp:defrule rule3300? () ()
)

;;; SOURCE-PROCEDURE system handle
;; Form no. 1
(meta-sexp:defrule rule3304? () ()
  (:and "SOURCE-PROCEDURE"
   (:? (:or (:and ":" (:rule rule3302?)) (:and ":" (:rule rule3303?))))))

;; attribute
(meta-sexp:defrule rule3302? () ()
)

;; method
(meta-sexp:defrule rule3303? () ()
)

;;; TARGET-PROCEDURE system handle
;; Form no. 1
(meta-sexp:defrule rule3307? () ()
  (:and "TARGET-PROCEDURE"
   (:? (:or (:and ":" (:rule rule3305?)) (:and ":" (:rule rule3306?))))))

;; attribute
(meta-sexp:defrule rule3305? () ()
)

;; method
(meta-sexp:defrule rule3306? () ()
)

;;; Temp-table object handle
;; Form no. 1
(meta-sexp:defrule rule3311? () ()
  (:and (:rule rule3308?)
   (:? (:or (:and ":" (:rule rule3309?)) (:and ":" (:rule rule3310?))))))

;; temp-table-handle
(meta-sexp:defrule rule3308? () ()
)

;; attribute
(meta-sexp:defrule rule3309? () ()
)

;; method
(meta-sexp:defrule rule3310? () ()
)

;;; THIS-PROCEDURE system handle
;; Form no. 1
(meta-sexp:defrule rule3314? () ()
  (:and "THIS-PROCEDURE"
   (:? (:or (:and ":" (:rule rule3312?)) (:and ":" (:rule rule3313?))))))

;; attribute
(meta-sexp:defrule rule3312? () ()
)

;; method
(meta-sexp:defrule rule3313? () ()
)

;;; Transaction object handle
;; Form no. 1
(meta-sexp:defrule rule3318? () ()
  (:and (:rule rule3315?)
   (:? (:or (:and ":" (:rule rule3316?)) (:and ":" (:rule rule3317?))))))

;; transaction-handle
(meta-sexp:defrule rule3315? () ()
)

;; attribute
(meta-sexp:defrule rule3316? () ()
)

;; method
(meta-sexp:defrule rule3317? () ()
)

;;; WEB-CONTEXT system handle
;; Form no. 1
(meta-sexp:defrule rule3321? () ()
  (:and "WEB-CONTEXT"
   (:? (:or (:and ":" (:rule rule3319?)) (:and ":" (:rule rule3320?))))))

;; attribute
(meta-sexp:defrule rule3319? () ()
)

;; method
(meta-sexp:defrule rule3320? () ()
)

;;; X-document object handle
;; Form no. 1
(meta-sexp:defrule rule3325? () ()
  (:and (:rule rule3322?)
   (:? (:or (:and ":" (:rule rule3323?)) (:and ":" (:rule rule3324?))))))

;; x-document-handle
(meta-sexp:defrule rule3322? () ()
)

;; attribute
(meta-sexp:defrule rule3323? () ()
)

;; method
(meta-sexp:defrule rule3324? () ()
)

;;; X-noderef object handle
;; Form no. 1
(meta-sexp:defrule rule3329? () ()
  (:and (:rule rule3326?)
   (:? (:or (:and ":" (:rule rule3327?)) (:and ":" (:rule rule3328?))))))

;; x-noderef-handle
(meta-sexp:defrule rule3326? () ()
)

;; attribute
(meta-sexp:defrule rule3327? () ()
)

;; method
(meta-sexp:defrule rule3328? () ()
)
