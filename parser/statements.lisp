
;;; WAIT-FOR statement
;; Form no. 1
(meta-sexp:defrule rule2171? () ()
  (:and "WAIT-FOR" "\"" "WEB-NOTIFY" "\"" "OF" "DEFAULT-WINDOW"
   (:? (:and "PAUSE" (:rule rule2170?))) (:? "EXCLUSIVE-WEB-USER")))

;; n
(meta-sexp:defrule rule2170? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2178? () ()
  (:and "WAIT-FOR" (:rule rule2172?) "OF" (:rule rule2173?)
   (:* (:? (:and "OR" (:rule rule2174?) "OF" (:rule rule2175?))))
   (:? (:rule whitespace?)) (:? (:and "FOCUS" (:rule rule2176?)))
   (:? (:and "PAUSE" (:rule rule2177?)))))

;; event-list
(meta-sexp:defrule rule2172? () ()
)

;; widget-list
(meta-sexp:defrule rule2173? () ()
)

;; event-list
(meta-sexp:defrule rule2174? () ()
)

;; widget-list
(meta-sexp:defrule rule2175? () ()
)

;; widget
(meta-sexp:defrule rule2176? () ()
)

;; n
(meta-sexp:defrule rule2177? () ()
)

;;; VIEW statement
;; Form no. 1
(meta-sexp:defrule rule2182? () ()
  (:and "VIEW" (:? (:and "STREAM" (:rule rule2179?))) (:? (:rule rule2180?))
   (:? (:and "IN" "WINDOW" (:rule rule2181?)))))

;; stream
(meta-sexp:defrule rule2179? () ()
)

;; widget-phrase
(meta-sexp:defrule rule2180? () ()
)

;; window
(meta-sexp:defrule rule2181? () ()
)

;;; VALIDATE statement
;; Form no. 1
(meta-sexp:defrule rule2184? () ()
  (:and "VALIDATE" (:rule rule2183?) (:? "NO-ERROR")))

;; record
(meta-sexp:defrule rule2183? () ()
)

;;; USING statement
;; Form no. 1
(meta-sexp:defrule rule2185? () ()
  "USING")

;;; USE statement
;; Form no. 1
(meta-sexp:defrule rule2187? () ()
  (:and "USE" (:rule rule2186?) (:? "NO-ERROR")))

;; environment
(meta-sexp:defrule rule2186? () ()
)

;;; UP statement
;; Form no. 1
(meta-sexp:defrule rule2191? () ()
  (:and "UP" (:? (:and "STREAM" (:rule rule2188?))) (:? (:rule rule2189?))
   (:? (:rule rule2190?))))

;; stream
(meta-sexp:defrule rule2188? () ()
)

;; expression
(meta-sexp:defrule rule2189? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2190? () ()
)

;;; UNSUBSCRIBE statement
;; Form no. 1
(meta-sexp:defrule rule2195? () ()
  (:and "UNSUBSCRIBE" (:? (:and "PROCEDURE" (:rule rule2192?))) (:? "TO")
   (:or (:rule rule2193?) "ALL") (:? (:and "IN" (:rule rule2194?)))))

;; subscriber-handle
(meta-sexp:defrule rule2192? () ()
)

;; event-name
(meta-sexp:defrule rule2193? () ()
)

;; publisher-handle
(meta-sexp:defrule rule2194? () ()
)

;;; UNLOAD statement
;; Form no. 1
(meta-sexp:defrule rule2197? () ()
  (:and "UNLOAD" (:rule rule2196?) (:? "NO-ERROR")))

;; environment
(meta-sexp:defrule rule2196? () ()
)

;;; UNIX statement
;; Form no. 1
(meta-sexp:defrule rule2200? () ()
  (:and "UNIX" (:? "SILENT")
   (:* (:? (:or (:rule rule2198?) (:and "VALUE" "(" (:rule rule2199?) ")"))))
   (:? (:rule whitespace?))))

;; command-token
(meta-sexp:defrule rule2198? () ()
)

;; expression
(meta-sexp:defrule rule2199? () ()
)

;;; UNDO statement
;; Form no. 1
(meta-sexp:defrule rule2206? () ()
  (:and "UNDO" (:? (:rule rule2201?))
   (:?
    (:or (:and "," "LEAVE" (:? (:rule rule2202?)))
     (:and "," "NEXT" (:? (:rule rule2203?)))
     (:and "," "RETRY" (:? (:rule rule2204?)))
     (:and "," "RETURN" (:? (:or "ERROR" "NO-APPLY"))
      (:? (:rule rule2205?)))))))

;; label
(meta-sexp:defrule rule2201? () ()
)

;; label2
(meta-sexp:defrule rule2202? () ()
)

;; label2
(meta-sexp:defrule rule2203? () ()
)

;; label1
(meta-sexp:defrule rule2204? () ()
)

;; return-value
(meta-sexp:defrule rule2205? () ()
)

;;; UNDERLINE statement
;; Form no. 1
(meta-sexp:defrule rule2210? () ()
  (:and "UNDERLINE" (:? (:and "STREAM" (:rule rule2207?)))
   (:* (:rule rule2208?)) (:? (:rule whitespace?)) (:? (:rule rule2209?))))

;; stream
(meta-sexp:defrule rule2207? () ()
)

;; field
(meta-sexp:defrule rule2208? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2209? () ()
)

;;; TRIGGER PROCEDURE statement
;; Form no. 1
(meta-sexp:defrule rule2214? () ()
  (:and "TRIGGER" "PROCEDURE" "FOR" (:rule rule2211?) "OF" (:rule rule2212?)
   (:? (:rule rule2213?))))

;; event
(meta-sexp:defrule rule2211? () ()
)

;; object
(meta-sexp:defrule rule2212? () ()
)

;; options
(meta-sexp:defrule rule2213? () ()
)

;;; TRANSACTION-MODE AUTOMATIC statement
;; Form no. 1
(meta-sexp:defrule rule2215? () ()
  (:and "TRANSACTION-MODE" "AUTOMATIC" (:? "CHAINED")))

;;; THIS-OBJECT statement
;; Form no. 1
(meta-sexp:defrule rule2218? () ()
  (:and "THIS-OBJECT" "("
   (:?
    (:and (:rule rule2216?) (:* (:? (:and "," (:rule rule2217?))))
     (:? (:rule whitespace?))))
   ")"))

;; parameter
(meta-sexp:defrule rule2216? () ()
)

;; parameter
(meta-sexp:defrule rule2217? () ()
)

;;; SYSTEM-HELP statement
;; Form no. 1
(meta-sexp:defrule rule2235? () ()
  (:and "SYSTEM-HELP" (:rule rule2219?)
   (:? (:and "WINDOW-NAME" (:rule rule2220?)))
   (:or "CONTENTS" (:and "CONTEXT" (:rule rule2221?))
    (:and "HELP-TOPIC" (:rule rule2222?)) (:and "KEY" (:rule rule2223?))
    (:and "ALTERNATE-KEY" (:rule rule2224?))
    (:and "POSITION" "X" (:rule rule2225?) "Y" (:rule rule2226?) "WIDTH"
     (:rule rule2227?) "HEIGHT" (:rule rule2228?))
    (:and "POSITION" "MAXIMIZE") "QUIT" (:and "SET-CONTENTS" (:rule rule2229?))
    (:and "CONTEXT-POPUP" (:rule rule2230?))
    (:and "PARTIAL-KEY" (:rule rule2231?))
    (:and "MULTIPLE-KEY" (:rule rule2232?) "TEXT" (:rule rule2233?))
    (:and "COMMAND" (:rule rule2234?)) "FINDER" "FORCE-FILE" "HELP")))

;; file-string
(meta-sexp:defrule rule2219? () ()
)

;; window-name
(meta-sexp:defrule rule2220? () ()
)

;; int-expr
(meta-sexp:defrule rule2221? () ()
)

;; string
(meta-sexp:defrule rule2222? () ()
)

;; string
(meta-sexp:defrule rule2223? () ()
)

;; string
(meta-sexp:defrule rule2224? () ()
)

;; x
(meta-sexp:defrule rule2225? () ()
)

;; y
(meta-sexp:defrule rule2226? () ()
)

;; dx
(meta-sexp:defrule rule2227? () ()
)

;; dy
(meta-sexp:defrule rule2228? () ()
)

;; int-expr
(meta-sexp:defrule rule2229? () ()
)

;; int-expr
(meta-sexp:defrule rule2230? () ()
)

;; string
(meta-sexp:defrule rule2231? () ()
)

;; char
(meta-sexp:defrule rule2232? () ()
)

;; string
(meta-sexp:defrule rule2233? () ()
)

;; string
(meta-sexp:defrule rule2234? () ()
)

;;; SYSTEM-DIALOG PRINTER-SETUP statement
;; Form no. 1
(meta-sexp:defrule rule2239? () ()
  (:and "SYSTEM-DIALOG" "PRINTER-SETUP"
   (:? (:and "NUM-COPIES" (:rule rule2236?))) (:? (:or "LANDSCAPE" "PORTRAIT"))
   (:? (:and "UPDATE" (:rule rule2237?)))
   (:? (:and "IN" "WINDOW" (:rule rule2238?)))))

;; expression
(meta-sexp:defrule rule2236? () ()
)

;; status
(meta-sexp:defrule rule2237? () ()
)

;; window
(meta-sexp:defrule rule2238? () ()
)

;;; SYSTEM-DIALOG GET-FILE statement
;; Form no. 1
(meta-sexp:defrule rule2251? () ()
  (:and "SYSTEM-DIALOG" "GET-FILE" (:rule rule2240?)
   (:?
    (:and "FILTERS" (:rule rule2241?) (:? (:rule whitespace?))
     (:rule rule2242?)
     (:*
      (:?
       (:and "," (:rule rule2243?) (:? (:rule whitespace?))
        (:rule rule2244?))))
     (:? (:rule whitespace?)) (:? (:and "INITIAL-FILTER" (:rule rule2245?)))))
   (:? "ASK-OVERWRITE") (:? "CREATE-TEST-FILE")
   (:? (:and "DEFAULT-EXTENSION" (:rule rule2246?)))
   (:? (:and "INITIAL-DIR" (:rule rule2247?))) (:? "MUST-EXIST")
   (:? "RETURN-TO-START-DIR") (:? "SAVE-AS")
   (:? (:and "TITLE" (:rule rule2248?))) (:? "USE-FILENAME")
   (:? (:and "UPDATE" (:rule rule2249?)))
   (:? (:and "IN" "WINDOW" (:rule rule2250?)))))

;; character-field
(meta-sexp:defrule rule2240? () ()
)

;; name
(meta-sexp:defrule rule2241? () ()
)

;; filespec
(meta-sexp:defrule rule2242? () ()
)

;; name
(meta-sexp:defrule rule2243? () ()
)

;; filespec
(meta-sexp:defrule rule2244? () ()
)

;; filter-num
(meta-sexp:defrule rule2245? () ()
)

;; extension-string
(meta-sexp:defrule rule2246? () ()
)

;; directory-string
(meta-sexp:defrule rule2247? () ()
)

;; title-string
(meta-sexp:defrule rule2248? () ()
)

;; logical-variable
(meta-sexp:defrule rule2249? () ()
)

;; window
(meta-sexp:defrule rule2250? () ()
)

;;; SYSTEM-DIALOG GET-DIR statement
;; Form no. 1
(meta-sexp:defrule rule2255? () ()
  (:and "SYSTEM-DIALOG" "GET-DIR" (:rule rule2252?)
   (:? (:and "INITIAL-DIR" (:rule rule2253?))) (:? "RETURN-TO-START-DIR")
   (:? (:and "TITLE" (:rule rule2254?)))))

;; character-field
(meta-sexp:defrule rule2252? () ()
)

;; directory-string
(meta-sexp:defrule rule2253? () ()
)

;; title-string
(meta-sexp:defrule rule2254? () ()
)

;;; SYSTEM-DIALOG FONT statement
;; Form no. 1
(meta-sexp:defrule rule2261? () ()
  (:and "SYSTEM-DIALOG" "FONT" (:rule rule2256?) (:? "ANSI-ONLY")
   (:? "FIXED-ONLY") (:? (:and "MAX-SIZE" (:rule rule2257?)))
   (:? (:and "MIN-SIZE" (:rule rule2258?)))
   (:? (:and "UPDATE" (:rule rule2259?)))
   (:? (:and "IN" "WINDOW" (:rule rule2260?)))))

;; font-number
(meta-sexp:defrule rule2256? () ()
)

;; point-size
(meta-sexp:defrule rule2257? () ()
)

;; point-size
(meta-sexp:defrule rule2258? () ()
)

;; logical-variable
(meta-sexp:defrule rule2259? () ()
)

;; window
(meta-sexp:defrule rule2260? () ()
)

;;; SYSTEM-DIALOG COLOR statement
;; Form no. 1
(meta-sexp:defrule rule2265? () ()
  (:and "SYSTEM-DIALOG" "COLOR" (:rule rule2262?)
   (:? (:and "UPDATE" (:rule rule2263?)))
   (:? (:and "IN" "WINDOW" (:rule rule2264?)))))

;; color-number
(meta-sexp:defrule rule2262? () ()
)

;; logical-variable
(meta-sexp:defrule rule2263? () ()
)

;; window
(meta-sexp:defrule rule2264? () ()
)

;;; SUBSCRIBE statement
;; Form no. 1
(meta-sexp:defrule rule2270? () ()
  (:and "SUBSCRIBE" (:? (:and "PROCEDURE" (:rule rule2266?))) (:? "TO")
   (:rule rule2267?) (:or (:and "IN" (:rule rule2268?)) "ANYWHERE")
   (:? (:and "RUN-PROCEDURE" (:rule rule2269?))) (:? "NO-ERROR")))

;; subscriber-handle
(meta-sexp:defrule rule2266? () ()
)

;; event-name
(meta-sexp:defrule rule2267? () ()
)

;; publisher-handle
(meta-sexp:defrule rule2268? () ()
)

;; local-internal-procedure
(meta-sexp:defrule rule2269? () ()
)

;;; STOP statement
;; Form no. 1
(meta-sexp:defrule rule2271? () ()
  "STOP")

;;; STATUS statement
;; Form no. 1
(meta-sexp:defrule rule2275? () ()
  (:and "STATUS"
   (:or (:and "DEFAULT" (:? (:rule rule2272?)))
    (:and "INPUT" (:? (:or "OFF" (:rule rule2273?)))))
   (:? (:and "IN" "WINDOW" (:rule rule2274?)))))

;; expression
(meta-sexp:defrule rule2272? () ()
)

;; expression
(meta-sexp:defrule rule2273? () ()
)

;; window
(meta-sexp:defrule rule2274? () ()
)

;;; SHOW-STATS statement
;; Form no. 1
(meta-sexp:defrule rule2276? () ()
  (:and "SHOW-STATS" (:? "CLEAR")))

;;; SET-SIZE statement
;; Form no. 1
(meta-sexp:defrule rule2279? () ()
  (:and "SET-SIZE" "(" (:rule rule2277?) ")" "=" (:rule rule2278?)))

;; memptr-var
(meta-sexp:defrule rule2277? () ()
)

;; size
(meta-sexp:defrule rule2278? () ()
)

;;; SET-POINTER-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule2282? () ()
  (:and "SET-POINTER-VALUE" "(" (:rule rule2280?) ")" "=" (:rule rule2281?)))

;; memptr-var
(meta-sexp:defrule rule2280? () ()
)

;; memptr-value
(meta-sexp:defrule rule2281? () ()
)

;;; SET-BYTE-ORDER statement
;; Form no. 1
(meta-sexp:defrule rule2285? () ()
  (:and "SET-BYTE-ORDER" "(" (:rule rule2283?) ")" "=" (:rule rule2284?)))

;; memptr
(meta-sexp:defrule rule2283? () ()
)

;; integer-expression
(meta-sexp:defrule rule2284? () ()
)

;;; SCROLL statement
;; Form no. 1
(meta-sexp:defrule rule2287? () ()
  (:and "SCROLL" (:? "FROM-CURRENT") (:? (:or "UP" "DOWN"))
   (:? (:rule rule2286?))))

;; frame-phrase
(meta-sexp:defrule rule2286? () ()
)

;;; SAVE CACHE statement
;; Form no. 1
(meta-sexp:defrule rule2292? () ()
  (:and "SAVE" "CACHE" (:or "CURRENT" "COMPLETE")
   (:or (:rule rule2288?) (:and "VALUE" "(" (:rule rule2289?) ")")) "TO"
   (:or (:rule rule2290?) (:and "VALUE" "(" (:rule rule2291?) ")"))
   (:? "NO-ERROR")))

;; database-name
(meta-sexp:defrule rule2288? () ()
)

;; char-expr
(meta-sexp:defrule rule2289? () ()
)

;; pathname
(meta-sexp:defrule rule2290? () ()
)

;; char-expr
(meta-sexp:defrule rule2291? () ()
)

;;; RUN SUPER statement
;; Form no. 1
(meta-sexp:defrule rule2295? () ()
  (:and "RUN" "SUPER"
   (:?
    (:and "(" (:rule rule2293?) (:* (:? (:and "," (:rule rule2294?))))
     (:? (:rule whitespace?)) ")"))
   (:? "NO-ERROR")))

;; parameter
(meta-sexp:defrule rule2293? () ()
)

;; parameter
(meta-sexp:defrule rule2294? () ()
)

;;; RUN STORED-PROCEDURE statement
;; Form no. 1
(meta-sexp:defrule rule2300? () ()
  (:and "RUN" "STORED-PROCEDURE" (:rule rule2296?)
   (:? (:and (:rule rule2297?) "=" "PROC-HANDLE")) (:? "NO-ERROR")
   (:?
    (:and "(" (:rule rule2298?) (:* (:? (:and "," (:rule rule2299?))))
     (:? (:rule whitespace?)) ")"))))

;; procedure
(meta-sexp:defrule rule2296? () ()
)

;; integer-field
(meta-sexp:defrule rule2297? () ()
)

;; parameter
(meta-sexp:defrule rule2298? () ()
)

;; parameter
(meta-sexp:defrule rule2299? () ()
)

;;; RUN statement
;; Form no. 1
(meta-sexp:defrule rule2305? () ()
  (:and (:rule rule2301?) (:rule rule2302?) (:rule rule2303?)
   (:rule rule2304?)))

;; RUN
(meta-sexp:defrule rule2301? () ()
)

;; operationName
(meta-sexp:defrule rule2302? () ()
)

;; IN
(meta-sexp:defrule rule2303? () ()
)

;; hPortType
(meta-sexp:defrule rule2304? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2309? () ()
  (:and "RUN" (:rule rule2306?) (:? (:and "SET" (:rule rule2307?))) "ON"
   "SERVER" (:rule rule2308?) (:? "NO-ERROR")))

;; portTypeName
(meta-sexp:defrule rule2306? () ()
)

;; hPortType
(meta-sexp:defrule rule2307? () ()
)

;; hWebService
(meta-sexp:defrule rule2308? () ()
)


;; Form no. 3
(meta-sexp:defrule rule2318? () ()
  (:and "RUN" (:or (:rule rule2310?) (:and "VALUE" "(" (:rule rule2311?) ")"))
   (:? (:and "IN" (:rule rule2312?)))
   (:?
    (:and "ASYNCHRONOUS" (:? (:and "SET" (:rule rule2313?)))
     (:?
      (:and "EVENT-PROCEDURE" (:rule rule2314?)
       (:? (:and "IN" (:rule rule2315?)))))))
   (:?
    (:and "(" (:rule rule2316?) (:* (:? (:and "," (:rule rule2317?))))
     (:? (:rule whitespace?)) ")"))
   (:? "NO-ERROR")))

;; intern-proc-name
(meta-sexp:defrule rule2310? () ()
)

;; intern-expression
(meta-sexp:defrule rule2311? () ()
)

;; proc-handle
(meta-sexp:defrule rule2312? () ()
)

;; async-request-handle
(meta-sexp:defrule rule2313? () ()
)

;; event-internal-procedure
(meta-sexp:defrule rule2314? () ()
)

;; procedure-context
(meta-sexp:defrule rule2315? () ()
)

;; parameter
(meta-sexp:defrule rule2316? () ()
)

;; parameter
(meta-sexp:defrule rule2317? () ()
)


;; Form no. 4
(meta-sexp:defrule rule2319? () ()
  "RUN")

;;; RETURN statement
;; Form no. 1
(meta-sexp:defrule rule2321? () ()
  (:and "RETURN" (:? (:or "ERROR" "NO-APPLY")) (:? (:rule rule2320?))))

;; return-value
(meta-sexp:defrule rule2320? () ()
)

;;; REPOSITION statement
;; Form no. 1
(meta-sexp:defrule rule2329? () ()
  (:and "REPOSITION" (:rule rule2322?)
   (:or
    (:and "TO" "ROWID" (:rule rule2323?) (:* (:? (:and "," (:rule rule2324?))))
     (:? (:rule whitespace?)) (:? "NO-ERROR"))
    (:and "TO" "RECID" (:rule rule2325?) (:? "NO-ERROR"))
    (:and "ROW" (:rule rule2326?)) (:and "FORWARDS" (:rule rule2327?))
    (:and "BACKWARDS" (:rule rule2328?)))))

;; query
(meta-sexp:defrule rule2322? () ()
)

;; rowid1
(meta-sexp:defrule rule2323? () ()
)

;; rowid2
(meta-sexp:defrule rule2324? () ()
)

;; recid
(meta-sexp:defrule rule2325? () ()
)

;; n
(meta-sexp:defrule rule2326? () ()
)

;; n
(meta-sexp:defrule rule2327? () ()
)

;; n
(meta-sexp:defrule rule2328? () ()
)

;;; RELEASE OBJECT statement
;; Form no. 1
(meta-sexp:defrule rule2331? () ()
  (:and "RELEASE" "OBJECT" (:rule rule2330?) (:? "NO-ERROR")))

;; COM-hdl-var
(meta-sexp:defrule rule2330? () ()
)

;;; RELEASE EXTERNAL statement
;; Form no. 1
(meta-sexp:defrule rule2333? () ()
  (:and "RELEASE" "EXTERNAL" (:? "PROCEDURE") "\"" (:rule rule2332?) "\""))

;; dll-name
(meta-sexp:defrule rule2332? () ()
)

;;; RELEASE statement
;; Form no. 1
(meta-sexp:defrule rule2335? () ()
  (:and "RELEASE" (:rule rule2334?) (:? "NO-ERROR")))

;; record
(meta-sexp:defrule rule2334? () ()
)

;;; READKEY statement
;; Form no. 1
(meta-sexp:defrule rule2338? () ()
  (:and "READKEY" (:? (:and "STREAM" (:rule rule2336?)))
   (:? (:and "PAUSE" (:rule rule2337?)))))

;; stream
(meta-sexp:defrule rule2336? () ()
)

;; n
(meta-sexp:defrule rule2337? () ()
)

;;; RAW-TRANSFER statement
;; Form no. 1
(meta-sexp:defrule rule2345? () ()
  (:and "RAW-TRANSFER"
   (:or
    (:and (:? "BUFFER") (:rule rule2339?) "TO" (:? "FIELD") (:rule rule2340?))
    (:and (:? "FIELD") (:rule rule2341?) "TO" (:? "BUFFER") (:rule rule2342?))
    (:and (:? "BUFFER") (:rule rule2343?) "TO" (:? "BUFFER")
     (:rule rule2344?)))
   (:? "NO-ERROR")))

;; buffer
(meta-sexp:defrule rule2339? () ()
)

;; raw-field
(meta-sexp:defrule rule2340? () ()
)

;; raw-field
(meta-sexp:defrule rule2341? () ()
)

;; buffer
(meta-sexp:defrule rule2342? () ()
)

;; buffer
(meta-sexp:defrule rule2343? () ()
)

;; buffer
(meta-sexp:defrule rule2344? () ()
)

;;; QUIT statement
;; Form no. 1
(meta-sexp:defrule rule2346? () ()
  "QUIT")

;;; PUT-UNSIGNED-SHORT statement
;; Form no. 1
(meta-sexp:defrule rule2350? () ()
  (:and "PUT-UNSIGNED-SHORT" "(" (:rule rule2347?) "," (:rule rule2348?) ")"
   "=" (:rule rule2349?)))

;; destination
(meta-sexp:defrule rule2347? () ()
)

;; position
(meta-sexp:defrule rule2348? () ()
)

;; expression
(meta-sexp:defrule rule2349? () ()
)

;;; PUT-UNSIGNED-LONG statement
;; Form no. 1
(meta-sexp:defrule rule2354? () ()
  (:and "PUT-UNSIGNED-LONG" "(" (:rule rule2351?) "," (:rule rule2352?) ")" "="
   (:rule rule2353?)))

;; destination
(meta-sexp:defrule rule2351? () ()
)

;; position
(meta-sexp:defrule rule2352? () ()
)

;; expression
(meta-sexp:defrule rule2353? () ()
)

;;; PUT-STRING statement
;; Form no. 1
(meta-sexp:defrule rule2359? () ()
  (:and "PUT-STRING" "(" (:rule rule2355?) "," (:rule rule2356?) ","
   (:? (:rule rule2357?)) ")" "=" (:rule rule2358?)))

;; destination
(meta-sexp:defrule rule2355? () ()
)

;; position
(meta-sexp:defrule rule2356? () ()
)

;; numbytes
(meta-sexp:defrule rule2357? () ()
)

;; expression
(meta-sexp:defrule rule2358? () ()
)

;;; PUT-SHORT statement
;; Form no. 1
(meta-sexp:defrule rule2363? () ()
  (:and "PUT-SHORT" "(" (:rule rule2360?) "," (:rule rule2361?) ")" "="
   (:rule rule2362?)))

;; destination
(meta-sexp:defrule rule2360? () ()
)

;; position
(meta-sexp:defrule rule2361? () ()
)

;; expression
(meta-sexp:defrule rule2362? () ()
)

;;; PUT-LONG statement
;; Form no. 1
(meta-sexp:defrule rule2367? () ()
  (:and "PUT-LONG" "(" (:rule rule2364?) "," (:rule rule2365?) ")" "="
   (:rule rule2366?)))

;; destination
(meta-sexp:defrule rule2364? () ()
)

;; position
(meta-sexp:defrule rule2365? () ()
)

;; expression
(meta-sexp:defrule rule2366? () ()
)

;;; PUT-KEY-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule2372? () ()
  (:and "PUT-KEY-VALUE"
   (:or
    (:and "SECTION" (:rule rule2368?) "KEY" (:or (:rule rule2369?) "DEFAULT")
     "VALUE" (:rule rule2370?))
    (:and (:or "COLOR" "FONT") (:or (:rule rule2371?) "ALL")))
   (:? "NO-ERROR")))

;; section-name
(meta-sexp:defrule rule2368? () ()
)

;; key-name
(meta-sexp:defrule rule2369? () ()
)

;; value
(meta-sexp:defrule rule2370? () ()
)

;; number
(meta-sexp:defrule rule2371? () ()
)

;;; PUT-INT64 statement
;; Form no. 1
(meta-sexp:defrule rule2376? () ()
  (:and "PUT-INT64" "(" (:rule rule2373?) "," (:rule rule2374?) ")" "="
   (:rule rule2375?)))

;; destination
(meta-sexp:defrule rule2373? () ()
)

;; position
(meta-sexp:defrule rule2374? () ()
)

;; expression
(meta-sexp:defrule rule2375? () ()
)

;;; PUT-FLOAT statement
;; Form no. 1
(meta-sexp:defrule rule2380? () ()
  (:and "PUT-FLOAT" "(" (:rule rule2377?) "," (:rule rule2378?) ")" "="
   (:rule rule2379?)))

;; destination
(meta-sexp:defrule rule2377? () ()
)

;; position
(meta-sexp:defrule rule2378? () ()
)

;; expression
(meta-sexp:defrule rule2379? () ()
)

;;; PUT-DOUBLE statement
;; Form no. 1
(meta-sexp:defrule rule2384? () ()
  (:and "PUT-DOUBLE" "(" (:rule rule2381?) "," (:rule rule2382?) ")" "="
   (:rule rule2383?)))

;; destination
(meta-sexp:defrule rule2381? () ()
)

;; position
(meta-sexp:defrule rule2382? () ()
)

;; expression
(meta-sexp:defrule rule2383? () ()
)

;;; PUT-BYTES statement
;; Form no. 1
(meta-sexp:defrule rule2388? () ()
  (:and "PUT-BYTES" "(" (:rule rule2385?) "," (:rule rule2386?) ")" "="
   (:rule rule2387?)))

;; destination
(meta-sexp:defrule rule2385? () ()
)

;; position
(meta-sexp:defrule rule2386? () ()
)

;; expression
(meta-sexp:defrule rule2387? () ()
)

;;; PUT-BYTE statement
;; Form no. 1
(meta-sexp:defrule rule2392? () ()
  (:and "PUT-BYTE" "(" (:rule rule2389?) "," (:rule rule2390?) ")" "="
   (:rule rule2391?)))

;; destination
(meta-sexp:defrule rule2389? () ()
)

;; position
(meta-sexp:defrule rule2390? () ()
)

;; expression
(meta-sexp:defrule rule2391? () ()
)

;;; PUT-BITS statement
;; Form no. 1
(meta-sexp:defrule rule2397? () ()
  (:and "PUT-BITS" "(" (:rule rule2393?) "," (:rule rule2394?) ","
   (:rule rule2395?) ")" "=" (:rule rule2396?)))

;; destination
(meta-sexp:defrule rule2393? () ()
)

;; position
(meta-sexp:defrule rule2394? () ()
)

;; numbits
(meta-sexp:defrule rule2395? () ()
)

;; expression
(meta-sexp:defrule rule2396? () ()
)

;;; PUT statement
;; Form no. 1
(meta-sexp:defrule rule2400? () ()
  (:and "PUT" (:? (:and "STREAM" (:rule rule2398?))) "CONTROL"
   (:* (:rule rule2399?)) (:? (:rule whitespace?))))

;; stream
(meta-sexp:defrule rule2398? () ()
)

;; expression
(meta-sexp:defrule rule2399? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2407? () ()
  (:and "PUT" (:? (:and "STREAM" (:rule rule2401?))) (:? "UNFORMATTED")
   (:*
    (:?
     (:or
      (:and (:rule rule2402?) (:? (:and "FORMAT" (:rule rule2403?)))
       (:? (:and (:or "AT" "TO") (:rule rule2404?))))
      (:and "SKIP" (:? (:and "(" (:rule rule2405?) ")")))
      (:and "SPACE" (:? (:and "(" (:rule rule2406?) ")"))))))
   (:? (:rule whitespace?))))

;; stream
(meta-sexp:defrule rule2401? () ()
)

;; expression
(meta-sexp:defrule rule2402? () ()
)

;; string
(meta-sexp:defrule rule2403? () ()
)

;; expression
(meta-sexp:defrule rule2404? () ()
)

;; expression
(meta-sexp:defrule rule2405? () ()
)

;; expression
(meta-sexp:defrule rule2406? () ()
)

;;; PUT SCREEN statement
;; Form no. 1
(meta-sexp:defrule rule2412? () ()
  (:and "PUT" "SCREEN" (:? (:or "ATTR-SPACE" "NO-ATTR-SPACE"))
   (:? (:and "COLOR" (:rule rule2408?))) (:? (:and "COLUMN" (:rule rule2409?)))
   (:? (:and "ROW" (:rule rule2410?))) (:rule rule2411?)))

;; color-phrase
(meta-sexp:defrule rule2408? () ()
)

;; expression
(meta-sexp:defrule rule2409? () ()
)

;; expression
(meta-sexp:defrule rule2410? () ()
)

;; expression
(meta-sexp:defrule rule2411? () ()
)

;;; PUT CURSOR statement
;; Form no. 1
(meta-sexp:defrule rule2415? () ()
  (:and "PUT" "CURSOR"
   (:or "OFF"
    (:and (:? (:and "ROW" (:rule rule2413?)))
     (:? (:and "COLUMN" (:rule rule2414?)))))))

;; expression
(meta-sexp:defrule rule2413? () ()
)

;; expression
(meta-sexp:defrule rule2414? () ()
)

;;; PUBLISH statement
;; Form no. 1
(meta-sexp:defrule rule2420? () ()
  (:and "PUBLISH" (:rule rule2416?) (:? (:and "FROM" (:rule rule2417?)))
   (:?
    (:and "(" (:rule rule2418?) (:* (:? (:and "," (:rule rule2419?))))
     (:? (:rule whitespace?)) ")"))))

;; event-name
(meta-sexp:defrule rule2416? () ()
)

;; publisher-handle
(meta-sexp:defrule rule2417? () ()
)

;; parameter
(meta-sexp:defrule rule2418? () ()
)

;; parameter
(meta-sexp:defrule rule2419? () ()
)

;;; PROCESS EVENTS statement
;; Form no. 1
(meta-sexp:defrule rule2421? () ()
  (:and "PROCESS" "EVENTS"))

;;; PROCEDURE statement
;; Form no. 1
(meta-sexp:defrule rule2426? () ()
  (:and "PROCEDURE" (:rule rule2422?)
   (:or
    (:and "EXTERNAL" "\"" (:rule rule2423?) "\""
     (:? (:or "CDECL" "PASCAL" "STDCALL"))
     (:? (:and "ORDINAL" (:rule rule2424?))) (:? "PERSISTENT"))
    (:and "IN" "SUPER"))
   ":" (:? (:rule rule2425?))))

;; proc-name
(meta-sexp:defrule rule2422? () ()
)

;; dllname
(meta-sexp:defrule rule2423? () ()
)

;; n
(meta-sexp:defrule rule2424? () ()
)

;; procedure-body
(meta-sexp:defrule rule2425? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2429? () ()
  (:and "PROCEDURE" (:rule rule2427?) (:? "PRIVATE") ":"
   (:? (:rule rule2428?))))

;; proc-name
(meta-sexp:defrule rule2427? () ()
)

;; procedure-body
(meta-sexp:defrule rule2428? () ()
)

;;; PAUSE statement
;; Form no. 1
(meta-sexp:defrule rule2433? () ()
  (:and "PAUSE" (:? (:rule rule2430?)) (:? "BEFORE-HIDE")
   (:? (:or (:and "MESSAGE" (:rule rule2431?)) "NO-MESSAGE"))
   (:? (:and "IN" "WINDOW" (:rule rule2432?)))))

;; n
(meta-sexp:defrule rule2430? () ()
)

;; message
(meta-sexp:defrule rule2431? () ()
)

;; window
(meta-sexp:defrule rule2432? () ()
)

;;; PAGE statement
;; Form no. 1
(meta-sexp:defrule rule2435? () ()
  (:and "PAGE" (:? (:and "STREAM" (:rule rule2434?)))))

;; stream
(meta-sexp:defrule rule2434? () ()
)

;;; OVERLAY statement
;; Form no. 1
(meta-sexp:defrule rule2441? () ()
  (:and "OVERLAY" "(" (:rule rule2436?) "," (:rule rule2437?)
   (:? (:and "," (:rule rule2438?) (:? (:and "," (:rule rule2439?))))) ")" "="
   (:rule rule2440?)))

;; target
(meta-sexp:defrule rule2436? () ()
)

;; position
(meta-sexp:defrule rule2437? () ()
)

;; length
(meta-sexp:defrule rule2438? () ()
)

;; type
(meta-sexp:defrule rule2439? () ()
)

;; expression
(meta-sexp:defrule rule2440? () ()
)

;;; OUTPUT TO statement
;; Form no. 1
(meta-sexp:defrule rule2456? () ()
  (:and "OUTPUT" (:? (:and "STREAM" (:rule rule2442?))) "TO"
   (:or (:and "PRINTER" (:? (:rule rule2443?))) (:rule rule2444?)
    (:rule rule2445?) "TERMINAL" (:and "VALUE" "(" (:rule rule2446?) ")")
    (:and "\"" "CLIPBOARD" "\""))
   (:?
    (:and "LOB-DIR"
     (:or (:rule rule2447?) (:and "VALUE" "(" (:rule rule2448?) ")"))))
   (:?
    (:and "NUM-COPIES"
     (:or (:rule rule2449?) (:and "VALUE" "(" (:rule rule2450?) ")"))))
   (:? "COLLATE") (:? (:or "LANDSCAPE" "PORTRAIT")) (:? "APPEND") (:? "BINARY")
   (:? (:or "ECHO" "NO-ECHO")) (:? "KEEP-MESSAGES")
   (:? (:or "NO-MAP" (:and "MAP" (:rule rule2451?)))) (:? "PAGED")
   (:?
    (:and "PAGE-SIZE"
     (:or (:rule rule2452?) (:and "VALUE" "(" (:rule rule2453?) ")"))))
   (:? "UNBUFFERED")
   (:?
    (:or "NO-CONVERT"
     (:and "CONVERT" (:? (:and "TARGET" (:rule rule2454?)))
      (:? (:and "SOURCE" (:rule rule2455?))))))))

;; stream
(meta-sexp:defrule rule2442? () ()
)

;; printer-name
(meta-sexp:defrule rule2443? () ()
)

;; opsys-file
(meta-sexp:defrule rule2444? () ()
)

;; opsys-device
(meta-sexp:defrule rule2445? () ()
)

;; expression
(meta-sexp:defrule rule2446? () ()
)

;; constant
(meta-sexp:defrule rule2447? () ()
)

;; expression
(meta-sexp:defrule rule2448? () ()
)

;; constant
(meta-sexp:defrule rule2449? () ()
)

;; expression
(meta-sexp:defrule rule2450? () ()
)

;; protermcap-entry
(meta-sexp:defrule rule2451? () ()
)

;; constant
(meta-sexp:defrule rule2452? () ()
)

;; expression
(meta-sexp:defrule rule2453? () ()
)

;; target-codepage
(meta-sexp:defrule rule2454? () ()
)

;; source-codepage
(meta-sexp:defrule rule2455? () ()
)

;;; OUTPUT THROUGH statement
;; Form no. 1
(meta-sexp:defrule rule2467? () ()
  (:and "OUTPUT" (:? (:and "STREAM" (:rule rule2457?))) "THROUGH"
   (:or (:rule rule2458?) (:and "VALUE" "(" (:rule rule2459?) ")"))
   (:* (:? (:or (:rule rule2460?) (:and "VALUE" "(" (:rule rule2461?) ")"))))
   (:? (:rule whitespace?)) (:? (:or "ECHO" "NO-ECHO"))
   (:? (:or (:and "MAP" (:rule rule2462?)) "NO-MAP")) (:? "PAGED")
   (:?
    (:and "PAGE-SIZE"
     (:or (:rule rule2463?) (:and "VALUE" "(" (:rule rule2464?) ")"))))
   (:? "UNBUFFERED")
   (:?
    (:or "NO-CONVERT"
     (:and "CONVERT" (:? (:and "TARGET" (:rule rule2465?)))
      (:? (:and "SOURCE" (:rule rule2466?))))))))

;; stream
(meta-sexp:defrule rule2457? () ()
)

;; program-name
(meta-sexp:defrule rule2458? () ()
)

;; expression
(meta-sexp:defrule rule2459? () ()
)

;; argument
(meta-sexp:defrule rule2460? () ()
)

;; expression
(meta-sexp:defrule rule2461? () ()
)

;; protermcap-entry
(meta-sexp:defrule rule2462? () ()
)

;; constant
(meta-sexp:defrule rule2463? () ()
)

;; expression
(meta-sexp:defrule rule2464? () ()
)

;; target-codepage
(meta-sexp:defrule rule2465? () ()
)

;; source-codepage
(meta-sexp:defrule rule2466? () ()
)

;;; OUTPUT CLOSE statement
;; Form no. 1
(meta-sexp:defrule rule2469? () ()
  (:and "OUTPUT" (:? (:and "STREAM" (:rule rule2468?))) "CLOSE"))

;; stream
(meta-sexp:defrule rule2468? () ()
)

;;; OS-RENAME statement
;; Form no. 1
(meta-sexp:defrule rule2474? () ()
  (:and "OS-RENAME"
   (:or (:rule rule2470?) (:and "VALUE" "(" (:rule rule2471?) ")"))
   (:or (:rule rule2472?) (:and "VALUE" "(" (:rule rule2473?) ")"))))

;; source-filename
(meta-sexp:defrule rule2470? () ()
)

;; expression
(meta-sexp:defrule rule2471? () ()
)

;; target-filename
(meta-sexp:defrule rule2472? () ()
)

;; expression
(meta-sexp:defrule rule2473? () ()
)

;;; OS-DELETE statement
;; Form no. 1
(meta-sexp:defrule rule2477? () ()
  (:and "OS-DELETE"
   (:* (:or (:rule rule2475?) (:and "VALUE" "(" (:rule rule2476?) ")")))
   (:? (:rule whitespace?)) (:? "RECURSIVE")))

;; filename
(meta-sexp:defrule rule2475? () ()
)

;; expression
(meta-sexp:defrule rule2476? () ()
)

;;; OS-CREATE-DIR statement
;; Form no. 1
(meta-sexp:defrule rule2480? () ()
  (:and "OS-CREATE-DIR"
   (:* (:or (:rule rule2478?) (:and "VALUE" "(" (:rule rule2479?) ")")))
   (:? (:rule whitespace?))))

;; dirname
(meta-sexp:defrule rule2478? () ()
)

;; expression
(meta-sexp:defrule rule2479? () ()
)

;;; OS-COPY statement
;; Form no. 1
(meta-sexp:defrule rule2485? () ()
  (:and "OS-COPY"
   (:or (:rule rule2481?) (:and "VALUE" "(" (:rule rule2482?) ")"))
   (:or (:rule rule2483?) (:and "VALUE" "(" (:rule rule2484?) ")"))))

;; source-filename
(meta-sexp:defrule rule2481? () ()
)

;; expression
(meta-sexp:defrule rule2482? () ()
)

;; target-filename
(meta-sexp:defrule rule2483? () ()
)

;; expression
(meta-sexp:defrule rule2484? () ()
)

;;; OS-COMMAND statement
;; Form no. 1
(meta-sexp:defrule rule2488? () ()
  (:and "OS-COMMAND" (:? (:or "SILENT" "NO-WAIT")) (:? "NO-CONSOLE")
   (:* (:? (:or (:rule rule2486?) (:and "VALUE" "(" (:rule rule2487?) ")"))))
   (:? (:rule whitespace?))))

;; command-token
(meta-sexp:defrule rule2486? () ()
)

;; expression
(meta-sexp:defrule rule2487? () ()
)

;;; OS-APPEND statement
;; Form no. 1
(meta-sexp:defrule rule2493? () ()
  (:and "OS-APPEND"
   (:or (:rule rule2489?) (:and "VALUE" "(" (:rule rule2490?) ")"))
   (:or (:rule rule2491?) (:and "VALUE" "(" (:rule rule2492?) ")"))))

;; source-filename
(meta-sexp:defrule rule2489? () ()
)

;; expression
(meta-sexp:defrule rule2490? () ()
)

;; target-filename
(meta-sexp:defrule rule2491? () ()
)

;; expression
(meta-sexp:defrule rule2492? () ()
)

;;; OPEN QUERY statement
;; Form no. 1
(meta-sexp:defrule rule2503? () ()
  (:and "OPEN" "QUERY" (:rule rule2494?) (:or "FOR" "PRESELECT") "EACH"
   (:rule rule2495?)
   (:* (:? (:and "," (:or "EACH" "FIRST" "LAST") (:rule rule2496?))))
   (:? (:rule whitespace?)) (:? (:rule rule2497?))
   (:*
    (:?
     (:or (:and "BY" (:rule rule2498?) (:? "DESCENDING"))
      (:and "COLLATE" "(" (:rule rule2499?) "," (:rule rule2500?)
       (:? (:and "," (:rule rule2501?))) ")" (:? "DESCENDING")))))
   (:? (:rule whitespace?)) (:? "INDEXED-REPOSITION")
   (:? (:and "MAX-ROWS" (:rule rule2502?)))))

;; query
(meta-sexp:defrule rule2494? () ()
)

;; record-phrase
(meta-sexp:defrule rule2495? () ()
)

;; record-phrase
(meta-sexp:defrule rule2496? () ()
)

;; query-tuning-phrase
(meta-sexp:defrule rule2497? () ()
)

;; expression
(meta-sexp:defrule rule2498? () ()
)

;; string
(meta-sexp:defrule rule2499? () ()
)

;; strength
(meta-sexp:defrule rule2500? () ()
)

;; collation
(meta-sexp:defrule rule2501? () ()
)

;; num-results
(meta-sexp:defrule rule2502? () ()
)

;;; ON statement
;; Form no. 1
(meta-sexp:defrule rule2505? () ()
  (:and "ON" "\"" "WEB-NOTIFY" "\"" "ANYWHERE" (:rule rule2504?)))

;; trigger-block
(meta-sexp:defrule rule2504? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2508? () ()
  (:and "ON" (:rule rule2506?) (:rule rule2507?)))

;; key-label
(meta-sexp:defrule rule2506? () ()
)

;; key-function
(meta-sexp:defrule rule2507? () ()
)


;; Form no. 3
(meta-sexp:defrule rule2513? () ()
  (:and "ON" (:rule rule2509?) "OF" (:rule rule2510?) (:? (:rule rule2511?))
   (:? "OVERRIDE") (:or (:rule rule2512?) "REVERT")))

;; event
(meta-sexp:defrule rule2509? () ()
)

;; database-object
(meta-sexp:defrule rule2510? () ()
)

;; referencing-phrase
(meta-sexp:defrule rule2511? () ()
)

;; trigger-block
(meta-sexp:defrule rule2512? () ()
)


;; Form no. 4
(meta-sexp:defrule rule2521? () ()
  (:and "ON" (:rule rule2514?)
   (:or "ANYWHERE"
    (:and "OF" (:rule rule2515?)
     (:* (:? (:and "OR" (:rule rule2516?) "OF" (:rule rule2517?))))
     (:? (:rule whitespace?)) (:? "ANYWHERE")))
   (:or (:rule rule2518?) "REVERT"
    (:and "PERSISTENT" "RUN" (:rule rule2519?)
     (:? (:and "(" (:rule rule2520?) ")"))))))

;; event-list
(meta-sexp:defrule rule2514? () ()
)

;; widget-list
(meta-sexp:defrule rule2515? () ()
)

;; event-list
(meta-sexp:defrule rule2516? () ()
)

;; widget-list
(meta-sexp:defrule rule2517? () ()
)

;; trigger-block
(meta-sexp:defrule rule2518? () ()
)

;; procedure
(meta-sexp:defrule rule2519? () ()
)

;; input-parameters
(meta-sexp:defrule rule2520? () ()
)

;;; NEXT-PROMPT statement
;; Form no. 1
(meta-sexp:defrule rule2524? () ()
  (:and "NEXT-PROMPT" (:rule rule2522?) (:? (:rule rule2523?))))

;; field
(meta-sexp:defrule rule2522? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2523? () ()
)

;;; NEXT statement
;; Form no. 1
(meta-sexp:defrule rule2526? () ()
  (:and "NEXT" (:? (:rule rule2525?))))

;; label
(meta-sexp:defrule rule2525? () ()
)

;;; METHOD statement
;; Form no. 1
(meta-sexp:defrule rule2532? () ()
  (:and "METHOD" (:rule rule2527?) (:or "VOID" (:rule rule2528?))
   (:rule rule2529?) "("
   (:?
    (:and (:rule rule2530?) (:* (:? (:and "," (:rule rule2531?))))
     (:? (:rule whitespace?))))
   ")"))

;; method-modifiers
(meta-sexp:defrule rule2527? () ()
)

;; return-type
(meta-sexp:defrule rule2528? () ()
)

;; method-name
(meta-sexp:defrule rule2529? () ()
)

;; parameter
(meta-sexp:defrule rule2530? () ()
)

;; parameter
(meta-sexp:defrule rule2531? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2539? () ()
  (:and "METHOD" (:rule rule2533?) (:or "VOID" (:rule rule2534?))
   (:rule rule2535?) "("
   (:?
    (:and (:rule rule2536?) (:* (:? (:and "," (:rule rule2537?))))
     (:? (:rule whitespace?))))
   ")" ":" (:rule rule2538?)))

;; method-modifiers
(meta-sexp:defrule rule2533? () ()
)

;; return-type
(meta-sexp:defrule rule2534? () ()
)

;; method-name
(meta-sexp:defrule rule2535? () ()
)

;; parameter
(meta-sexp:defrule rule2536? () ()
)

;; parameter
(meta-sexp:defrule rule2537? () ()
)

;; method-body
(meta-sexp:defrule rule2538? () ()
)

;;; MESSAGE statement
;; Form no. 1
(meta-sexp:defrule rule2551? () ()
  (:and "MESSAGE" (:? (:and "COLOR" (:rule rule2540?)))
   (:*
    (:or (:rule rule2541?)
     (:and "SKIP" (:? (:and "(" (:rule rule2542?) ")")))))
   (:? (:rule whitespace?))
   (:?
    (:and "VIEW-AS" "ALERT-BOX" (:? (:rule rule2543?))
     (:? (:and "BUTTONS" (:rule rule2544?)))
     (:? (:and "TITLE" (:rule rule2545?)))))
   (:?
    (:and (:or "SET" "UPDATE") (:rule rule2546?)
     (:or (:and "AS" (:rule rule2547?)) (:and "LIKE" (:rule rule2548?)))
     (:? (:and "FORMAT" (:rule rule2549?))) (:? "AUTO-RETURN")))
   (:? (:and "IN" "WINDOW" (:rule rule2550?)))))

;; color-phrase
(meta-sexp:defrule rule2540? () ()
)

;; expression
(meta-sexp:defrule rule2541? () ()
)

;; n
(meta-sexp:defrule rule2542? () ()
)

;; alert-type
(meta-sexp:defrule rule2543? () ()
)

;; button-set
(meta-sexp:defrule rule2544? () ()
)

;; title-string
(meta-sexp:defrule rule2545? () ()
)

;; field
(meta-sexp:defrule rule2546? () ()
)

;; datatype
(meta-sexp:defrule rule2547? () ()
)

;; field
(meta-sexp:defrule rule2548? () ()
)

;; string
(meta-sexp:defrule rule2549? () ()
)

;; window
(meta-sexp:defrule rule2550? () ()
)

;;; LOAD-PICTURE statement
;; Form no. 1
(meta-sexp:defrule rule2553? () ()
  (:and "LOAD-PICTURE" (:? (:rule rule2552?))))

;; image
(meta-sexp:defrule rule2552? () ()
)

;;; LOAD statement
;; Form no. 1
(meta-sexp:defrule rule2557? () ()
  (:and "LOAD" (:rule rule2554?) (:? (:and "DIR" (:rule rule2555?)))
   (:? "APPLICATION") (:? "NEW")
   (:? (:and "BASE-KEY" (:or (:rule rule2556?) (:and "\"" "INI" "\""))))
   (:? "NO-ERROR")))

;; environment
(meta-sexp:defrule rule2554? () ()
)

;; directory
(meta-sexp:defrule rule2555? () ()
)

;; key-name
(meta-sexp:defrule rule2556? () ()
)

;;; LEAVE statement
;; Form no. 1
(meta-sexp:defrule rule2559? () ()
  (:and "LEAVE" (:? (:rule rule2558?))))

;; label
(meta-sexp:defrule rule2558? () ()
)

;;; INTERFACE statement
;; Form no. 1
(meta-sexp:defrule rule2562? () ()
  (:and "INTERFACE" (:rule rule2560?) ":" (:rule rule2561?)))

;; type-name
(meta-sexp:defrule rule2560? () ()
)

;; interface-body
(meta-sexp:defrule rule2561? () ()
)

;;; INPUT-OUTPUT THROUGH statement
;; Form no. 1
(meta-sexp:defrule rule2571? () ()
  (:and "INPUT-OUTPUT" (:? (:and "STREAM" (:rule rule2563?))) "THROUGH"
   (:or (:rule rule2564?) (:and "VALUE" "(" (:rule rule2565?) ")"))
   (:* (:? (:or (:rule rule2566?) (:and "VALUE" "(" (:rule rule2567?) ")"))))
   (:? (:rule whitespace?)) (:? (:or "ECHO" "NO-ECHO"))
   (:? (:or (:and "MAP" (:rule rule2568?)) "NO-MAP")) (:? "UNBUFFERED")
   (:?
    (:or "NO-CONVERT"
     (:and "CONVERT" (:? (:and "TARGET" (:rule rule2569?)))
      (:? (:and "SOURCE" (:rule rule2570?))))))))

;; stream
(meta-sexp:defrule rule2563? () ()
)

;; program-name
(meta-sexp:defrule rule2564? () ()
)

;; expression
(meta-sexp:defrule rule2565? () ()
)

;; argument
(meta-sexp:defrule rule2566? () ()
)

;; expression
(meta-sexp:defrule rule2567? () ()
)

;; protermcap-entry
(meta-sexp:defrule rule2568? () ()
)

;; target-codepage
(meta-sexp:defrule rule2569? () ()
)

;; source-codepage
(meta-sexp:defrule rule2570? () ()
)

;;; INPUT-OUTPUT CLOSE statement
;; Form no. 1
(meta-sexp:defrule rule2573? () ()
  (:and "INPUT-OUTPUT" (:? (:and "STREAM" (:rule rule2572?))) "CLOSE"))

;; stream
(meta-sexp:defrule rule2572? () ()
)

;;; INPUT THROUGH statement
;; Form no. 1
(meta-sexp:defrule rule2582? () ()
  (:and "INPUT" (:? (:and "STREAM" (:rule rule2574?))) "THROUGH"
   (:or (:rule rule2575?) (:and "VALUE" "(" (:rule rule2576?) ")"))
   (:* (:? (:or (:rule rule2577?) (:and "VALUE" "(" (:rule rule2578?) ")"))))
   (:? (:rule whitespace?)) (:? (:or "ECHO" "NO-ECHO"))
   (:? (:or (:and "MAP" (:rule rule2579?)) "NO-MAP")) (:? "UNBUFFERED")
   (:?
    (:or "NO-CONVERT"
     (:and "CONVERT" (:? (:and "TARGET" (:rule rule2580?)))
      (:? (:and "SOURCE" (:rule rule2581?))))))))

;; stream
(meta-sexp:defrule rule2574? () ()
)

;; program-name
(meta-sexp:defrule rule2575? () ()
)

;; expression
(meta-sexp:defrule rule2576? () ()
)

;; argument
(meta-sexp:defrule rule2577? () ()
)

;; expression
(meta-sexp:defrule rule2578? () ()
)

;; protermcap-entry
(meta-sexp:defrule rule2579? () ()
)

;; target-codepage
(meta-sexp:defrule rule2580? () ()
)

;; source-codepage
(meta-sexp:defrule rule2581? () ()
)

;;; INPUT FROM statement
;; Form no. 1
(meta-sexp:defrule rule2593? () ()
  (:and "INPUT" (:? (:and "STREAM" (:rule rule2583?))) "FROM"
   (:or (:rule rule2584?) (:rule rule2585?) "TERMINAL"
    (:and "VALUE" "(" (:rule rule2586?) ")")
    (:and "OS-DIR" "(" (:rule rule2587?) ")" (:? "NO-ATTR-LIST")))
   (:?
    (:and "LOB-DIR"
     (:or (:rule rule2588?) (:and "VALUE" "(" (:rule rule2589?) ")"))))
   (:? "BINARY") (:? (:or "ECHO" "NO-ECHO"))
   (:? (:or (:and "MAP" (:rule rule2590?)) "NO-MAP")) (:? "UNBUFFERED")
   (:?
    (:or "NO-CONVERT"
     (:and "CONVERT" (:? (:and "TARGET" (:rule rule2591?)))
      (:? (:and "SOURCE" (:rule rule2592?))))))))

;; stream
(meta-sexp:defrule rule2583? () ()
)

;; opsys-file
(meta-sexp:defrule rule2584? () ()
)

;; opsys-device
(meta-sexp:defrule rule2585? () ()
)

;; expression
(meta-sexp:defrule rule2586? () ()
)

;; directory
(meta-sexp:defrule rule2587? () ()
)

;; constant
(meta-sexp:defrule rule2588? () ()
)

;; expression
(meta-sexp:defrule rule2589? () ()
)

;; protermcap-entry
(meta-sexp:defrule rule2590? () ()
)

;; target-codepage
(meta-sexp:defrule rule2591? () ()
)

;; source-codepage
(meta-sexp:defrule rule2592? () ()
)

;;; INPUT CLOSE statement
;; Form no. 1
(meta-sexp:defrule rule2595? () ()
  (:and "INPUT" (:? (:and "STREAM" (:rule rule2594?))) "CLOSE"))

;; stream
(meta-sexp:defrule rule2594? () ()
)

;;; INPUT CLEAR statement
;; Form no. 1
(meta-sexp:defrule rule2596? () ()
  (:and "INPUT" "CLEAR"))

;;; IMPORT statement
;; Form no. 1
(meta-sexp:defrule rule2600? () ()
  (:and "IMPORT" (:? (:and "STREAM" (:rule rule2597?)))
   (:or (:rule rule2598?) (:rule rule2599?))))

;; stream
(meta-sexp:defrule rule2597? () ()
)

;; memptr
(meta-sexp:defrule rule2598? () ()
)

;; longchar
(meta-sexp:defrule rule2599? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2602? () ()
  (:and "IMPORT" (:? (:and "STREAM" (:rule rule2601?)))))

;; stream
(meta-sexp:defrule rule2601? () ()
)

;;; HIDE statement
;; Form no. 1
(meta-sexp:defrule rule2606? () ()
  (:and "HIDE" (:? (:and "STREAM" (:rule rule2603?)))
   (:? (:or (:rule rule2604?) "MESSAGE" "ALL")) (:? "NO-PAUSE")
   (:? (:and "IN" "WINDOW" (:rule rule2605?)))))

;; stream
(meta-sexp:defrule rule2603? () ()
)

;; widget-phrase
(meta-sexp:defrule rule2604? () ()
)

;; window
(meta-sexp:defrule rule2605? () ()
)

;;; GET-KEY-VALUE statement
;; Form no. 1
(meta-sexp:defrule rule2610? () ()
  (:and "GET-KEY-VALUE" "SECTION" (:rule rule2607?) "KEY"
   (:or (:rule rule2608?) "DEFAULT") "VALUE" (:rule rule2609?)))

;; section-name
(meta-sexp:defrule rule2607? () ()
)

;; key-name
(meta-sexp:defrule rule2608? () ()
)

;; key-value
(meta-sexp:defrule rule2609? () ()
)

;;; GET statement
;; Form no. 1
(meta-sexp:defrule rule2612? () ()
  (:and "GET" (:or "FIRST" "NEXT" "PREV" "LAST" "CURRENT") (:rule rule2611?)
   (:? (:or "SHARE-LOCK" "EXCLUSIVE-LOCK" "NO-LOCK")) (:? "NO-WAIT")))

;; query
(meta-sexp:defrule rule2611? () ()
)

;;; FUNCTION statement
;; Form no. 1
(meta-sexp:defrule rule2621? () ()
  (:and (:rule rule2613?) (:rule rule2614?) (:? (:rule rule2615?))
   (:rule rule2616?)
   (:? (:and "(" (:rule rule2617?) (:* (:? (:and "," (:rule rule2618?)))) ")"))
   (:rule rule2619?) (:rule rule2620?)))

;; FUNCTION
(meta-sexp:defrule rule2613? () ()
)

;; operationName
(meta-sexp:defrule rule2614? () ()
)

;; RETURNS
(meta-sexp:defrule rule2615? () ()
)

;; return-type
(meta-sexp:defrule rule2616? () ()
)

;; parameter
(meta-sexp:defrule rule2617? () ()
)

;; parameter
(meta-sexp:defrule rule2618? () ()
)

;; IN
(meta-sexp:defrule rule2619? () ()
)

;; hPortType
(meta-sexp:defrule rule2620? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2628? () ()
  (:and "FUNCTION" (:rule rule2622?) (:? "RETURNS") (:rule rule2623?)
   (:?
    (:and "(" (:rule rule2624?) (:* (:? (:and "," (:rule rule2625?))))
     (:? (:rule whitespace?)) ")"))
   (:or "FORWARD"
    (:and (:? (:and "MAP" (:? "TO") (:rule rule2626?))) "IN" (:rule rule2627?))
    (:and "IN" "SUPER"))))

;; function-name
(meta-sexp:defrule rule2622? () ()
)

;; return-type
(meta-sexp:defrule rule2623? () ()
)

;; parameter
(meta-sexp:defrule rule2624? () ()
)

;; parameter
(meta-sexp:defrule rule2625? () ()
)

;; actual-name
(meta-sexp:defrule rule2626? () ()
)

;; proc-handle
(meta-sexp:defrule rule2627? () ()
)


;; Form no. 3
(meta-sexp:defrule rule2634? () ()
  (:and "FUNCTION" (:rule rule2629?) (:? "RETURNS") (:rule rule2630?)
   (:? "PRIVATE")
   (:?
    (:and "(" (:rule rule2631?) (:* (:? (:and "," (:rule rule2632?))))
     (:? (:rule whitespace?)) ")"))
   ":" (:rule rule2633?)))

;; function-name
(meta-sexp:defrule rule2629? () ()
)

;; return-type
(meta-sexp:defrule rule2630? () ()
)

;; parameter
(meta-sexp:defrule rule2631? () ()
)

;; parameter
(meta-sexp:defrule rule2632? () ()
)

;; function-body
(meta-sexp:defrule rule2633? () ()
)

;;; FORM statement
;; Form no. 1
(meta-sexp:defrule rule2638? () ()
  (:and "FORM" (:rule rule2635?)
   (:? (:and "EXCEPT" (:* (:rule rule2636?)) (:? (:rule whitespace?))))
   (:? (:rule rule2637?))))

;; record
(meta-sexp:defrule rule2635? () ()
)

;; field
(meta-sexp:defrule rule2636? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2637? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2642? () ()
  (:and "FORM" (:? (:and (:* (:rule rule2639?)) (:? (:rule whitespace?))))
   (:?
    (:and (:or "HEADER" "BACKGROUND") (:* (:rule rule2640?))
     (:? (:rule whitespace?))))
   (:? (:rule rule2641?))))

;; form-item
(meta-sexp:defrule rule2639? () ()
)

;; head-item
(meta-sexp:defrule rule2640? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2641? () ()
)

;;; FIX-CODEPAGE statement
;; Form no. 1
(meta-sexp:defrule rule2645? () ()
  (:and "FIX-CODEPAGE" "(" (:rule rule2643?) ")" "=" (:rule rule2644?)))

;; longchar
(meta-sexp:defrule rule2643? () ()
)

;; codepage
(meta-sexp:defrule rule2644? () ()
)

;;; EXPORT statement
;; Form no. 1
(meta-sexp:defrule rule2649? () ()
  (:and "EXPORT" (:? (:and "STREAM" (:rule rule2646?)))
   (:or (:rule rule2647?) (:rule rule2648?))))

;; stream
(meta-sexp:defrule rule2646? () ()
)

;; memptr
(meta-sexp:defrule rule2647? () ()
)

;; longchar
(meta-sexp:defrule rule2648? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2655? () ()
  (:and "EXPORT" (:? (:and "STREAM" (:rule rule2650?)))
   (:? (:and "DELIMITER" (:rule rule2651?)))
   (:or (:and (:* (:rule rule2652?)) (:? (:rule whitespace?)))
    (:and (:rule rule2653?)
     (:? (:and "EXCEPT" (:* (:rule rule2654?)) (:? (:rule whitespace?))))))
   (:? "NO-LOBS")))

;; stream
(meta-sexp:defrule rule2650? () ()
)

;; character
(meta-sexp:defrule rule2651? () ()
)

;; expression
(meta-sexp:defrule rule2652? () ()
)

;; record
(meta-sexp:defrule rule2653? () ()
)

;; field
(meta-sexp:defrule rule2654? () ()
)

;;; END statement
;; Form no. 1
(meta-sexp:defrule rule2656? () ()
  (:and "END"
   (:?
    (:or "CASE" "CLASS" "CONSTRUCTOR" "DESTRUCTOR" "FUNCTION" "GET" "INTERFACE"
     "METHOD" "PROCEDURE" "SET" "TRIGGERS"))))

;;; EMPTY TEMP-TABLE statement
;; Form no. 1
(meta-sexp:defrule rule2658? () ()
  (:and "EMPTY" "TEMP-TABLE" (:rule rule2657?) (:? "NO-ERROR")))

;; temp-table-name
(meta-sexp:defrule rule2657? () ()
)

;;; DOWN statement
;; Form no. 1
(meta-sexp:defrule rule2662? () ()
  (:and "DOWN" (:? (:and "STREAM" (:rule rule2659?))) (:? (:rule rule2660?))
   (:? (:rule rule2661?))))

;; stream
(meta-sexp:defrule rule2659? () ()
)

;; expression
(meta-sexp:defrule rule2660? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2661? () ()
)

;;; DOS statement
;; Form no. 1
(meta-sexp:defrule rule2665? () ()
  (:and "DOS" (:? "SILENT")
   (:* (:? (:or (:rule rule2663?) (:and "VALUE" "(" (:rule rule2664?) ")"))))
   (:? (:rule whitespace?))))

;; command-token
(meta-sexp:defrule rule2663? () ()
)

;; expression
(meta-sexp:defrule rule2664? () ()
)

;;; DO statement
;; Form no. 1
(meta-sexp:defrule rule2681? () ()
  (:and (:? (:and (:rule rule2666?) ":")) "DO"
   (:?
    (:and "FOR" (:rule rule2667?) (:* (:? (:and "," (:rule rule2668?))))
     (:? (:rule whitespace?))))
   (:? (:rule rule2669?)) (:? (:rule rule2670?))
   (:?
    (:and (:rule rule2671?) "=" (:rule rule2672?) "TO" (:rule rule2673?)
     (:? (:and "BY" (:rule rule2674?)))))
   (:? (:and "WHILE" (:rule rule2675?))) (:? "TRANSACTION")
   (:? (:rule rule2676?)) (:? (:rule rule2677?)) (:? (:rule rule2678?))
   (:? (:rule rule2679?)) (:? (:rule rule2680?))))

;; label
(meta-sexp:defrule rule2666? () ()
)

;; record
(meta-sexp:defrule rule2667? () ()
)

;; record
(meta-sexp:defrule rule2668? () ()
)

;; preselect-phrase
(meta-sexp:defrule rule2669? () ()
)

;; query-tuning-phrase
(meta-sexp:defrule rule2670? () ()
)

;; variable
(meta-sexp:defrule rule2671? () ()
)

;; expression1
(meta-sexp:defrule rule2672? () ()
)

;; expression2
(meta-sexp:defrule rule2673? () ()
)

;; k
(meta-sexp:defrule rule2674? () ()
)

;; expression
(meta-sexp:defrule rule2675? () ()
)

;; on-endkey-phrase
(meta-sexp:defrule rule2676? () ()
)

;; on-error-phrase
(meta-sexp:defrule rule2677? () ()
)

;; on-quit-phrase
(meta-sexp:defrule rule2678? () ()
)

;; on-stop-phrase
(meta-sexp:defrule rule2679? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2680? () ()
)

;;; DISCONNECT statement
;; Form no. 1
(meta-sexp:defrule rule2684? () ()
  (:and "DISCONNECT"
   (:or (:rule rule2682?) (:and "VALUE" "(" (:rule rule2683?) ")"))
   (:? "NO-ERROR")))

;; logical-name
(meta-sexp:defrule rule2682? () ()
)

;; expression
(meta-sexp:defrule rule2683? () ()
)

;;; DISABLE TRIGGERS statement
;; Form no. 1
(meta-sexp:defrule rule2686? () ()
  (:and "DISABLE" "TRIGGERS" "FOR" (:or "DUMP" "LOAD") "OF" (:rule rule2685?)
   (:? "ALLOW-REPLICATION")))

;; table-name
(meta-sexp:defrule rule2685? () ()
)

;;; DISABLE statement
;; Form no. 1
(meta-sexp:defrule rule2691? () ()
  (:and "DISABLE" (:? "UNLESS-HIDDEN")
   (:or
    (:and "ALL"
     (:? (:and "EXCEPT" (:* (:rule rule2687?)) (:? (:rule whitespace?)))))
    (:and (:* (:and (:rule rule2688?) (:? (:and "WHEN" (:rule rule2689?)))))
     (:? (:rule whitespace?))))
   (:? (:rule rule2690?))))

;; field
(meta-sexp:defrule rule2687? () ()
)

;; field
(meta-sexp:defrule rule2688? () ()
)

;; expression
(meta-sexp:defrule rule2689? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2690? () ()
)

;;; DICTIONARY statement
;; Form no. 1
(meta-sexp:defrule rule2692? () ()
  "DICTIONARY")

;;; DESTRUCTOR statement
;; Form no. 1
(meta-sexp:defrule rule2695? () ()
  (:and "DESTRUCTOR" (:? "PUBLIC") (:rule rule2693?) "(" ")" ":"
   (:rule rule2694?)))

;; class-name
(meta-sexp:defrule rule2693? () ()
)

;; destructor-body
(meta-sexp:defrule rule2694? () ()
)

;;; DELETE WIDGET-POOL statement
;; Form no. 1
(meta-sexp:defrule rule2697? () ()
  (:and "DELETE" "WIDGET-POOL" (:? (:rule rule2696?)) (:? "NO-ERROR")))

;; pool-name
(meta-sexp:defrule rule2696? () ()
)

;;; DELETE WIDGET statement
;; Form no. 1
(meta-sexp:defrule rule2700? () ()
  (:and "DELETE" "WIDGET" (:rule rule2698?) (:* (:? (:rule rule2699?)))
   (:? (:rule whitespace?))))

;; handle
(meta-sexp:defrule rule2698? () ()
)

;; handle
(meta-sexp:defrule rule2699? () ()
)

;;; DELETE PROCEDURE statement
;; Form no. 1
(meta-sexp:defrule rule2702? () ()
  (:and "DELETE" "PROCEDURE" (:rule rule2701?) (:? "NO-ERROR")))

;; proc-handle
(meta-sexp:defrule rule2701? () ()
)

;;; DELETE OBJECT statement
;; Form no. 1
(meta-sexp:defrule rule2705? () ()
  (:and "DELETE" "OBJECT" (:or (:rule rule2703?) (:rule rule2704?))
   (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2703? () ()
)

;; object-reference
(meta-sexp:defrule rule2704? () ()
)

;;; DELETE ALIAS statement
;; Form no. 1
(meta-sexp:defrule rule2708? () ()
  (:and "DELETE" "ALIAS"
   (:or (:rule rule2706?) (:and "VALUE" "(" (:rule rule2707?) ")"))))

;; alias
(meta-sexp:defrule rule2706? () ()
)

;; expression
(meta-sexp:defrule rule2707? () ()
)

;;; DEFINE WORKFILE statement
;; Form no. 1
(meta-sexp:defrule rule2715? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) (:or "WORK-TABLE" "WORKFILE")
   (:rule rule2709?) (:? "NO-UNDO") (:? (:and "LIKE" (:rule rule2710?)))
   (:*
    (:?
     (:and "FIELD" (:rule rule2711?)
      (:or (:and "AS" (:rule rule2712?)) (:and "LIKE" (:rule rule2713?)))
      (:? (:rule rule2714?)))))
   (:? (:rule whitespace?))))

;; work-table-name
(meta-sexp:defrule rule2709? () ()
)

;; tablename
(meta-sexp:defrule rule2710? () ()
)

;; field-name
(meta-sexp:defrule rule2711? () ()
)

;; data-type
(meta-sexp:defrule rule2712? () ()
)

;; field
(meta-sexp:defrule rule2713? () ()
)

;; field-options
(meta-sexp:defrule rule2714? () ()
)

;;; DEFINE WORK-TABLE statement
;; Form no. 1
(meta-sexp:defrule rule2722? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) (:? "PRIVATE")
   (:or "WORK-TABLE" "WORKFILE") (:rule rule2716?) (:? "NO-UNDO")
   (:? (:and "LIKE" (:rule rule2717?) (:? "VALIDATE")))
   (:*
    (:?
     (:and "FIELD" (:rule rule2718?)
      (:or (:and "AS" (:rule rule2719?)) (:and "LIKE" (:rule rule2720?)))
      (:? (:rule rule2721?)))))
   (:? (:rule whitespace?))))

;; work-table-name
(meta-sexp:defrule rule2716? () ()
)

;; tablename
(meta-sexp:defrule rule2717? () ()
)

;; field-name
(meta-sexp:defrule rule2718? () ()
)

;; data-type
(meta-sexp:defrule rule2719? () ()
)

;; field
(meta-sexp:defrule rule2720? () ()
)

;; field-options
(meta-sexp:defrule rule2721? () ()
)

;;; DEFINE VARIABLE statement
;; Form no. 1
(meta-sexp:defrule rule2745? () ()
  (:and "DEFINE"
   (:or (:? (:and (:? (:and "NEW" (:? "GLOBAL"))) "SHARED"))
    (:? (:or "PRIVATE" "PROTECTED" "PUBLIC")))
   "VARIABLE" (:rule rule2723?)
   (:or (:and "AS" (:rule rule2724?))
    (:and "AS" (:? "CLASS") (:rule rule2725?)) (:and "LIKE" (:rule rule2726?)))
   (:? (:and "BGCOLOR" (:rule rule2727?)))
   (:? (:and "COLUMN-LABEL" (:rule rule2728?)))
   (:? (:and "CONTEXT-HELP-ID" (:rule rule2729?)))
   (:? (:and "DCOLOR" (:rule rule2730?)))
   (:? (:and "DECIMALS" (:rule rule2731?))) (:? "DROP-TARGET")
   (:? (:and "EXTENT" (:? (:rule rule2732?))))
   (:? (:and "FONT" (:rule rule2733?))) (:? (:and "FGCOLOR" (:rule rule2734?)))
   (:? (:and "FORMAT" (:rule rule2735?)))
   (:?
    (:and "INITIAL"
     (:or (:rule rule2736?)
      (:?
       (:and (:rule rule2737?) (:* (:? (:and "," (:rule rule2738?))))
        (:? (:rule whitespace?)))))))
   (:?
    (:and "LABEL" (:rule rule2739?) (:* (:? (:and "," (:rule rule2740?))))
     (:? (:rule whitespace?))))
   (:? (:and "MOUSE-POINTER" (:rule rule2741?))) (:? "NO-UNDO")
   (:? (:and (:? "NOT") "CASE-SENSITIVE"))
   (:? (:and "PFCOLOR" (:rule rule2742?))) (:? (:rule rule2743?))
   (:? (:rule rule2744?))))

;; variable-name
(meta-sexp:defrule rule2723? () ()
)

;; datatype
(meta-sexp:defrule rule2724? () ()
)

;; type-name
(meta-sexp:defrule rule2725? () ()
)

;; field
(meta-sexp:defrule rule2726? () ()
)

;; expression
(meta-sexp:defrule rule2727? () ()
)

;; label
(meta-sexp:defrule rule2728? () ()
)

;; expression
(meta-sexp:defrule rule2729? () ()
)

;; expression
(meta-sexp:defrule rule2730? () ()
)

;; n
(meta-sexp:defrule rule2731? () ()
)

;; expression
(meta-sexp:defrule rule2732? () ()
)

;; expression
(meta-sexp:defrule rule2733? () ()
)

;; expression
(meta-sexp:defrule rule2734? () ()
)

;; string
(meta-sexp:defrule rule2735? () ()
)

;; constant
(meta-sexp:defrule rule2736? () ()
)

;; constant
(meta-sexp:defrule rule2737? () ()
)

;; constant
(meta-sexp:defrule rule2738? () ()
)

;; string
(meta-sexp:defrule rule2739? () ()
)

;; string
(meta-sexp:defrule rule2740? () ()
)

;; expression
(meta-sexp:defrule rule2741? () ()
)

;; expression
(meta-sexp:defrule rule2742? () ()
)

;; view-as-phrase
(meta-sexp:defrule rule2743? () ()
)

;; trigger-phrase
(meta-sexp:defrule rule2744? () ()
)

;;; DEFINE TEMP-TABLE statement
;; Form no. 1
(meta-sexp:defrule rule2758? () ()
  (:and "DEFINE" (:? (:and (:? (:and "NEW" (:? "GLOBAL"))) "SHARED"))
   (:? (:or "PRIVATE" "PROTECTED")) "TEMP-TABLE" (:rule rule2746?)
   (:? "NO-UNDO") (:? (:and "NAMESPACE-URI" (:rule rule2747?)))
   (:? (:and "NAMESPACE-PREFIX" (:rule rule2748?))) (:? "REFERENCE-ONLY")
   (:?
    (:and "LIKE" (:rule rule2749?) (:? "VALIDATE")
     (:* (:? (:and "USE-INDEX" (:rule rule2750?) (:? (:and "AS" "PRIMARY")))))
     (:? (:rule whitespace?))))
   (:? "RCODE-INFORMATION") (:? (:and "BEFORE-TABLE" (:rule rule2751?)))
   (:*
    (:?
     (:and "FIELD" (:rule rule2752?)
      (:or (:and "AS" (:rule rule2753?))
       (:and "LIKE" (:rule rule2754?) (:? "VALIDATE")))
      (:? (:rule rule2755?)))))
   (:? (:rule whitespace?))
   (:*
    (:?
     (:and "INDEX" (:rule rule2756?)
      (:? (:and "IS" (:? "UNIQUE") (:? "PRIMARY") (:? "WORD-INDEX")))
      (:* (:and (:rule rule2757?) (:? (:or "ASCENDING" "DESCENDING"))))
      (:? (:rule whitespace?)))))
   (:? (:rule whitespace?))))

;; temp-table-name
(meta-sexp:defrule rule2746? () ()
)

;; namespace
(meta-sexp:defrule rule2747? () ()
)

;; prefix
(meta-sexp:defrule rule2748? () ()
)

;; table-name
(meta-sexp:defrule rule2749? () ()
)

;; index-name
(meta-sexp:defrule rule2750? () ()
)

;; before-table-name
(meta-sexp:defrule rule2751? () ()
)

;; field-name
(meta-sexp:defrule rule2752? () ()
)

;; data-type
(meta-sexp:defrule rule2753? () ()
)

;; field
(meta-sexp:defrule rule2754? () ()
)

;; field-options
(meta-sexp:defrule rule2755? () ()
)

;; index-name
(meta-sexp:defrule rule2756? () ()
)

;; index-field
(meta-sexp:defrule rule2757? () ()
)

;;; DEFINE SUB-MENU statement
;; Form no. 1
(meta-sexp:defrule rule2767? () ()
  (:and "DEFINE" (:? "PRIVATE") "SUB-MENU" (:rule rule2759?)
   (:? (:and "BGCOLOR" (:rule rule2760?)))
   (:? (:and "DCOLOR" (:rule rule2761?)))
   (:? (:and "FGCOLOR" (:rule rule2762?)))
   (:? (:and "PFCOLOR" (:rule rule2763?))) (:? (:and "FONT" (:rule rule2764?)))
   (:? "SUB-MENU-HELP")
   (:or (:and "LIKE" (:rule rule2765?))
    (:and (:* (:rule rule2766?)) (:? (:rule whitespace?))))))

;; submenu
(meta-sexp:defrule rule2759? () ()
)

;; expression
(meta-sexp:defrule rule2760? () ()
)

;; expression
(meta-sexp:defrule rule2761? () ()
)

;; expression
(meta-sexp:defrule rule2762? () ()
)

;; expression
(meta-sexp:defrule rule2763? () ()
)

;; number
(meta-sexp:defrule rule2764? () ()
)

;; menu
(meta-sexp:defrule rule2765? () ()
)

;; menu-element-descriptor
(meta-sexp:defrule rule2766? () ()
)

;;; DEFINE STREAM statement
;; Form no. 1
(meta-sexp:defrule rule2769? () ()
  (:and "DEFINE" (:? (:and (:? (:and "NEW" (:? "GLOBAL"))) "SHARED"))
   (:? "PRIVATE") "STREAM" (:rule rule2768?)))

;; stream
(meta-sexp:defrule rule2768? () ()
)

;;; DEFINE RECTANGLE statement
;; Form no. 1
(meta-sexp:defrule rule2781? () ()
  (:and "DEFINE" (:? "PRIVATE") "RECTANGLE" (:rule rule2770?)
   (:? (:and "LIKE" (:rule rule2771?))) (:? "NO-FILL")
   (:?
    (:or (:and "EDGE-CHARS" (:rule rule2772?))
     (:and "EDGE-PIXELS" (:rule rule2773?))))
   (:? (:and "DCOLOR" (:rule rule2774?)))
   (:? (:and "BGCOLOR" (:rule rule2775?)))
   (:? (:and "FGCOLOR" (:rule rule2776?))) (:? "GRAPHIC-EDGE")
   (:? (:and "PFCOLOR" (:rule rule2777?))) (:? "ROUNDED") (:? "GROUP-BOX")
   (:? (:rule rule2778?)) (:? (:and "TOOLTIP" (:rule rule2779?)))
   (:? (:rule rule2780?))))

;; rectangle
(meta-sexp:defrule rule2770? () ()
)

;; rectangle2
(meta-sexp:defrule rule2771? () ()
)

;; width
(meta-sexp:defrule rule2772? () ()
)

;; width
(meta-sexp:defrule rule2773? () ()
)

;; expression
(meta-sexp:defrule rule2774? () ()
)

;; expression
(meta-sexp:defrule rule2775? () ()
)

;; expression
(meta-sexp:defrule rule2776? () ()
)

;; expression
(meta-sexp:defrule rule2777? () ()
)

;; size-phrase
(meta-sexp:defrule rule2778? () ()
)

;; tooltip
(meta-sexp:defrule rule2779? () ()
)

;; trigger-phrase
(meta-sexp:defrule rule2780? () ()
)

;;; DEFINE QUERY statement
;; Form no. 1
(meta-sexp:defrule rule2788? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED"))
   (:? (:or "PRIVATE" "PROTECTED")) "QUERY" (:rule rule2782?) "FOR"
   (:rule rule2783?) (:? (:rule rule2784?))
   (:* (:? (:and "," (:rule rule2785?) (:? (:rule rule2786?)))))
   (:? (:rule whitespace?)) (:? (:and "CACHE" (:rule rule2787?)))
   (:? "SCROLLING") (:? "RCODE-INFORMATION")))

;; query
(meta-sexp:defrule rule2782? () ()
)

;; bufname
(meta-sexp:defrule rule2783? () ()
)

;; field-list
(meta-sexp:defrule rule2784? () ()
)

;; bufname
(meta-sexp:defrule rule2785? () ()
)

;; field-list
(meta-sexp:defrule rule2786? () ()
)

;; n
(meta-sexp:defrule rule2787? () ()
)

;;; DEFINE PROPERTY statement
;; Form no. 1
(meta-sexp:defrule rule2794? () ()
  (:and "DEFINE" (:? (:or "PRIVATE" "PROTECTED" "PUBLIC")) "PROPERTY"
   (:rule rule2789?)
   (:or (:and "AS" (:rule rule2790?) (:? (:and "INITIAL" (:rule rule2791?))))
    (:and "AS" (:? "CLASS") (:rule rule2792?)))
   (:? "NO-UNDO") "SET" (:? (:rule rule2793?))))

;; property-name
(meta-sexp:defrule rule2789? () ()
)

;; datatype
(meta-sexp:defrule rule2790? () ()
)

;; constant
(meta-sexp:defrule rule2791? () ()
)

;; type-name
(meta-sexp:defrule rule2792? () ()
)

;; implementation
(meta-sexp:defrule rule2793? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2800? () ()
  (:and "DEFINE" (:? (:or "PRIVATE" "PROTECTED" "PUBLIC")) "PROPERTY"
   (:rule rule2795?)
   (:or (:and "AS" (:rule rule2796?) (:? (:and "INITIAL" (:rule rule2797?))))
    (:and "AS" (:? "CLASS") (:rule rule2798?)))
   (:? "NO-UNDO") "GET" (:? (:rule rule2799?))))

;; property-name
(meta-sexp:defrule rule2795? () ()
)

;; datatype
(meta-sexp:defrule rule2796? () ()
)

;; constant
(meta-sexp:defrule rule2797? () ()
)

;; type-name
(meta-sexp:defrule rule2798? () ()
)

;; implementation
(meta-sexp:defrule rule2799? () ()
)


;; Form no. 3
(meta-sexp:defrule rule2805? () ()
  (:and "DEFINE" (:? (:or "PRIVATE" "PROTECTED" "PUBLIC")) "PROPERTY"
   (:rule rule2801?)
   (:or (:and "AS" (:rule rule2802?) (:? (:and "INITIAL" (:rule rule2803?))))
    (:and "AS" (:? "CLASS") (:rule rule2804?)))
   (:? "NO-UNDO")))

;; property-name
(meta-sexp:defrule rule2801? () ()
)

;; datatype
(meta-sexp:defrule rule2802? () ()
)

;; constant
(meta-sexp:defrule rule2803? () ()
)

;; type-name
(meta-sexp:defrule rule2804? () ()
)

;;; DEFINE PARAMETER statement
;; Form no. 1
(meta-sexp:defrule rule2810? () ()
  (:and "DEFINE" (:or "INPUT" "OUTPUT" "INPUT-OUTPUT") "PARAMETER"
   (:or (:and "TABLE" "FOR" (:rule rule2806?) (:? "APPEND") (:? "BIND"))
    (:and "TABLE-HANDLE" (:rule rule2807?) (:? "APPEND") (:? "BIND"))
    (:and "DATASET" "FOR" (:rule rule2808?) (:? "APPEND") (:? "BIND"))
    (:and "DATASET-HANDLE" (:rule rule2809?) (:? "APPEND") (:? "BIND")))))

;; temp-table-name
(meta-sexp:defrule rule2806? () ()
)

;; temp-table-handle
(meta-sexp:defrule rule2807? () ()
)

;; dataset-name
(meta-sexp:defrule rule2808? () ()
)

;; dataset-handle
(meta-sexp:defrule rule2809? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2814? () ()
  (:and "DEFINE" "PARAMETER" "BUFFER" (:rule rule2811?) "FOR" (:rule rule2812?)
   (:? "PRESELECT") (:? (:and "LABEL" (:rule rule2813?)))))

;; buffer
(meta-sexp:defrule rule2811? () ()
)

;; table
(meta-sexp:defrule rule2812? () ()
)

;; label
(meta-sexp:defrule rule2813? () ()
)


;; Form no. 3
(meta-sexp:defrule rule2825? () ()
  (:and "DEFINE" (:or "INPUT" "OUTPUT" "INPUT-OUTPUT" "RETURN") "PARAMETER"
   (:rule rule2815?)
   (:or (:and "AS" (:? (:and "HANDLE" "TO")) (:rule rule2816?))
    (:and "AS" (:? "CLASS") (:rule rule2817?)) (:and "LIKE" (:rule rule2818?)))
   (:? (:and "EXTENT" (:? (:rule rule2819?))))
   (:? (:and (:? "NOT") "CASE-SENSITIVE"))
   (:? (:and "FORMAT" (:rule rule2820?)))
   (:? (:and "DECIMALS" (:rule rule2821?)))
   (:? (:and "INITIAL" (:rule rule2822?)))
   (:? (:and "COLUMN-LABEL" (:rule rule2823?)))
   (:? (:and "LABEL" (:rule rule2824?))) (:? "NO-UNDO")))

;; parameter
(meta-sexp:defrule rule2815? () ()
)

;; datatype
(meta-sexp:defrule rule2816? () ()
)

;; type-name
(meta-sexp:defrule rule2817? () ()
)

;; field
(meta-sexp:defrule rule2818? () ()
)

;; expression
(meta-sexp:defrule rule2819? () ()
)

;; string
(meta-sexp:defrule rule2820? () ()
)

;; n
(meta-sexp:defrule rule2821? () ()
)

;; constant
(meta-sexp:defrule rule2822? () ()
)

;; label
(meta-sexp:defrule rule2823? () ()
)

;; string
(meta-sexp:defrule rule2824? () ()
)

;;; DEFINE MENU statement
;; Form no. 1
(meta-sexp:defrule rule2835? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) (:? "PRIVATE") "MENU"
   (:rule rule2826?) (:? (:and "FGCOLOR" (:rule rule2827?)))
   (:? (:and "BGCOLOR" (:rule rule2828?)))
   (:? (:and "DCOLOR" (:rule rule2829?)))
   (:? (:and "PFCOLOR" (:rule rule2830?))) (:? (:and "FONT" (:rule rule2831?)))
   (:? (:or (:and "TITLE" (:rule rule2832?)) "MENUBAR"))
   (:?
    (:or (:and "LIKE" (:rule rule2833?))
     (:and (:* (:rule rule2834?)) (:? (:rule whitespace?)))))))

;; menu-name
(meta-sexp:defrule rule2826? () ()
)

;; expression
(meta-sexp:defrule rule2827? () ()
)

;; expression
(meta-sexp:defrule rule2828? () ()
)

;; expression
(meta-sexp:defrule rule2829? () ()
)

;; expression
(meta-sexp:defrule rule2830? () ()
)

;; number
(meta-sexp:defrule rule2831? () ()
)

;; title
(meta-sexp:defrule rule2832? () ()
)

;; menu
(meta-sexp:defrule rule2833? () ()
)

;; menu-element-descriptor
(meta-sexp:defrule rule2834? () ()
)

;;; DEFINE IMAGE statement
;; Form no. 1
(meta-sexp:defrule rule2843? () ()
  (:and "DEFINE" (:? "PRIVATE") "IMAGE" (:rule rule2836?)
   (:or (:rule rule2837?) (:and "LIKE" (:rule rule2838?)) (:rule rule2839?))
   (:? (:and "BGCOLOR" (:rule rule2840?)))
   (:? (:and "FGCOLOR" (:rule rule2841?))) (:? "CONVERT-3D-COLORS")
   (:? (:and "TOOLTIP" (:rule rule2842?)))
   (:? (:and "STRETCH-TO-FIT" (:? "RETAIN-SHAPE"))) (:? "TRANSPARENT")))

;; image-name
(meta-sexp:defrule rule2836? () ()
)

;; image-phrase
(meta-sexp:defrule rule2837? () ()
)

;; image
(meta-sexp:defrule rule2838? () ()
)

;; size-phrase
(meta-sexp:defrule rule2839? () ()
)

;; expression
(meta-sexp:defrule rule2840? () ()
)

;; expression
(meta-sexp:defrule rule2841? () ()
)

;; tooltip
(meta-sexp:defrule rule2842? () ()
)

;;; DEFINE FRAME statement
;; Form no. 1
(meta-sexp:defrule rule2848? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) "FRAME" (:rule rule2844?)
   (:rule rule2845?)
   (:? (:and "EXCEPT" (:* (:rule rule2846?)) (:? (:rule whitespace?))))
   (:? (:rule rule2847?))))

;; frame
(meta-sexp:defrule rule2844? () ()
)

;; record
(meta-sexp:defrule rule2845? () ()
)

;; field
(meta-sexp:defrule rule2846? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2847? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2853? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) (:? "PRIVATE") "FRAME"
   (:rule rule2849?)
   (:? (:and (:* (:rule rule2850?)) (:? (:rule whitespace?))))
   (:?
    (:and (:or "HEADER" "BACKGROUND") (:* (:rule rule2851?))
     (:? (:rule whitespace?))))
   (:? (:rule rule2852?))))

;; frame
(meta-sexp:defrule rule2849? () ()
)

;; form-item
(meta-sexp:defrule rule2850? () ()
)

;; head-item
(meta-sexp:defrule rule2851? () ()
)

;; frame-phrase
(meta-sexp:defrule rule2852? () ()
)

;;; DEFINE DATA-SOURCE statement
;; Form no. 1
(meta-sexp:defrule rule2858? () ()
  (:and "DEFINE" (:? (:or "PRIVATE" "PROTECTED")) "DATA-SOURCE"
   (:rule rule2854?) "FOR" (:? (:and "QUERY" (:rule rule2855?)))
   (:?
    (:and (:rule rule2856?) (:* (:? (:and "," (:rule rule2857?))))
     (:? (:rule whitespace?))))))

;; data-source-name
(meta-sexp:defrule rule2854? () ()
)

;; query-name
(meta-sexp:defrule rule2855? () ()
)

;; source-buffer-phrase
(meta-sexp:defrule rule2856? () ()
)

;; source-buffer-phrase
(meta-sexp:defrule rule2857? () ()
)

;;; DEFINE DATASET statement
;; Form no. 1
(meta-sexp:defrule rule2868? () ()
  (:and "DEFINE" (:? (:and "NEW" (:? "SHARED")))
   (:? (:or "PRIVATE" "PROTECTED")) "DATASET" (:rule rule2859?)
   (:? (:and "NAMESPACE-URI" (:rule rule2860?)))
   (:? (:and "NAMESPACE-PREFIX" (:rule rule2861?))) (:? "REFERENCE-ONLY") "FOR"
   (:rule rule2862?) (:* (:? (:and "," (:rule rule2863?))))
   (:? (:rule whitespace?)) "DATA-RELATION" (:? (:rule rule2864?)) "FOR"
   (:rule rule2865?)
   (:* (:? (:and "DATA-RELATION" (:? (:rule rule2866?)) (:rule rule2867?))))
   (:? (:rule whitespace?))))

;; dataset-name
(meta-sexp:defrule rule2859? () ()
)

;; namespace
(meta-sexp:defrule rule2860? () ()
)

;; prefix
(meta-sexp:defrule rule2861? () ()
)

;; buffer-name
(meta-sexp:defrule rule2862? () ()
)

;; buffer-name
(meta-sexp:defrule rule2863? () ()
)

;; data-rel-name
(meta-sexp:defrule rule2864? () ()
)

;; data-rel-spec
(meta-sexp:defrule rule2865? () ()
)

;; data-rel-name
(meta-sexp:defrule rule2866? () ()
)

;; data-rel-spec
(meta-sexp:defrule rule2867? () ()
)

;;; DEFINE BUTTON statement
;; Form no. 1
(meta-sexp:defrule rule2885? () ()
  (:and "DEFINE" (:? "PRIVATE") "BUTTON" (:rule rule2869?)
   (:? (:or "AUTO-GO" "AUTO-ENDKEY")) (:? "DEFAULT")
   (:? (:and "BGCOLOR" (:rule rule2870?)))
   (:? (:and "CONTEXT-HELP-ID" (:rule rule2871?)))
   (:? (:and "DCOLOR" (:rule rule2872?))) (:? "DROP-TARGET")
   (:? (:and "FGCOLOR" (:rule rule2873?))) (:? (:and "FONT" (:rule rule2874?)))
   (:? (:and "IMAGE-DOWN" (:rule rule2875?)))
   (:? (:and (:or "IMAGE" "IMAGE-UP") (:rule rule2876?)))
   (:? (:and "IMAGE-INSENSITIVE" (:rule rule2877?)))
   (:? (:and "MOUSE-POINTER" (:rule rule2878?)))
   (:? (:and "LABEL" (:rule rule2879?))) (:? (:and "LIKE" (:rule rule2880?)))
   (:? (:and "PFCOLOR" (:rule rule2881?))) (:? (:rule rule2882?))
   (:? (:and "NO-FOCUS" (:? "FLAT-BUTTON"))) (:? "NO-CONVERT-3D-COLORS")
   (:? (:and "TOOLTIP" (:rule rule2883?))) (:? (:rule rule2884?))))

;; button
(meta-sexp:defrule rule2869? () ()
)

;; expression
(meta-sexp:defrule rule2870? () ()
)

;; expression
(meta-sexp:defrule rule2871? () ()
)

;; expression
(meta-sexp:defrule rule2872? () ()
)

;; expression
(meta-sexp:defrule rule2873? () ()
)

;; number
(meta-sexp:defrule rule2874? () ()
)

;; image-phrase
(meta-sexp:defrule rule2875? () ()
)

;; image-phrase
(meta-sexp:defrule rule2876? () ()
)

;; image-phrase
(meta-sexp:defrule rule2877? () ()
)

;; name
(meta-sexp:defrule rule2878? () ()
)

;; label
(meta-sexp:defrule rule2879? () ()
)

;; button
(meta-sexp:defrule rule2880? () ()
)

;; expression
(meta-sexp:defrule rule2881? () ()
)

;; size-phrase
(meta-sexp:defrule rule2882? () ()
)

;; tooltip
(meta-sexp:defrule rule2883? () ()
)

;; trigger-phrase
(meta-sexp:defrule rule2884? () ()
)

;;; DEFINE BUFFER statement
;; Form no. 1
(meta-sexp:defrule rule2891? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED"))
   (:? (:or "PRIVATE" "PROTECTED")) "BUFFER" (:rule rule2886?) "FOR"
   (:? "TEMP-TABLE") (:rule rule2887?) (:? "PRESELECT")
   (:? (:and "LABEL" (:rule rule2888?)))
   (:? (:and "NAMESPACE-URI" (:rule rule2889?)))
   (:? (:and "NAMESPACE-PREFIX" (:rule rule2890?)))))

;; buffer-name
(meta-sexp:defrule rule2886? () ()
)

;; table-name
(meta-sexp:defrule rule2887? () ()
)

;; label-name
(meta-sexp:defrule rule2888? () ()
)

;; namespace
(meta-sexp:defrule rule2889? () ()
)

;; prefix
(meta-sexp:defrule rule2890? () ()
)

;;; DEFINE BROWSE statement
;; Form no. 1
(meta-sexp:defrule rule2901? () ()
  (:and "DEFINE" (:? (:and (:? "NEW") "SHARED")) (:? "PRIVATE") "BROWSE"
   (:rule rule2892?) "QUERY" (:rule rule2893?)
   (:? (:or "SHARE-LOCK" "EXCLUSIVE-LOCK" "NO-LOCK")) (:? "NO-WAIT") "DISPLAY"
   (:or (:rule rule2894?)
    (:and (:rule rule2895?)
     (:? (:and "EXCEPT" (:* (:rule rule2896?)) (:? (:rule whitespace?))))))
   (:? (:rule rule2897?)) (:rule rule2898?)
   (:? (:and "CONTEXT-HELP-ID" (:rule rule2899?))) (:? "DROP-TARGET")
   (:? (:and "TOOLTIP" (:rule rule2900?)))))

;; browse-name
(meta-sexp:defrule rule2892? () ()
)

;; query-name
(meta-sexp:defrule rule2893? () ()
)

;; column-list
(meta-sexp:defrule rule2894? () ()
)

;; record
(meta-sexp:defrule rule2895? () ()
)

;; field
(meta-sexp:defrule rule2896? () ()
)

;; browse-enable-phrase
(meta-sexp:defrule rule2897? () ()
)

;; browse-options-phrase
(meta-sexp:defrule rule2898? () ()
)

;; expression
(meta-sexp:defrule rule2899? () ()
)

;; tooltip
(meta-sexp:defrule rule2900? () ()
)

;;; DDE TERMINATE statement
;; Form no. 1
(meta-sexp:defrule rule2903? () ()
  (:and "DDE" "TERMINATE" (:rule rule2902?) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2902? () ()
)

;;; DDE SEND statement
;; Form no. 1
(meta-sexp:defrule rule2908? () ()
  (:and "DDE" "SEND" (:rule rule2904?) "SOURCE" (:rule rule2905?) "ITEM"
   (:rule rule2906?) (:? (:and "TIME" (:rule rule2907?))) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2904? () ()
)

;; data
(meta-sexp:defrule rule2905? () ()
)

;; name
(meta-sexp:defrule rule2906? () ()
)

;; seconds
(meta-sexp:defrule rule2907? () ()
)

;;; DDE REQUEST statement
;; Form no. 1
(meta-sexp:defrule rule2913? () ()
  (:and "DDE" "REQUEST" (:rule rule2909?) "TARGET" (:rule rule2910?) "ITEM"
   (:rule rule2911?) (:? (:and "TIME" (:rule rule2912?))) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2909? () ()
)

;; field
(meta-sexp:defrule rule2910? () ()
)

;; name
(meta-sexp:defrule rule2911? () ()
)

;; seconds
(meta-sexp:defrule rule2912? () ()
)

;;; DDE INITIATE statement
;; Form no. 1
(meta-sexp:defrule rule2918? () ()
  (:and "DDE" "INITIATE" (:rule rule2914?) "FRAME" (:rule rule2915?)
   "APPLICATION" (:rule rule2916?) "TOPIC" (:rule rule2917?) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2914? () ()
)

;; frame-handle
(meta-sexp:defrule rule2915? () ()
)

;; server-name
(meta-sexp:defrule rule2916? () ()
)

;; topic-name
(meta-sexp:defrule rule2917? () ()
)

;;; DDE GET statement
;; Form no. 1
(meta-sexp:defrule rule2923? () ()
  (:and "DDE" "GET" (:rule rule2919?) "TARGET" (:rule rule2920?) "ITEM"
   (:rule rule2921?) (:? (:and "TIME" (:rule rule2922?))) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2919? () ()
)

;; field
(meta-sexp:defrule rule2920? () ()
)

;; name
(meta-sexp:defrule rule2921? () ()
)

;; seconds
(meta-sexp:defrule rule2922? () ()
)

;;; DDE EXECUTE statement
;; Form no. 1
(meta-sexp:defrule rule2927? () ()
  (:and "DDE" "EXECUTE" (:rule rule2924?) "COMMAND" (:rule rule2925?)
   (:? (:and "TIME" (:rule rule2926?))) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2924? () ()
)

;; string
(meta-sexp:defrule rule2925? () ()
)

;; seconds
(meta-sexp:defrule rule2926? () ()
)

;;; DDE ADVISE statement
;; Form no. 1
(meta-sexp:defrule rule2931? () ()
  (:and "DDE" "ADVISE" (:rule rule2928?) (:or "START" "STOP") "ITEM"
   (:rule rule2929?) (:? (:and "TIME" (:rule rule2930?))) (:? "NO-ERROR")))

;; ddeid
(meta-sexp:defrule rule2928? () ()
)

;; name
(meta-sexp:defrule rule2929? () ()
)

;; seconds
(meta-sexp:defrule rule2930? () ()
)

;;; CREATE X-NODEREF statement
;; Form no. 1
(meta-sexp:defrule rule2934? () ()
  (:and "CREATE" "X-NODEREF" (:rule rule2932?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2933?)))))

;; handle
(meta-sexp:defrule rule2932? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2933? () ()
)

;;; CREATE X-DOCUMENT statement
;; Form no. 1
(meta-sexp:defrule rule2937? () ()
  (:and "CREATE" "X-DOCUMENT" (:rule rule2935?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2936?)))))

;; handle
(meta-sexp:defrule rule2935? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2936? () ()
)

;;; CREATE WIDGET-POOL statement
;; Form no. 1
(meta-sexp:defrule rule2939? () ()
  (:and "CREATE" "WIDGET-POOL" (:? (:and (:rule rule2938?) (:? "PERSISTENT")))
   (:? "NO-ERROR")))

;; pool-name
(meta-sexp:defrule rule2938? () ()
)

;;; CREATE widget statement
;; Form no. 1
(meta-sexp:defrule rule2946? () ()
  (:and "CREATE"
   (:or "BUTTON" "COMBO-BOX" "CONTROL-FRAME" "DIALOG-BOX" "EDITOR" "FILL-IN"
    "FRAME" "IMAGE" "MENU" "MENU-ITEM" "RADIO-SET" "RECTANGLE" "SELECTION-LIST"
    "SLIDER" "SUB-MENU" "TEXT" "TOGGLE-BOX" "WINDOW"
    (:and "VALUE" "(" (:rule rule2940?) ")"))
   (:rule rule2941?) (:? (:and "IN" "WIDGET-POOL" (:rule rule2942?)))
   (:?
    (:and "ASSIGN"
     (:*
      (:and (:rule rule2943?) (:? (:rule whitespace?)) "="
       (:? (:rule whitespace?)) (:rule rule2944?)))
     (:? (:rule whitespace?))))
   (:? (:rule rule2945?))))

;; string-expression
(meta-sexp:defrule rule2940? () ()
)

;; handle
(meta-sexp:defrule rule2941? () ()
)

;; pool-name
(meta-sexp:defrule rule2942? () ()
)

;; attribute
(meta-sexp:defrule rule2943? () ()
)

;; expression
(meta-sexp:defrule rule2944? () ()
)

;; trigger-phrase
(meta-sexp:defrule rule2945? () ()
)

;;; CREATE TEMP-TABLE statement
;; Form no. 1
(meta-sexp:defrule rule2949? () ()
  (:and "CREATE" "TEMP-TABLE" (:rule rule2947?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2948?)))))

;; handle
(meta-sexp:defrule rule2947? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2948? () ()
)

;;; CREATE SOCKET statement
;; Form no. 1
(meta-sexp:defrule rule2951? () ()
  (:and "CREATE" "SOCKET" (:rule rule2950?) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2950? () ()
)

;;; CREATE SOAP-HEADER-ENTRYREF statement
;; Form no. 1
(meta-sexp:defrule rule2954? () ()
  (:and "CREATE" "SOAP-HEADER-ENTRYREF" (:rule rule2952?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2953?)))))

;; hshEntry
(meta-sexp:defrule rule2952? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2953? () ()
)

;;; CREATE SOAP-HEADER statement
;; Form no. 1
(meta-sexp:defrule rule2957? () ()
  (:and "CREATE" "SOAP-HEADER" (:rule rule2955?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2956?)))))

;; handle
(meta-sexp:defrule rule2955? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2956? () ()
)

;;; CREATE SERVER-SOCKET statement
;; Form no. 1
(meta-sexp:defrule rule2959? () ()
  (:and "CREATE" "SERVER-SOCKET" (:rule rule2958?) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2958? () ()
)

;;; CREATE SERVER statement
;; Form no. 1
(meta-sexp:defrule rule2963? () ()
  (:and "CREATE" "SERVER" (:rule rule2960?)
   (:?
    (:and "ASSIGN"
     (:*
      (:and (:rule rule2961?) (:? (:rule whitespace?)) "="
       (:? (:rule whitespace?)) (:rule rule2962?)))
     (:? (:rule whitespace?))))))

;; handle
(meta-sexp:defrule rule2960? () ()
)

;; attribute
(meta-sexp:defrule rule2961? () ()
)

;; expression
(meta-sexp:defrule rule2962? () ()
)

;;; CREATE SAX-WRITER statement
;; Form no. 1
(meta-sexp:defrule rule2966? () ()
  (:and "CREATE" "SAX-WRITER" (:rule rule2964?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2965?))) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2964? () ()
)

;; pool-name
(meta-sexp:defrule rule2965? () ()
)

;;; CREATE SAX-READER statement
;; Form no. 1
(meta-sexp:defrule rule2969? () ()
  (:and "CREATE" "SAX-READER" (:rule rule2967?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2968?))) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2967? () ()
)

;; pool-name
(meta-sexp:defrule rule2968? () ()
)

;;; CREATE SAX-ATTRIBUTES statement
;; Form no. 1
(meta-sexp:defrule rule2972? () ()
  (:and "CREATE" "SAX-ATTRIBUTES" (:rule rule2970?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2971?))) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2970? () ()
)

;; pool-name
(meta-sexp:defrule rule2971? () ()
)

;;; CREATE QUERY statement
;; Form no. 1
(meta-sexp:defrule rule2975? () ()
  (:and "CREATE" "QUERY" (:rule rule2973?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2974?)))))

;; handle
(meta-sexp:defrule rule2973? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2974? () ()
)

;;; CREATE DATA-SOURCE statement
;; Form no. 1
(meta-sexp:defrule rule2978? () ()
  (:and "CREATE" "DATA-SOURCE" (:rule rule2976?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2977?)))))

;; data-source-handle
(meta-sexp:defrule rule2976? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2977? () ()
)

;;; CREATE DATASET statement
;; Form no. 1
(meta-sexp:defrule rule2981? () ()
  (:and "CREATE" "DATASET" (:rule rule2979?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2980?)))))

;; dataset-handle
(meta-sexp:defrule rule2979? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2980? () ()
)

;;; CREATE DATABASE statement
;; Form no. 1
(meta-sexp:defrule rule2984? () ()
  (:and "CREATE" "DATABASE" (:rule rule2982?)
   (:? (:and "FROM" (:rule rule2983?) (:? "NEW-INSTANCE"))) (:? "REPLACE")
   (:? "NO-ERROR")))

;; new-database
(meta-sexp:defrule rule2982? () ()
)

;; old-database
(meta-sexp:defrule rule2983? () ()
)

;;; CREATE CLIENT-PRINCIPAL statement
;; Form no. 1
(meta-sexp:defrule rule2986? () ()
  (:and "CREATE" "CLIENT-PRINCIPAL" (:rule rule2985?)))

;; client-principal-handle
(meta-sexp:defrule rule2985? () ()
)

;;; CREATE CALL statement
;; Form no. 1
(meta-sexp:defrule rule2989? () ()
  (:and "CREATE" "CALL" (:rule rule2987?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2988?))) (:? "NO-ERROR")))

;; handle
(meta-sexp:defrule rule2987? () ()
)

;; widget-pool
(meta-sexp:defrule rule2988? () ()
)

;;; CREATE BUFFER statement
;; Form no. 1
(meta-sexp:defrule rule2995? () ()
  (:and "CREATE" "BUFFER" (:rule rule2990?) "FOR" "TABLE"
   (:or (:rule rule2991?) (:rule rule2992?))
   (:? (:and "BUFFER-NAME" (:rule rule2993?)))
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2994?)))))

;; handle
(meta-sexp:defrule rule2990? () ()
)

;; table-name
(meta-sexp:defrule rule2991? () ()
)

;; buffer-handle
(meta-sexp:defrule rule2992? () ()
)

;; buffer-name
(meta-sexp:defrule rule2993? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2994? () ()
)

;;; CREATE BROWSE statement
;; Form no. 1
(meta-sexp:defrule rule3001? () ()
  (:and "CREATE" "BROWSE" (:rule rule2996?)
   (:? (:and "IN" "WIDGET-POOL" (:rule rule2997?)))
   (:?
    (:and "ASSIGN" (:* (:and (:rule rule2998?) "=" (:rule rule2999?)))
     (:? (:rule whitespace?))))
   (:? (:rule rule3000?))))

;; handle
(meta-sexp:defrule rule2996? () ()
)

;; widget-pool-name
(meta-sexp:defrule rule2997? () ()
)

;; attribute
(meta-sexp:defrule rule2998? () ()
)

;; expression
(meta-sexp:defrule rule2999? () ()
)

;; trigger-phrase
(meta-sexp:defrule rule3000? () ()
)

;;; CREATE automation object statement
;; Form no. 1
(meta-sexp:defrule rule3005? () ()
  (:and "CREATE" (:rule rule3002?) (:? (:rule whitespace?)) (:rule rule3003?)
   (:? (:and "CONNECT" (:? (:and "TO" (:rule rule3004?))))) (:? "NO-ERROR")))

;; expression1
(meta-sexp:defrule rule3002? () ()
)

;; COM-hdl-var
(meta-sexp:defrule rule3003? () ()
)

;; expression2
(meta-sexp:defrule rule3004? () ()
)

;;; CREATE ALIAS statement
;; Form no. 1
(meta-sexp:defrule rule3012? () ()
  (:or (:and "CREATE" "ALIAS" (:rule rule3006?))
   (:and (:rule rule3007?) "(" (:rule rule3008?) ")" "FOR" "DATABASE"
    (:rule rule3009?))
   (:and (:rule rule3010?) "(" (:rule rule3011?) ")" (:? "NO-ERROR"))))

;; alias-string
(meta-sexp:defrule rule3006? () ()
)

;; value
(meta-sexp:defrule rule3007? () ()
)

;; expression
(meta-sexp:defrule rule3008? () ()
)

;; logical-name-string
(meta-sexp:defrule rule3009? () ()
)

;; value
(meta-sexp:defrule rule3010? () ()
)

;; expression
(meta-sexp:defrule rule3011? () ()
)

;;; COPY-LOB statement
;; Form no. 1
(meta-sexp:defrule rule3021? () ()
  (:and "COPY-LOB" (:? "FROM")
   (:or (:and (:? "OBJECT") (:rule rule3013?)) (:and "FILE" (:rule rule3014?)))
   (:? (:and "STARTING" "AT" (:rule rule3015?)))
   (:? (:and "FOR" (:rule rule3016?))) "TO"
   (:or
    (:and (:? "OBJECT") (:rule rule3017?)
     (:? (:and "OVERLAY" "AT" (:rule rule3018?) (:? "TRIM"))))
    (:and "FILE" (:rule rule3019?) (:? "APPEND")))
   (:? (:or "NO-CONVERT" (:and "CONVERT" (:rule rule3020?)))) (:? "NO-ERROR")))

;; source-lob
(meta-sexp:defrule rule3013? () ()
)

;; source-filename
(meta-sexp:defrule rule3014? () ()
)

;; n
(meta-sexp:defrule rule3015? () ()
)

;; length
(meta-sexp:defrule rule3016? () ()
)

;; target-lob
(meta-sexp:defrule rule3017? () ()
)

;; n
(meta-sexp:defrule rule3018? () ()
)

;; target-filename
(meta-sexp:defrule rule3019? () ()
)

;; convert-phrase
(meta-sexp:defrule rule3020? () ()
)

;;; CONSTRUCTOR statement
;; Form no. 1
(meta-sexp:defrule rule3026? () ()
  (:and "CONSTRUCTOR" (:? (:or "PRIVATE" "PROTECTED" "PUBLIC"))
   (:rule rule3022?) "("
   (:?
    (:and (:rule rule3023?) (:* (:? (:and "," (:rule rule3024?))))
     (:? (:rule whitespace?))))
   ")" ":" (:rule rule3025?)))

;; class-name
(meta-sexp:defrule rule3022? () ()
)

;; parameter
(meta-sexp:defrule rule3023? () ()
)

;; parameter
(meta-sexp:defrule rule3024? () ()
)

;; constructor-body
(meta-sexp:defrule rule3025? () ()
)

;;; CONNECT statement
;; Form no. 1
(meta-sexp:defrule rule3031? () ()
  (:and "CONNECT"
   (:or
    (:and (:or (:rule rule3027?) (:and "VALUE" "(" (:rule rule3028?) ")"))
     (:? (:rule rule3029?)))
    (:rule rule3030?))
   (:? "NO-ERROR")))

;; physical-name
(meta-sexp:defrule rule3027? () ()
)

;; expression
(meta-sexp:defrule rule3028? () ()
)

;; options
(meta-sexp:defrule rule3029? () ()
)

;; options
(meta-sexp:defrule rule3030? () ()
)

;;; COMPILE statement
;; Form no. 1
(meta-sexp:defrule rule3065? () ()
  (:and "COMPILE"
   (:or (:rule rule3032?) (:rule rule3033?)
    (:and "VALUE" "(" (:rule rule3034?) ")"))
   (:? (:and "ATTR-SPACE" (:? (:and "=" (:rule rule3035?)))))
   (:?
    (:and "SAVE" (:? (:and "=" (:rule rule3036?)))
     (:?
      (:and "INTO"
       (:or (:rule rule3037?) (:and "VALUE" "(" (:rule rule3038?) ")"))))))
   (:?
    (:and "LISTING"
     (:or (:rule rule3039?) (:and "VALUE" "(" (:rule rule3040?) ")"))
     (:?
      (:or (:and "APPEND" (:? (:and "=" (:rule rule3041?))))
       (:and "PAGE-SIZE" (:rule rule3042?))
       (:and "PAGE-WIDTH" (:rule rule3043?))))))
   (:? (:and "XCODE" (:rule rule3044?)))
   (:?
    (:and "XREF"
     (:or (:rule rule3045?) (:and "VALUE" "(" (:rule rule3046?) ")"))
     (:? (:and "APPEND" (:? (:and "=" (:rule rule3047?)))))))
   (:?
    (:and "XREF-XML"
     (:or (:rule rule3048?) (:rule rule3049?)
      (:and "VALUE" "(" (:rule rule3050?) ")"))))
   (:?
    (:and "STRING-XREF"
     (:or (:rule rule3051?) (:and "VALUE" "(" (:rule rule3052?) ")"))
     (:? (:and "APPEND" (:? (:and "=" (:rule rule3053?)))))))
   (:? (:and "STREAM-IO" (:? (:and "=" (:rule rule3054?)))))
   (:?
    (:and "LANGUAGES" "("
     (:or (:rule rule3055?) (:and "VALUE" "(" (:rule rule3056?) ")")) ")"
     (:? (:and "TEXT-SEG-GROW" "=" (:rule rule3057?)))))
   (:?
    (:and "DEBUG-LIST"
     (:or (:rule rule3058?) (:and "VALUE" "(" (:rule rule3059?) ")"))))
   (:?
    (:and "PREPROCESS"
     (:or (:rule rule3060?) (:and "VALUE" "(" (:rule rule3061?) ")"))))
   (:? "NO-ERROR")
   (:?
    (:and "V6FRAME" (:? (:and "=" (:rule rule3062?)))
     (:? (:or "USE-REVVIDEO" "USE-UNDERLINE"))))
   (:? (:and "MIN-SIZE" (:? (:and "=" (:rule rule3063?)))))
   (:? (:and "GENERATE-MD5" (:? (:and "=" (:rule rule3064?)))))))

;; procedure-name
(meta-sexp:defrule rule3032? () ()
)

;; type-name
(meta-sexp:defrule rule3033? () ()
)

;; expression
(meta-sexp:defrule rule3034? () ()
)

;; logical-expression
(meta-sexp:defrule rule3035? () ()
)

;; logical-expression
(meta-sexp:defrule rule3036? () ()
)

;; directory
(meta-sexp:defrule rule3037? () ()
)

;; expression
(meta-sexp:defrule rule3038? () ()
)

;; listfile
(meta-sexp:defrule rule3039? () ()
)

;; expression
(meta-sexp:defrule rule3040? () ()
)

;; logical-expression
(meta-sexp:defrule rule3041? () ()
)

;; integer-expression
(meta-sexp:defrule rule3042? () ()
)

;; integer-expression
(meta-sexp:defrule rule3043? () ()
)

;; expression
(meta-sexp:defrule rule3044? () ()
)

;; xreffile
(meta-sexp:defrule rule3045? () ()
)

;; expression
(meta-sexp:defrule rule3046? () ()
)

;; logical-expression
(meta-sexp:defrule rule3047? () ()
)

;; directory
(meta-sexp:defrule rule3048? () ()
)

;; filename
(meta-sexp:defrule rule3049? () ()
)

;; expression
(meta-sexp:defrule rule3050? () ()
)

;; sxreffile
(meta-sexp:defrule rule3051? () ()
)

;; expression
(meta-sexp:defrule rule3052? () ()
)

;; logical-expression
(meta-sexp:defrule rule3053? () ()
)

;; logical-expression
(meta-sexp:defrule rule3054? () ()
)

;; language-list
(meta-sexp:defrule rule3055? () ()
)

;; expression
(meta-sexp:defrule rule3056? () ()
)

;; growth-factor
(meta-sexp:defrule rule3057? () ()
)

;; debugfile
(meta-sexp:defrule rule3058? () ()
)

;; expression
(meta-sexp:defrule rule3059? () ()
)

;; preprocessfile
(meta-sexp:defrule rule3060? () ()
)

;; expression
(meta-sexp:defrule rule3061? () ()
)

;; logical-expression
(meta-sexp:defrule rule3062? () ()
)

;; logical-expression
(meta-sexp:defrule rule3063? () ()
)

;; logical-expression
(meta-sexp:defrule rule3064? () ()
)

;;; COLOR statement
;; Form no. 1
(meta-sexp:defrule rule3069? () ()
  (:and "COLOR" "PROMPT" (:rule rule3066?) (:* (:rule rule3067?))
   (:? (:rule whitespace?)) (:? (:rule rule3068?))))

;; color-phrase
(meta-sexp:defrule rule3066? () ()
)

;; field
(meta-sexp:defrule rule3067? () ()
)

;; frame-phrase
(meta-sexp:defrule rule3068? () ()
)


;; Form no. 2
(meta-sexp:defrule rule3074? () ()
  (:and "COLOR" (:? "DISPLAY") (:rule rule3070?)
   (:? (:and "PROMPT" (:rule rule3071?))) (:* (:rule rule3072?))
   (:? (:rule whitespace?)) (:? (:rule rule3073?))))

;; color-phrase
(meta-sexp:defrule rule3070? () ()
)

;; color-phrase
(meta-sexp:defrule rule3071? () ()
)

;; field
(meta-sexp:defrule rule3072? () ()
)

;; frame-phrase
(meta-sexp:defrule rule3073? () ()
)

;;; CLOSE STORED-PROCEDURE statement
;; Form no. 1
(meta-sexp:defrule rule3078? () ()
  (:and "CLOSE" "STORED-PROCEDURE" (:rule rule3075?)
   (:? (:and (:rule rule3076?) "=" "PROC-STATUS"))
   (:? (:and "WHERE" "PROC-HANDLE" "=" (:rule rule3077?)))))

;; procedure
(meta-sexp:defrule rule3075? () ()
)

;; integer-field
(meta-sexp:defrule rule3076? () ()
)

;; integer-field
(meta-sexp:defrule rule3077? () ()
)

;;; CLOSE QUERY statement
;; Form no. 1
(meta-sexp:defrule rule3080? () ()
  (:and "CLOSE" "QUERY" (:rule rule3079?)))

;; query
(meta-sexp:defrule rule3079? () ()
)

;;; CLEAR statement
;; Form no. 1
(meta-sexp:defrule rule3082? () ()
  (:and "CLEAR" (:? (:and "FRAME" (:rule rule3081?))) (:? "ALL")
   (:? "NO-PAUSE")))

;; frame
(meta-sexp:defrule rule3081? () ()
)

;;; CLASS statement
;; Form no. 1
(meta-sexp:defrule rule3088? () ()
  (:and "CLASS" (:rule rule3083?) (:? (:and "INHERITS" (:rule rule3084?)))
   (:?
    (:and "IMPLEMENTS" (:rule rule3085?) (:* (:? (:and "," (:rule rule3086?))))
     (:? (:rule whitespace?))))
   (:? "USE-WIDGET-POOL") (:? "FINAL") ":" (:rule rule3087?)))

;; type-name
(meta-sexp:defrule rule3083? () ()
)

;; super-type-name
(meta-sexp:defrule rule3084? () ()
)

;; interface-type-name
(meta-sexp:defrule rule3085? () ()
)

;; interface-type-name
(meta-sexp:defrule rule3086? () ()
)

;; class-body
(meta-sexp:defrule rule3087? () ()
)

;;; CHOOSE statement
;; Form no. 1
(meta-sexp:defrule rule3098? () ()
  (:and "CHOOSE"
   (:or (:and "ROW" (:rule rule3089?) (:? (:and "HELP" (:rule rule3090?))))
    (:and "FIELD"
     (:* (:and (:rule rule3091?) (:? (:and "HELP" (:rule rule3092?)))))
     (:? (:rule whitespace?))))
   (:? "AUTO-RETURN") (:? (:and "COLOR" (:rule rule3093?)))
   (:? (:and "GO-ON" "(" (:* (:rule rule3094?)) (:? (:rule whitespace?)) ")"))
   (:? (:and "KEYS" (:rule rule3095?))) (:? "NO-ERROR")
   (:? (:and "PAUSE" (:rule rule3096?))) (:? (:rule rule3097?))))

;; field
(meta-sexp:defrule rule3089? () ()
)

;; char-constant
(meta-sexp:defrule rule3090? () ()
)

;; field
(meta-sexp:defrule rule3091? () ()
)

;; char-constant
(meta-sexp:defrule rule3092? () ()
)

;; color-phrase
(meta-sexp:defrule rule3093? () ()
)

;; key-label
(meta-sexp:defrule rule3094? () ()
)

;; char-variable
(meta-sexp:defrule rule3095? () ()
)

;; expression
(meta-sexp:defrule rule3096? () ()
)

;; frame-phrase
(meta-sexp:defrule rule3097? () ()
)

;;; CASE statement
;; Form no. 1
(meta-sexp:defrule rule3106? () ()
  (:and "CASE" (:rule rule3099?) ":"
   (:*
    (:and "WHEN" (:rule rule3100?)
     (:* (:? (:and "OR" "WHEN" (:rule rule3101?)))) (:? (:rule whitespace?))
     "THEN" (:or (:rule rule3102?) (:rule rule3103?))))
   (:? (:rule whitespace?))
   (:? (:and "OTHERWISE" (:or (:rule rule3104?) (:rule rule3105?)))) "END"
   (:? "CASE")))

;; expression
(meta-sexp:defrule rule3099? () ()
)

;; value
(meta-sexp:defrule rule3100? () ()
)

;; value
(meta-sexp:defrule rule3101? () ()
)

;; block
(meta-sexp:defrule rule3102? () ()
)

;; statement
(meta-sexp:defrule rule3103? () ()
)

;; block
(meta-sexp:defrule rule3104? () ()
)

;; statement
(meta-sexp:defrule rule3105? () ()
)

;;; BUFFER-COPY statement
;; Form no. 1
(meta-sexp:defrule rule3111? () ()
  (:and "BUFFER-COPY" (:rule rule3107?)
   (:?
    (:and (:or "EXCEPT" "USING") (:* (:rule rule3108?))
     (:? (:rule whitespace?))))
   "TO" (:rule rule3109?)
   (:? (:and "ASSIGN" (:* (:rule rule3110?)) (:? (:rule whitespace?))))
   (:? "NO-LOBS") (:? "NO-ERROR")))

;; source
(meta-sexp:defrule rule3107? () ()
)

;; field
(meta-sexp:defrule rule3108? () ()
)

;; target
(meta-sexp:defrule rule3109? () ()
)

;; assign-expression
(meta-sexp:defrule rule3110? () ()
)

;;; BUFFER-COMPARE statement
;; Form no. 1
(meta-sexp:defrule rule3120? () ()
  (:and "BUFFER-COMPARE" (:rule rule3112?)
   (:?
    (:and (:or "EXCEPT" "USING") (:* (:rule rule3113?))
     (:? (:rule whitespace?))))
   "TO" (:rule rule3114?) (:? (:or "CASE-SENSITIVE" "BINARY"))
   (:? (:and "SAVE" (:? (:and "RESULT" "IN")) (:rule rule3115?)))
   (:? (:and (:? "EXPLICIT") "COMPARES")) ":"
   (:*
    (:?
     (:and "WHEN" (:rule rule3116?) (:? (:rule whitespace?)) (:rule rule3117?)
      (:? (:rule whitespace?)) (:rule rule3118?) "THEN" (:rule rule3119?))))
   (:? (:rule whitespace?)) (:? (:and "END" (:? "COMPARES"))) (:? "NO-LOBS")
   (:? "NO-ERROR")))

;; source
(meta-sexp:defrule rule3112? () ()
)

;; field
(meta-sexp:defrule rule3113? () ()
)

;; target
(meta-sexp:defrule rule3114? () ()
)

;; result-field
(meta-sexp:defrule rule3115? () ()
)

;; field
(meta-sexp:defrule rule3116? () ()
)

;; compare-operator
(meta-sexp:defrule rule3117? () ()
)

;; expression
(meta-sexp:defrule rule3118? () ()
)

;; statement-or-block
(meta-sexp:defrule rule3119? () ()
)

;;; BELL statement
;; Form no. 1
(meta-sexp:defrule rule3121? () ()
  "BELL")

;;; APPLY statement
;; Form no. 1
(meta-sexp:defrule rule3124? () ()
  (:and "APPLY" (:rule rule3122?) (:? (:and "TO" (:rule rule3123?)))))

;; event
(meta-sexp:defrule rule3122? () ()
)

;; widget-phrase
(meta-sexp:defrule rule3123? () ()
)

;;; ACCUMULATE statement
;; Form no. 1
(meta-sexp:defrule rule3127? () ()
  (:and "ACCUMULATE" (:* (:and (:rule rule3125?) "(" (:rule rule3126?) ")"))
   (:? (:rule whitespace?))))

;; expression
(meta-sexp:defrule rule3125? () ()
)

;; aggregate-phrase
(meta-sexp:defrule rule3126? () ()
)
