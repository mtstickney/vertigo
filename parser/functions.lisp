(in-package :vertigo)

(meta-sexp:defrule param-spec? (&aux match type expr) ()
  (:? (:assign type (:or (:icase "input-output")
                         (:icase "input")
                         (:icase "output")))
      (:not (:type identifier-char)))
  (:assign type (if type (intern (string-upcase type) :keyword) :input))
  (:? (:rule whitespace?))
  (:assign expr (:rule expression?))

  (:return (vertigo::make-param :type type :val expr)))

(meta-sexp:defrule thingy () ()
  (:delimited* #\,
               "a"))
(meta-sexp:defrule param-list? (&aux match param (list (meta-sexp:make-list-accum))) ()
  (:with-stored-match (match)
      "("
    (:? (:rule whitespace?))

    ;; Parameters
    (:? (:delimited* #\,
                     (:? (:rule whitespace?))
                     (:assign param (:rule param-spec?))
                     (:list-push param list)
                     (:? (:rule whitespace?))))
    (:? (:rule whitespace?))
    ")")
  (:return (vertigo::make-param-list :params (nreverse list))))

;;; Custom function call rule, for user-defined and builtin functions.
;;; May not be a bit overly permissive of arg specs.
(meta-sexp:defrule function-call? (&aux match func params) ()
  (:with-stored-match (match)
    (:assign func (:rule identifier?))
    (:? (:rule whitespace?))
    (:assign params (:rule param-list?)))
  (:return (vertigo::make-call :type :function
                               :func func
                               :params (param-list-params params))))

;;; NOTE: procedure calls aren't a simple extension of funcalls (there
;;; can be options between the procedure ident and the parameter list
;; (meta-sexp:defrule procedure-call? (&aux match) ()
;;   (:with-stored-match (match)
;;     (:? (:rule whitespace?))
;;     (:or (:and (:icase "value(")
;;                (:rule value-expression?)
;;                ")")
;;          (:rule function-call?)))
;;   (:return match))

;;; ABSOLUTE function
;; Form no. 1
(meta-sexp:defrule rule1616? () ()
  (:and "ABSOLUTE" "(" (:rule rule1615?) ")"))

;; n
(meta-sexp:defrule rule1615? () ()
)

;;; ACCUM function
;; Form no. 1
(meta-sexp:defrule rule1619? () ()
  (:and "ACCUM" (:rule rule1617?) (:? (:rule whitespace?)) (:rule rule1618?)))

;; aggregate-phrase
(meta-sexp:defrule rule1617? () ()
)

;; expression
(meta-sexp:defrule rule1618? () ()
)

;;; ADD-INTERVAL function
;; Form no. 1
(meta-sexp:defrule rule1623? () ()
  (:and "ADD-INTERVAL" "(" (:rule rule1620?) "," (:rule rule1621?) ","
   (:rule rule1622?) ")"))

;; datetime
(meta-sexp:defrule rule1620? () ()
)

;; interval-amount
(meta-sexp:defrule rule1621? () ()
)

;; interval-unit
(meta-sexp:defrule rule1622? () ()
)

;;; ALIAS function
;; Form no. 1
(meta-sexp:defrule rule1625? () ()
  (:and "ALIAS" "(" (:rule rule1624?) ")"))

;; integer-expression
(meta-sexp:defrule rule1624? () ()
)

;;; AMBIGUOUS function
;; Form no. 1
(meta-sexp:defrule rule1627? () ()
  (:and "AMBIGUOUS" (:rule rule1626?)))

;; record
(meta-sexp:defrule rule1626? () ()
)

;;; ASC function
;; Form no. 1
(meta-sexp:defrule rule1631? () ()
  (:and "ASC" "(" (:rule rule1628?)
   (:? (:and "," (:rule rule1629?) (:? (:and "," (:rule rule1630?))))) ")"))

;; expression
(meta-sexp:defrule rule1628? () ()
)

;; target-codepage
(meta-sexp:defrule rule1629? () ()
)

;; source-codepage
(meta-sexp:defrule rule1630? () ()
)

;;; AUDIT-ENABLED function
;; Form no. 1
(meta-sexp:defrule rule1635? () ()
  (:and "AUDIT-ENABLED" "("
   (:? (:or (:rule rule1632?) (:rule rule1633?) (:rule rule1634?))) ")"))

;; integer-expression
(meta-sexp:defrule rule1632? () ()
)

;; logical-name
(meta-sexp:defrule rule1633? () ()
)

;; alias
(meta-sexp:defrule rule1634? () ()
)

;;; AVAILABLE function
;; Form no. 1
(meta-sexp:defrule rule1637? () ()
  (:and "AVAILABLE" (:rule rule1636?)))

;; record
(meta-sexp:defrule rule1636? () ()
)

;;; BASE64-DECODE function
;; Form no. 1
(meta-sexp:defrule rule1639? () ()
  (:and "BASE64-DECODE" "(" (:rule rule1638?) ")"))

;; expression
(meta-sexp:defrule rule1638? () ()
)

;;; BASE64-ENCODE function
;; Form no. 1
(meta-sexp:defrule rule1641? () ()
  (:and "BASE64-ENCODE" "(" (:rule rule1640?) ")"))

;; expression
(meta-sexp:defrule rule1640? () ()
)

;;; CAN-DO function
;; Form no. 1
(meta-sexp:defrule rule1644? () ()
  (:and "CAN-DO" "(" (:rule rule1642?) (:? (:and "," (:rule rule1643?))) ")"))

;; id-list
(meta-sexp:defrule rule1642? () ()
)

;; string
(meta-sexp:defrule rule1643? () ()
)

;;; CAN-FIND function
;; Form no. 1
(meta-sexp:defrule rule1654? () ()
  (:and "CAN-FIND" "(" (:? (:or "FIRST" "LAST")) (:rule rule1645?)
   (:? (:rule rule1646?)) (:? (:and "OF" (:rule rule1647?)))
   (:? (:and "WHERE" (:rule rule1648?)))
   (:? (:and "USE-INDEX" (:rule rule1649?)))
   (:?
    (:and "USING" (:? (:and "FRAME" (:rule rule1650?))) (:rule rule1651?)
     (:*
      (:?
       (:and "AND" (:? (:and "FRAME" (:rule rule1652?))) (:rule rule1653?))))
     (:? (:rule whitespace?))))
   (:? (:or "SHARE-LOCK" "NO-LOCK")) (:? "NO-WAIT") (:? "NO-PREFETCH") ")"))

;; record
(meta-sexp:defrule rule1645? () ()
)

;; constant
(meta-sexp:defrule rule1646? () ()
)

;; table
(meta-sexp:defrule rule1647? () ()
)

;; expression
(meta-sexp:defrule rule1648? () ()
)

;; index
(meta-sexp:defrule rule1649? () ()
)

;; frame
(meta-sexp:defrule rule1650? () ()
)

;; field
(meta-sexp:defrule rule1651? () ()
)

;; frame
(meta-sexp:defrule rule1652? () ()
)

;; field
(meta-sexp:defrule rule1653? () ()
)

;;; CAN-QUERY function
;; Form no. 1
(meta-sexp:defrule rule1657? () ()
  (:and "CAN-QUERY" "(" (:rule rule1655?) "," (:rule rule1656?) ")"))

;; handle
(meta-sexp:defrule rule1655? () ()
)

;; attribute-name
(meta-sexp:defrule rule1656? () ()
)

;;; CAN-SET function
;; Form no. 1
(meta-sexp:defrule rule1660? () ()
  (:and "CAN-SET" "(" (:rule rule1658?) (:? (:rule whitespace?)) ","
   (:? (:rule whitespace?)) (:rule rule1659?) ")"))

;; handle
(meta-sexp:defrule rule1658? () ()
)

;; attribute-name
(meta-sexp:defrule rule1659? () ()
)

;;; CAPS function
;; Form no. 1
(meta-sexp:defrule rule1662? () ()
  (:and "CAPS" "(" (:rule rule1661?) ")"))

;; expression
(meta-sexp:defrule rule1661? () ()
)

;;; CAST function
;; Form no. 1
(meta-sexp:defrule rule1665? () ()
  (:and "CAST" "(" (:rule rule1663?) "," (:rule rule1664?) ")"))

;; object-reference
(meta-sexp:defrule rule1663? () ()
)

;; type-name
(meta-sexp:defrule rule1664? () ()
)

;;; CHR function
;; Form no. 1
(meta-sexp:defrule rule1669? () ()
  (:and "CHR" "(" (:rule rule1666?)
   (:? (:and "," (:rule rule1667?) (:? (:and "," (:rule rule1668?))))) ")"))

;; expression
(meta-sexp:defrule rule1666? () ()
)

;; target-codepage
(meta-sexp:defrule rule1667? () ()
)

;; source-codepage
(meta-sexp:defrule rule1668? () ()
)

;;; CODEPAGE-CONVERT function
;; Form no. 1
(meta-sexp:defrule rule1673? () ()
  (:and "CODEPAGE-CONVERT" "(" (:rule rule1670?)
   (:? (:and "," (:rule rule1671?) (:? (:and "," (:rule rule1672?))))) ")"))

;; source-string
(meta-sexp:defrule rule1670? () ()
)

;; target-codepage
(meta-sexp:defrule rule1671? () ()
)

;; source-codepage
(meta-sexp:defrule rule1672? () ()
)

;;; COMPARE function
;; Form no. 1
(meta-sexp:defrule rule1679? () ()
  (:and "COMPARE" "(" (:rule rule1674?) "," (:rule rule1675?) ","
   (:rule rule1676?) "," (:rule rule1677?) (:? (:and "," (:rule rule1678?)))
   ")"))

;; string1
(meta-sexp:defrule rule1674? () ()
)

;; relational-operator
(meta-sexp:defrule rule1675? () ()
)

;; string2
(meta-sexp:defrule rule1676? () ()
)

;; strength
(meta-sexp:defrule rule1677? () ()
)

;; collation
(meta-sexp:defrule rule1678? () ()
)

;;; CONNECTED function
;; Form no. 1
(meta-sexp:defrule rule1682? () ()
  (:or (:and "CONNECTED" "(" (:rule rule1680?)) (:and (:rule rule1681?) ")")))

;; logical-name
(meta-sexp:defrule rule1680? () ()
)

;; alias
(meta-sexp:defrule rule1681? () ()
)

;;; COUNT-OF function
;; Form no. 1
(meta-sexp:defrule rule1684? () ()
  (:and "COUNT-OF" "(" (:rule rule1683?) ")"))

;; break-group
(meta-sexp:defrule rule1683? () ()
)

;;; CURRENT-CHANGED function
;; Form no. 1
(meta-sexp:defrule rule1686? () ()
  (:and "CURRENT-CHANGED" (:rule rule1685?)))

;; record
(meta-sexp:defrule rule1685? () ()
)

;;; CURRENT-LANGUAGE function
;; Form no. 1
(meta-sexp:defrule rule1687? () ()
  "CURRENT-LANGUAGE")

;;; CURRENT-RESULT-ROW function
;; Form no. 1
(meta-sexp:defrule rule1689? () ()
  (:and "CURRENT-RESULT-ROW" "(" (:rule rule1688?) ")"))

;; query-name
(meta-sexp:defrule rule1688? () ()
)

;;; CURRENT-VALUE function
;; Form no. 1
(meta-sexp:defrule rule1692? () ()
  (:and "CURRENT-VALUE" "(" (:rule rule1690?) (:? (:and "," (:rule rule1691?)))
   ")"))

;; sequence
(meta-sexp:defrule rule1690? () ()
)

;; logical-dbname
(meta-sexp:defrule rule1691? () ()
)

;;; DATASERVERS function
;; Form no. 1
(meta-sexp:defrule rule1693? () ()
  (:and "\"" "PROGRESS" "," "ODBC" "," "ORACLE" "\""))


;; Form no. 2
(meta-sexp:defrule rule1694? () ()
  "DATASERVERS")

;;; DATA-SOURCE-MODIFIED function
;; Form no. 1
(meta-sexp:defrule rule1696? () ()
  (:and "DATA-SOURCE-MODIFIED" "(" (:rule rule1695?) ")"))

;; buffer-name
(meta-sexp:defrule rule1695? () ()
)

;;; DATE function
;; Form no. 1
(meta-sexp:defrule rule1698? () ()
  (:and "DATE" "(" (:rule rule1697?) ")"))

;; datetime-expression
(meta-sexp:defrule rule1697? () ()
)


;; Form no. 2
(meta-sexp:defrule rule1700? () ()
  (:and "DATE" "(" (:rule rule1699?) ")"))

;; integer-expression
(meta-sexp:defrule rule1699? () ()
)


;; Form no. 3
(meta-sexp:defrule rule1702? () ()
  (:and "DATE" "(" (:rule rule1701?) ")"))

;; string
(meta-sexp:defrule rule1701? () ()
)


;; Form no. 4
(meta-sexp:defrule rule1706? () ()
  (:and "DATE" "(" (:rule rule1703?) "," (:rule rule1704?) ","
   (:rule rule1705?) ")"))

;; month
(meta-sexp:defrule rule1703? () ()
)

;; day
(meta-sexp:defrule rule1704? () ()
)

;; year
(meta-sexp:defrule rule1705? () ()
)

;;; DATETIME function
;; Form no. 1
(meta-sexp:defrule rule1714? () ()
  (:and "DATETIME" "(" (:rule rule1707?) "," (:rule rule1708?) ","
   (:rule rule1709?) "," (:rule rule1710?) "," (:rule rule1711?)
   (:? (:and "," (:rule rule1712?) (:? (:and "," (:rule rule1713?))))) ")"))

;; month
(meta-sexp:defrule rule1707? () ()
)

;; day
(meta-sexp:defrule rule1708? () ()
)

;; year
(meta-sexp:defrule rule1709? () ()
)

;; hours
(meta-sexp:defrule rule1710? () ()
)

;; minutes
(meta-sexp:defrule rule1711? () ()
)

;; seconds
(meta-sexp:defrule rule1712? () ()
)

;; milliseconds
(meta-sexp:defrule rule1713? () ()
)


;; Form no. 2
(meta-sexp:defrule rule1716? () ()
  (:and "DATETIME" "(" (:rule rule1715?) ")"))

;; string
(meta-sexp:defrule rule1715? () ()
)


;; Form no. 3
(meta-sexp:defrule rule1719? () ()
  (:and "DATETIME" "(" (:rule rule1717?) (:? (:and "," (:rule rule1718?))) ")"))

;; date-exp
(meta-sexp:defrule rule1717? () ()
)

;; mtime-exp
(meta-sexp:defrule rule1718? () ()
)

;;; DATETIME-TZ function
;; Form no. 1
(meta-sexp:defrule rule1721? () ()
  (:and "DATETIME-TZ" "(" (:rule rule1720?) ")"))

;; string
(meta-sexp:defrule rule1720? () ()
)


;; Form no. 2
(meta-sexp:defrule rule1730? () ()
  (:and "DATETIME-TZ" "(" (:rule rule1722?) "," (:rule rule1723?) ","
   (:rule rule1724?) "," (:rule rule1725?) "," (:rule rule1726?)
   (:?
    (:and "," (:rule rule1727?)
     (:? (:and "," (:rule rule1728?) (:? (:and "," (:rule rule1729?)))))))
   ")"))

;; month
(meta-sexp:defrule rule1722? () ()
)

;; day
(meta-sexp:defrule rule1723? () ()
)

;; year
(meta-sexp:defrule rule1724? () ()
)

;; hours
(meta-sexp:defrule rule1725? () ()
)

;; minutes
(meta-sexp:defrule rule1726? () ()
)

;; seconds
(meta-sexp:defrule rule1727? () ()
)

;; milliseconds
(meta-sexp:defrule rule1728? () ()
)

;; timezone-exp
(meta-sexp:defrule rule1729? () ()
)


;; Form no. 3
(meta-sexp:defrule rule1733? () ()
  (:and "DATETIME-TZ" "(" (:rule rule1731?) (:? (:and "," (:rule rule1732?)))
   ")"))

;; datetime-tz-exp
(meta-sexp:defrule rule1731? () ()
)

;; timezone-exp
(meta-sexp:defrule rule1732? () ()
)


;; Form no. 4
(meta-sexp:defrule rule1736? () ()
  (:and "DATETIME-TZ" "(" (:rule rule1734?) (:? (:and "," (:rule rule1735?)))
   ")"))

;; datetime-exp
(meta-sexp:defrule rule1734? () ()
)

;; timezone-exp
(meta-sexp:defrule rule1735? () ()
)


;; Form no. 5
(meta-sexp:defrule rule1740? () ()
  (:and "DATETIME-TZ" "(" (:rule rule1737?)
   (:? (:and "," (:rule rule1738?) (:? (:and "," (:rule rule1739?))))) ")"))

;; date-exp
(meta-sexp:defrule rule1737? () ()
)

;; mtime-exp
(meta-sexp:defrule rule1738? () ()
)

;; timezone-exp
(meta-sexp:defrule rule1739? () ()
)

;;; DAY function
;; Form no. 1
(meta-sexp:defrule rule1742? () ()
  (:and "DAY" "(" (:rule rule1741?) ")"))

;; datetime-expression
(meta-sexp:defrule rule1741? () ()
)


;; Form no. 2
(meta-sexp:defrule rule1744? () ()
  (:and "DAY" "(" (:rule rule1743?) ")"))

;; date
(meta-sexp:defrule rule1743? () ()
)

;;; DBCODEPAGE function
;; Form no. 1
(meta-sexp:defrule rule1748? () ()
  (:and "DBCODEPAGE" "("
   (:or (:rule rule1745?) (:rule rule1746?) (:rule rule1747?)) ")"))

;; integer-expression
(meta-sexp:defrule rule1745? () ()
)

;; logical-name
(meta-sexp:defrule rule1746? () ()
)

;; alias
(meta-sexp:defrule rule1747? () ()
)

;;; DBCOLLATION function
;; Form no. 1
(meta-sexp:defrule rule1752? () ()
  (:and "DBCOLLATION" "("
   (:or (:rule rule1749?) (:rule rule1750?) (:rule rule1751?)) ")"))

;; integer-expression
(meta-sexp:defrule rule1749? () ()
)

;; logical-name
(meta-sexp:defrule rule1750? () ()
)

;; alias
(meta-sexp:defrule rule1751? () ()
)

;;; DBNAME function
;; Form no. 1
(meta-sexp:defrule rule1753? () ()
  "DBNAME")

;;; DBPARAM function
;; Form no. 1
(meta-sexp:defrule rule1757? () ()
  (:or (:and "DBPARAM" "(" (:rule rule1754?)) (:rule rule1755?)
   (:and (:rule rule1756?) ")")))

;; integer-expression
(meta-sexp:defrule rule1754? () ()
)

;; logical-name
(meta-sexp:defrule rule1755? () ()
)

;; alias
(meta-sexp:defrule rule1756? () ()
)

;;; DBRESTRICTIONS function
;; Form no. 1
(meta-sexp:defrule rule1762? () ()
  (:and "DBRESTRICTIONS" "("
   (:or (:rule rule1758?) (:rule rule1759?) (:rule rule1760?))
   (:? (:and "," (:rule rule1761?))) ")"))

;; integer-expression
(meta-sexp:defrule rule1758? () ()
)

;; logical-name
(meta-sexp:defrule rule1759? () ()
)

;; alias
(meta-sexp:defrule rule1760? () ()
)

;; table-name
(meta-sexp:defrule rule1761? () ()
)

;;; DBTASKID function
;; Form no. 1
(meta-sexp:defrule rule1766? () ()
  (:or (:and "DBTASKID" "(" (:rule rule1763?)) (:rule rule1764?)
   (:and (:rule rule1765?) ")")))

;; integer-expression
(meta-sexp:defrule rule1763? () ()
)

;; logical-name
(meta-sexp:defrule rule1764? () ()
)

;; alias
(meta-sexp:defrule rule1765? () ()
)

;;; DBTYPE function
;; Form no. 1
(meta-sexp:defrule rule1770? () ()
  (:or (:and "DBTYPE" "(" (:rule rule1767?)) (:rule rule1768?)
   (:and (:rule rule1769?) ")")))

;; integer-expression
(meta-sexp:defrule rule1767? () ()
)

;; logical-name
(meta-sexp:defrule rule1768? () ()
)

;; alias
(meta-sexp:defrule rule1769? () ()
)

;;; DBVERSION function
;; Form no. 1
(meta-sexp:defrule rule1774? () ()
  (:or (:and "DBVERSION" "(" (:rule rule1771?)) (:rule rule1772?)
   (:and (:rule rule1773?) ")")))

;; integer-expression
(meta-sexp:defrule rule1771? () ()
)

;; logical-name
(meta-sexp:defrule rule1772? () ()
)

;; alias
(meta-sexp:defrule rule1773? () ()
)

;;; DECIMAL function
;; Form no. 1
(meta-sexp:defrule rule1776? () ()
  (:and "DECIMAL" "(" (:rule rule1775?) ")"))

;; expression
(meta-sexp:defrule rule1775? () ()
)

;;; DECRYPT function
;; Form no. 1
(meta-sexp:defrule rule1781? () ()
  (:and "DECRYPT" "(" (:rule rule1777?)
   (:?
    (:and "," (:rule rule1778?)
     (:? (:and "," (:rule rule1779?) (:? (:and "," (:rule rule1780?)))))))
   ")"))

;; data-to-decrypt
(meta-sexp:defrule rule1777? () ()
)

;; encrypt-key
(meta-sexp:defrule rule1778? () ()
)

;; iv-value
(meta-sexp:defrule rule1779? () ()
)

;; algorithm
(meta-sexp:defrule rule1780? () ()
)

;;; DEFINED preprocessor function
;; Form no. 1
(meta-sexp:defrule rule1783? () ()
  (:and "DEFINED" "(" (:rule rule1782?) ")"))

;; name
(meta-sexp:defrule rule1782? () ()
)

;;; DYNAMIC-CURRENT-VALUE function
;; Form no. 1
(meta-sexp:defrule rule1786? () ()
  (:and "DYNAMIC-CURRENT-VALUE" "(" (:rule rule1784?) "," (:rule rule1785?)
   ")"))

;; sequence-expression
(meta-sexp:defrule rule1784? () ()
)

;; logical-dbname-expression
(meta-sexp:defrule rule1785? () ()
)

;;; DYNAMIC-FUNCTION function
;; Form no. 1
(meta-sexp:defrule rule1791? () ()
  (:and "DYNAMIC-FUNCTION" "(" (:rule rule1787?)
   (:? (:and "IN" (:rule rule1788?)))
   (:?
    (:and "," (:rule rule1789?) (:* (:? (:and "," (:rule rule1790?))))
     (:? (:rule whitespace?))))
   ")"))

;; function-name
(meta-sexp:defrule rule1787? () ()
)

;; proc-handle
(meta-sexp:defrule rule1788? () ()
)

;; param1
(meta-sexp:defrule rule1789? () ()
)

;; param2
(meta-sexp:defrule rule1790? () ()
)

;;; DYNAMIC-NEXT-VALUE function
;; Form no. 1
(meta-sexp:defrule rule1794? () ()
  (:and "DYNAMIC-NEXT-VALUE" "(" (:rule rule1792?) "," (:rule rule1793?) ")"))

;; sequence-expression
(meta-sexp:defrule rule1792? () ()
)

;; logical-dbname-expression
(meta-sexp:defrule rule1793? () ()
)

;;; ENCODE function
;; Form no. 1
(meta-sexp:defrule rule1796? () ()
  (:and "ENCODE" "(" (:rule rule1795?) ")"))

;; expression
(meta-sexp:defrule rule1795? () ()
)

;;; ENCRYPT function
;; Form no. 1
(meta-sexp:defrule rule1801? () ()
  (:and "ENCRYPT" "(" (:rule rule1797?)
   (:?
    (:and "," (:rule rule1798?)
     (:? (:and "," (:rule rule1799?) (:? (:and "," (:rule rule1800?)))))))
   ")"))

;; data-to-encrypt
(meta-sexp:defrule rule1797? () ()
)

;; encrypt-key
(meta-sexp:defrule rule1798? () ()
)

;; iv-value
(meta-sexp:defrule rule1799? () ()
)

;; algorithm
(meta-sexp:defrule rule1800? () ()
)

;;; ENTERED function
;; Form no. 1
(meta-sexp:defrule rule1804? () ()
  (:and (:? (:and "FRAME" (:rule rule1802?))) (:rule rule1803?) "ENTERED"))

;; frame
(meta-sexp:defrule rule1802? () ()
)

;; field
(meta-sexp:defrule rule1803? () ()
)

;;; ENTRY function
;; Form no. 1
(meta-sexp:defrule rule1808? () ()
  (:and "ENTRY" "(" (:rule rule1805?) "," (:rule rule1806?)
   (:? (:and "," (:rule rule1807?))) ")"))

;; element
(meta-sexp:defrule rule1805? () ()
)

;; list
(meta-sexp:defrule rule1806? () ()
)

;; character
(meta-sexp:defrule rule1807? () ()
)

;;; ERROR function
;; Form no. 1
(meta-sexp:defrule rule1810? () ()
  (:and "ERROR" "(" (:rule rule1809?) ")"))

;; buffer-name
(meta-sexp:defrule rule1809? () ()
)

;;; ETIME function
;; Form no. 1
(meta-sexp:defrule rule1812? () ()
  (:and "ETIME" (:? (:and "(" (:rule rule1811?) ")"))))

;; logical
(meta-sexp:defrule rule1811? () ()
)

;;; EXP function
;; Form no. 1
(meta-sexp:defrule rule1815? () ()
  (:and "EXP" "(" (:rule rule1813?) "," (:rule rule1814?) ")"))

;; base
(meta-sexp:defrule rule1813? () ()
)

;; exponent
(meta-sexp:defrule rule1814? () ()
)

;;; EXTENT function
;; Form no. 1
(meta-sexp:defrule rule1817? () ()
  (:and "EXTENT" "(" (:rule rule1816?) ")"))

;; array
(meta-sexp:defrule rule1816? () ()
)

;;; FILL function
;; Form no. 1
(meta-sexp:defrule rule1820? () ()
  (:and "FILL" "(" (:rule rule1818?) "," (:rule rule1819?) ")"))

;; expression
(meta-sexp:defrule rule1818? () ()
)

;; repeats
(meta-sexp:defrule rule1819? () ()
)

;;; FIRST function
;; Form no. 1
(meta-sexp:defrule rule1822? () ()
  (:and "FIRST" "(" (:rule rule1821?) ")"))

;; break-group
(meta-sexp:defrule rule1821? () ()
)

;;; FIRST-OF function
;; Form no. 1
(meta-sexp:defrule rule1824? () ()
  (:and "FIRST-OF" "(" (:rule rule1823?) ")"))

;; break-group
(meta-sexp:defrule rule1823? () ()
)

;;; FRAME-COL function
;; Form no. 1
(meta-sexp:defrule rule1826? () ()
  (:and "FRAME-COL" (:? (:and "(" (:rule rule1825?) ")"))))

;; frame
(meta-sexp:defrule rule1825? () ()
)

;;; FRAME-DB function
;; Form no. 1
(meta-sexp:defrule rule1827? () ()
  "FRAME-DB")

;;; FRAME-DOWN function
;; Form no. 1
(meta-sexp:defrule rule1829? () ()
  (:and "FRAME-DOWN" (:? (:and "(" (:rule rule1828?) ")"))))

;; frame
(meta-sexp:defrule rule1828? () ()
)

;;; FRAME-FIELD function
;; Form no. 1
(meta-sexp:defrule rule1830? () ()
  "FRAME-FIELD")

;;; FRAME-FILE function
;; Form no. 1
(meta-sexp:defrule rule1831? () ()
  "FRAME-FILE")

;;; FRAME-INDEX function
;; Form no. 1
(meta-sexp:defrule rule1832? () ()
  "FRAME-INDEX")

;;; FRAME-LINE function
;; Form no. 1
(meta-sexp:defrule rule1834? () ()
  (:and "FRAME-LINE" (:? (:and "(" (:rule rule1833?) ")"))))

;; frame
(meta-sexp:defrule rule1833? () ()
)

;;; FRAME-NAME function
;; Form no. 1
(meta-sexp:defrule rule1835? () ()
  "FRAME-NAME")

;;; FRAME-ROW function
;; Form no. 1
(meta-sexp:defrule rule1837? () ()
  (:and "FRAME-ROW" (:? (:and "(" (:rule rule1836?) ")"))))

;; frame
(meta-sexp:defrule rule1836? () ()
)

;;; FRAME-VALUE function
;; Form no. 1
(meta-sexp:defrule rule1838? () ()
  "FRAME-VALUE")

;;; GATEWAYS function
;; Form no. 1
(meta-sexp:defrule rule1839? () ()
  "GATEWAYS")

;;; GENERATE-PBE-KEY function
;; Form no. 1
(meta-sexp:defrule rule1842? () ()
  (:and "GENERATE-PBE-KEY" "(" (:rule rule1840?)
   (:? (:and "," (:rule rule1841?))) ")"))

;; password
(meta-sexp:defrule rule1840? () ()
)

;; salt
(meta-sexp:defrule rule1841? () ()
)

;;; GENERATE-PBE-SALT function
;; Form no. 1
(meta-sexp:defrule rule1843? () ()
  "GENERATE-PBE-SALT")

;;; GENERATE-RANDOM-KEY function
;; Form no. 1
(meta-sexp:defrule rule1844? () ()
  "GENERATE-RANDOM-KEY")

;;; GENERATE-UUID function
;; Form no. 1
(meta-sexp:defrule rule1845? () ()
  "GENERATE-UUID")

;;; GET-BITS function
;; Form no. 1
(meta-sexp:defrule rule1849? () ()
  (:and "GET-BITS" "(" (:rule rule1846?) "," (:rule rule1847?) ","
   (:rule rule1848?) ")"))

;; source
(meta-sexp:defrule rule1846? () ()
)

;; position
(meta-sexp:defrule rule1847? () ()
)

;; numbits
(meta-sexp:defrule rule1848? () ()
)

;;; GET-BYTE function
;; Form no. 1
(meta-sexp:defrule rule1852? () ()
  (:and "GET-BYTE" "(" (:rule rule1850?) "," (:rule rule1851?) ")"))

;; source
(meta-sexp:defrule rule1850? () ()
)

;; position
(meta-sexp:defrule rule1851? () ()
)

;;; GET-BYTE-ORDER function
;; Form no. 1
(meta-sexp:defrule rule1854? () ()
  (:and "GET-BYTE-ORDER" "(" (:rule rule1853?) ")"))

;; memptr
(meta-sexp:defrule rule1853? () ()
)

;;; GET-BYTES function
;; Form no. 1
(meta-sexp:defrule rule1858? () ()
  (:and "GET-BYTES" "(" (:rule rule1855?) "," (:rule rule1856?) ","
   (:rule rule1857?) ")"))

;; source
(meta-sexp:defrule rule1855? () ()
)

;; position
(meta-sexp:defrule rule1856? () ()
)

;; numbytes
(meta-sexp:defrule rule1857? () ()
)

;;; GET-CODEPAGE function
;; Form no. 1
(meta-sexp:defrule rule1860? () ()
  (:and "GET-CODEPAGE" "(" (:rule rule1859?) ")"))

;; large-char-object
(meta-sexp:defrule rule1859? () ()
)

;;; GET-CODEPAGES function
;; Form no. 1
(meta-sexp:defrule rule1861? () ()
  "GET-CODEPAGES")

;;; GET-COLLATION function
;; Form no. 1
(meta-sexp:defrule rule1863? () ()
  (:and "GET-COLLATION" "(" (:rule rule1862?) ")"))

;; clob-field
(meta-sexp:defrule rule1862? () ()
)

;;; GET-COLLATIONS function
;; Form no. 1
(meta-sexp:defrule rule1865? () ()
  (:and "GET-COLLATIONS" "(" (:rule rule1864?) ")"))

;; codepage
(meta-sexp:defrule rule1864? () ()
)

;;; GET-DOUBLE function
;; Form no. 1
(meta-sexp:defrule rule1868? () ()
  (:and "GET-DOUBLE" "(" (:rule rule1866?) "," (:rule rule1867?) ")"))

;; source
(meta-sexp:defrule rule1866? () ()
)

;; position
(meta-sexp:defrule rule1867? () ()
)

;;; GET-FLOAT function
;; Form no. 1
(meta-sexp:defrule rule1871? () ()
  (:and "GET-FLOAT" "(" (:rule rule1869?) "," (:rule rule1870?) ")"))

;; source
(meta-sexp:defrule rule1869? () ()
)

;; position
(meta-sexp:defrule rule1870? () ()
)

;;; GET-INT64 function
;; Form no. 1
(meta-sexp:defrule rule1874? () ()
  (:and "GET-INT64" "(" (:rule rule1872?) "," (:rule rule1873?) ")"))

;; source
(meta-sexp:defrule rule1872? () ()
)

;; position
(meta-sexp:defrule rule1873? () ()
)

;;; GET-LONG function
;; Form no. 1
(meta-sexp:defrule rule1877? () ()
  (:and "GET-LONG" "(" (:rule rule1875?) "," (:rule rule1876?) ")"))

;; source
(meta-sexp:defrule rule1875? () ()
)

;; position
(meta-sexp:defrule rule1876? () ()
)

;;; GET-POINTER-VALUE function
;; Form no. 1
(meta-sexp:defrule rule1879? () ()
  (:and "GET-POINTER-VALUE" "(" (:rule rule1878?) ")"))

;; memptr-var
(meta-sexp:defrule rule1878? () ()
)

;;; GET-SHORT function
;; Form no. 1
(meta-sexp:defrule rule1882? () ()
  (:and "GET-SHORT" "(" (:rule rule1880?) "," (:rule rule1881?) ")"))

;; source
(meta-sexp:defrule rule1880? () ()
)

;; position
(meta-sexp:defrule rule1881? () ()
)

;;; GET-SIZE function
;; Form no. 1
(meta-sexp:defrule rule1884? () ()
  (:and "GET-SIZE" "(" (:rule rule1883?) ")"))

;; memptr-var
(meta-sexp:defrule rule1883? () ()
)

;;; GET-STRING function
;; Form no. 1
(meta-sexp:defrule rule1888? () ()
  (:and "GET-STRING" "(" (:rule rule1885?) "," (:rule rule1886?)
   (:? (:and "," (:rule rule1887?))) ")"))

;; source
(meta-sexp:defrule rule1885? () ()
)

;; position
(meta-sexp:defrule rule1886? () ()
)

;; numbytes
(meta-sexp:defrule rule1887? () ()
)

;;; GET-UNSIGNED-LONG function
;; Form no. 1
(meta-sexp:defrule rule1891? () ()
  (:and "GET-UNSIGNED-LONG" "(" (:rule rule1889?) "," (:rule rule1890?) ")"))

;; source
(meta-sexp:defrule rule1889? () ()
)

;; position
(meta-sexp:defrule rule1890? () ()
)

;;; GET-UNSIGNED-SHORT function
;; Form no. 1
(meta-sexp:defrule rule1894? () ()
  (:and "GET-UNSIGNED-SHORT" "(" (:rule rule1892?) "," (:rule rule1893?) ")"))

;; source
(meta-sexp:defrule rule1892? () ()
)

;; position
(meta-sexp:defrule rule1893? () ()
)

;;; GO-PENDING function
;; Form no. 1
(meta-sexp:defrule rule1895? () ()
  "GO-PENDING")

;;; GUID function
;; Form no. 1
(meta-sexp:defrule rule1897? () ()
  (:and "GUID" "(" (:? (:rule rule1896?)) ")"))

;; UUID
(meta-sexp:defrule rule1896? () ()
)

;;; HANDLE function
;; Form no. 1
(meta-sexp:defrule rule1899? () ()
  (:and "HANDLE" "(" (:rule rule1898?) ")"))

;; handle-string
(meta-sexp:defrule rule1898? () ()
)

;;; HEX-DECODE function
;; Form no. 1
(meta-sexp:defrule rule1901? () ()
  (:and "HEX-DECODE" "(" (:rule rule1900?) ")"))

;; expression
(meta-sexp:defrule rule1900? () ()
)

;;; HEX-ENCODE function
;; Form no. 1
(meta-sexp:defrule rule1903? () ()
  (:and "HEX-ENCODE" "(" (:rule rule1902?) ")"))

;; expression
(meta-sexp:defrule rule1902? () ()
)

;;; IF...THEN...ELSE function
;; Form no. 1
(meta-sexp:defrule rule1907? () ()
  (:and "IF" (:rule rule1904?) "THEN" (:rule rule1905?) "ELSE"
   (:rule rule1906?)))

;; condition
(meta-sexp:defrule rule1904? () ()
)

;; expression1
(meta-sexp:defrule rule1905? () ()
)

;; expression2
(meta-sexp:defrule rule1906? () ()
)

;;; INDEX function
;; Form no. 1
(meta-sexp:defrule rule1911? () ()
  (:and "INDEX" "(" (:rule rule1908?) "," (:rule rule1909?)
   (:? (:and "," (:rule rule1910?))) ")"))

;; source
(meta-sexp:defrule rule1908? () ()
)

;; target
(meta-sexp:defrule rule1909? () ()
)

;; starting
(meta-sexp:defrule rule1910? () ()
)

;;; INPUT function
;; Form no. 1
(meta-sexp:defrule rule1914? () ()
  (:and "INPUT" (:? (:and "FRAME" (:rule rule1912?))) (:rule rule1913?)))

;; frame
(meta-sexp:defrule rule1912? () ()
)

;; field
(meta-sexp:defrule rule1913? () ()
)

;;; INT64 function
;; Form no. 1
(meta-sexp:defrule rule1916? () ()
  (:and "INT64" "(" (:rule rule1915?) ")"))

;; expression
(meta-sexp:defrule rule1915? () ()
)

;;; INTEGER function
;; Form no. 1
(meta-sexp:defrule rule1918? () ()
  (:and "INTEGER" "(" (:rule rule1917?) ")"))

;; expression
(meta-sexp:defrule rule1917? () ()
)

;;; INTERVAL function
;; Form no. 1
(meta-sexp:defrule rule1922? () ()
  (:and "INTERVAL" "(" (:rule rule1919?) "," (:rule rule1920?) ","
   (:rule rule1921?) ")"))

;; datetime1
(meta-sexp:defrule rule1919? () ()
)

;; datetime2
(meta-sexp:defrule rule1920? () ()
)

;; interval-unit
(meta-sexp:defrule rule1921? () ()
)

;;; IS-ATTR-SPACE function
;; Form no. 1
(meta-sexp:defrule rule1923? () ()
  "IS-ATTR-SPACE")

;;; IS-CODEPAGE-FIXED( ) function
;; Form no. 1
(meta-sexp:defrule rule1925? () ()
  (:and "IS-CODEPAGE-FIXED" "(" (:rule rule1924?) ")"))

;; longchar
(meta-sexp:defrule rule1924? () ()
)

;;; IS-COLUMN-CODEPAGE( ) function
;; Form no. 1
(meta-sexp:defrule rule1927? () ()
  (:and "IS-COLUMN-CODEPAGE" "(" (:rule rule1926?) ")"))

;; field
(meta-sexp:defrule rule1926? () ()
)

;;; IS-LEAD-BYTE function
;; Form no. 1
(meta-sexp:defrule rule1929? () ()
  (:and "IS-LEAD-BYTE" "(" (:rule rule1928?) ")"))

;; string
(meta-sexp:defrule rule1928? () ()
)

;;; ISO-DATE function
;; Form no. 1
(meta-sexp:defrule rule1931? () ()
  (:and "ISO-DATE" "(" (:rule rule1930?) ")"))

;; expression
(meta-sexp:defrule rule1930? () ()
)

;;; KBLABEL function
;; Form no. 1
(meta-sexp:defrule rule1933? () ()
  (:and "KBLABEL" "(" (:rule rule1932?) ")"))

;; key-function
(meta-sexp:defrule rule1932? () ()
)

;;; KEYCODE function
;; Form no. 1
(meta-sexp:defrule rule1935? () ()
  (:and "KEYCODE" "(" (:rule rule1934?) ")"))

;; key-label
(meta-sexp:defrule rule1934? () ()
)

;;; KEYFUNCTION function
;; Form no. 1
(meta-sexp:defrule rule1937? () ()
  (:and "KEYFUNCTION" "(" (:rule rule1936?) ")"))

;; expression
(meta-sexp:defrule rule1936? () ()
)

;;; KEYLABEL function
;; Form no. 1
(meta-sexp:defrule rule1939? () ()
  (:and "KEYLABEL" "(" (:rule rule1938?) ")"))

;; key-code
(meta-sexp:defrule rule1938? () ()
)

;;; KEYWORD function
;; Form no. 1
(meta-sexp:defrule rule1941? () ()
  (:and "KEYWORD" "(" (:rule rule1940?) ")"))

;; expression
(meta-sexp:defrule rule1940? () ()
)

;;; KEYWORD-ALL function
;; Form no. 1
(meta-sexp:defrule rule1943? () ()
  (:and "KEYWORD-ALL" "(" (:rule rule1942?) ")"))

;; expression
(meta-sexp:defrule rule1942? () ()
)

;;; LAST function
;; Form no. 1
(meta-sexp:defrule rule1945? () ()
  (:and "LAST" "(" (:rule rule1944?) ")"))

;; break-group
(meta-sexp:defrule rule1944? () ()
)

;;; LASTKEY function
;; Form no. 1
(meta-sexp:defrule rule1946? () ()
  "LASTKEY")

;;; LAST-OF function
;; Form no. 1
(meta-sexp:defrule rule1948? () ()
  (:and "LAST-OF" "(" (:rule rule1947?) ")"))

;; break-group
(meta-sexp:defrule rule1947? () ()
)

;;; LC function
;; Form no. 1
(meta-sexp:defrule rule1950? () ()
  (:and "LC" "(" (:rule rule1949?) ")"))

;; expression
(meta-sexp:defrule rule1949? () ()
)

;;; LDBNAME function
;; Form no. 1
(meta-sexp:defrule rule1955? () ()
  (:and "LDBNAME" "("
   (:or (:rule rule1951?) (:rule rule1952?) (:rule rule1953?)
    (:and "BUFFER" (:rule rule1954?)))
   ")"))

;; integer-expression
(meta-sexp:defrule rule1951? () ()
)

;; logical-name
(meta-sexp:defrule rule1952? () ()
)

;; alias
(meta-sexp:defrule rule1953? () ()
)

;; bufname
(meta-sexp:defrule rule1954? () ()
)

;;; LEFT-TRIM function
;; Form no. 1
(meta-sexp:defrule rule1958? () ()
  (:and "LEFT-TRIM" "(" (:rule rule1956?) (:? (:and "," (:rule rule1957?)))
   ")"))

;; expression
(meta-sexp:defrule rule1956? () ()
)

;; trim-chars
(meta-sexp:defrule rule1957? () ()
)

;;; LENGTH function
;; Form no. 1
(meta-sexp:defrule rule1963? () ()
  (:and "LENGTH" "("
   (:or (:and (:rule rule1959?) (:? (:and "," (:rule rule1960?))))
    (:rule rule1961?) (:rule rule1962?))
   ")"))

;; string
(meta-sexp:defrule rule1959? () ()
)

;; type
(meta-sexp:defrule rule1960? () ()
)

;; raw-expression
(meta-sexp:defrule rule1961? () ()
)

;; blob-field
(meta-sexp:defrule rule1962? () ()
)

;;; LIBRARY function
;; Form no. 1
(meta-sexp:defrule rule1965? () ()
  (:and "LIBRARY" "(" (:rule rule1964?) ")"))

;; string
(meta-sexp:defrule rule1964? () ()
)

;;; LINE-COUNTER function
;; Form no. 1
(meta-sexp:defrule rule1967? () ()
  (:and "LINE-COUNTER" (:? (:and "(" (:rule rule1966?) ")"))))

;; stream
(meta-sexp:defrule rule1966? () ()
)

;;; LIST-EVENTS function
;; Form no. 1
(meta-sexp:defrule rule1970? () ()
  (:and "LIST-EVENTS" "(" (:rule rule1968?) (:? (:and "," (:rule rule1969?)))
   ")"))

;; handle
(meta-sexp:defrule rule1968? () ()
)

;; platform
(meta-sexp:defrule rule1969? () ()
)

;;; LIST-QUERY-ATTRS function
;; Form no. 1
(meta-sexp:defrule rule1972? () ()
  (:and "LIST-QUERY-ATTRS" "(" (:rule rule1971?) ")"))

;; handle
(meta-sexp:defrule rule1971? () ()
)

;;; LIST-SET-ATTRS function
;; Form no. 1
(meta-sexp:defrule rule1974? () ()
  (:and "LIST-SET-ATTRS" "(" (:rule rule1973?) ")"))

;; handle
(meta-sexp:defrule rule1973? () ()
)

;;; LIST-WIDGETS function
;; Form no. 1
(meta-sexp:defrule rule1977? () ()
  (:and "LIST-WIDGETS" "(" (:rule rule1975?) (:? (:and "," (:rule rule1976?)))
   ")"))

;; event-name
(meta-sexp:defrule rule1975? () ()
)

;; platform
(meta-sexp:defrule rule1976? () ()
)

;;; LOCKED function
;; Form no. 1
(meta-sexp:defrule rule1979? () ()
  (:and "LOCKED" (:rule rule1978?)))

;; record
(meta-sexp:defrule rule1978? () ()
)

;;; LOG function
;; Form no. 1
(meta-sexp:defrule rule1982? () ()
  (:and "LOG" "(" (:rule rule1980?) (:? (:and "," (:rule rule1981?))) ")"))

;; expression
(meta-sexp:defrule rule1980? () ()
)

;; base
(meta-sexp:defrule rule1981? () ()
)

;;; LOGICAL function
;; Form no. 1
(meta-sexp:defrule rule1985? () ()
  (:and "LOGICAL" "(" (:rule rule1983?) (:? (:and "," (:rule rule1984?))) ")"))

;; expression
(meta-sexp:defrule rule1983? () ()
)

;; char-expression-format
(meta-sexp:defrule rule1984? () ()
)

;;; LOOKUP function
;; Form no. 1
(meta-sexp:defrule rule1989? () ()
  (:and "LOOKUP" "(" (:rule rule1986?) "," (:rule rule1987?)
   (:? (:and "," (:rule rule1988?))) ")"))

;; expression
(meta-sexp:defrule rule1986? () ()
)

;; list
(meta-sexp:defrule rule1987? () ()
)

;; character
(meta-sexp:defrule rule1988? () ()
)

;;; MAXIMUM function
;; Form no. 1
(meta-sexp:defrule rule1993? () ()
  (:and "MAXIMUM" "(" (:rule rule1990?) "," (:rule rule1991?)
   (:* (:? (:and "," (:rule rule1992?)))) (:? (:rule whitespace?)) ")"))

;; expression
(meta-sexp:defrule rule1990? () ()
)

;; expression
(meta-sexp:defrule rule1991? () ()
)

;; expression
(meta-sexp:defrule rule1992? () ()
)

;;; MD5-DIGEST function
;; Form no. 1
(meta-sexp:defrule rule1996? () ()
  (:and "MD5-DIGEST" "(" (:rule rule1994?) (:? (:and "," (:rule rule1995?)))
   ")"))

;; data-to-hash
(meta-sexp:defrule rule1994? () ()
)

;; hash-key
(meta-sexp:defrule rule1995? () ()
)

;;; MEMBER function
;; Form no. 1
(meta-sexp:defrule rule1998? () ()
  (:and "MEMBER" "(" (:rule rule1997?) ")"))

;; string
(meta-sexp:defrule rule1997? () ()
)

;;; MESSAGE-LINES function
;; Form no. 1
(meta-sexp:defrule rule1999? () ()
  "MESSAGE-LINES")

;;; MINIMUM function
;; Form no. 1
(meta-sexp:defrule rule2003? () ()
  (:and "MINIMUM" "(" (:rule rule2000?) "," (:rule rule2001?)
   (:* (:? (:and "," (:rule rule2002?)))) (:? (:rule whitespace?)) ")"))

;; expression
(meta-sexp:defrule rule2000? () ()
)

;; expression
(meta-sexp:defrule rule2001? () ()
)

;; expression
(meta-sexp:defrule rule2002? () ()
)

;;; MONTH function
;; Form no. 1
(meta-sexp:defrule rule2005? () ()
  (:and "MONTH" "(" (:rule rule2004?) ")"))

;; datetime-expression
(meta-sexp:defrule rule2004? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2007? () ()
  (:and "MONTH" "(" (:rule rule2006?) ")"))

;; date
(meta-sexp:defrule rule2006? () ()
)

;;; MTIME function
;; Form no. 1
(meta-sexp:defrule rule2009? () ()
  (:and "MTIME" "(" (:? (:rule rule2008?)) ")"))

;; datetime-expression
(meta-sexp:defrule rule2008? () ()
)

;;; NEW function
;; Form no. 1
(meta-sexp:defrule rule2011? () ()
  (:and "NEW" (:rule rule2010?)))

;; record
(meta-sexp:defrule rule2010? () ()
)

;;; NEXT-VALUE function
;; Form no. 1
(meta-sexp:defrule rule2014? () ()
  (:and "NEXT-VALUE" "(" (:rule rule2012?) (:? (:and "," (:rule rule2013?)))
   ")"))

;; sequence
(meta-sexp:defrule rule2012? () ()
)

;; logical-dbname
(meta-sexp:defrule rule2013? () ()
)

;;; NORMALIZE function
;; Form no. 1
(meta-sexp:defrule rule2017? () ()
  (:and "NORMALIZE" "(" (:rule rule2015?) "," (:rule rule2016?) ")"))

;; string
(meta-sexp:defrule rule2015? () ()
)

;; normalization-form
(meta-sexp:defrule rule2016? () ()
)

;;; NOT ENTERED function
;; Form no. 1
(meta-sexp:defrule rule2020? () ()
  (:and (:? (:and "FRAME" (:rule rule2018?))) (:rule rule2019?) "NOT"
   "ENTERED"))

;; frame
(meta-sexp:defrule rule2018? () ()
)

;; field
(meta-sexp:defrule rule2019? () ()
)

;;; NOW function
;; Form no. 1
(meta-sexp:defrule rule2021? () ()
  "NOW")

;;; NUM-ALIASES function
;; Form no. 1
(meta-sexp:defrule rule2022? () ()
  "NUM-ALIASES")

;;; NUM-DBS function
;; Form no. 1
(meta-sexp:defrule rule2023? () ()
  "NUM-DBS")

;;; NUM-ENTRIES function
;; Form no. 1
(meta-sexp:defrule rule2026? () ()
  (:and "NUM-ENTRIES" "(" (:rule rule2024?) (:? (:and "," (:rule rule2025?)))
   ")"))

;; list
(meta-sexp:defrule rule2024? () ()
)

;; character
(meta-sexp:defrule rule2025? () ()
)

;;; NUM-RESULTS function
;; Form no. 1
(meta-sexp:defrule rule2028? () ()
  (:and "NUM-RESULTS" "(" (:rule rule2027?) ")"))

;; query-name
(meta-sexp:defrule rule2027? () ()
)

;;; OPSYS function
;; Form no. 1
(meta-sexp:defrule rule2029? () ()
  "OPSYS")

;;; OS-DRIVES function
;; Form no. 1
(meta-sexp:defrule rule2030? () ()
  "OS-DRIVES")

;;; OS-ERROR function
;; Form no. 1
(meta-sexp:defrule rule2031? () ()
  "OS-ERROR")

;;; OS-GETENV function
;; Form no. 1
(meta-sexp:defrule rule2033? () ()
  (:and "OS-GETENV" "(" (:rule rule2032?) ")"))

;; environment-variable
(meta-sexp:defrule rule2032? () ()
)

;;; PAGE-NUMBER function
;; Form no. 1
(meta-sexp:defrule rule2035? () ()
  (:and "PAGE-NUMBER" (:? (:and "(" (:rule rule2034?) ")"))))

;; stream
(meta-sexp:defrule rule2034? () ()
)

;;; PAGE-SIZE function
;; Form no. 1
(meta-sexp:defrule rule2037? () ()
  (:and "PAGE-SIZE" (:? (:and "(" (:rule rule2036?) ")"))))

;; stream
(meta-sexp:defrule rule2036? () ()
)

;;; PDBNAME function
;; Form no. 1
(meta-sexp:defrule rule2041? () ()
  (:or (:and "PDBNAME" "(" (:rule rule2038?)) (:rule rule2039?)
   (:and (:rule rule2040?) ")")))

;; integer-expression
(meta-sexp:defrule rule2038? () ()
)

;; logical-name
(meta-sexp:defrule rule2039? () ()
)

;; alias
(meta-sexp:defrule rule2040? () ()
)

;;; PROC-HANDLE function
;; Form no. 1
(meta-sexp:defrule rule2042? () ()
  "PROC-HANDLE")

;;; PROC-STATUS function
;; Form no. 1
(meta-sexp:defrule rule2043? () ()
  "PROC-STATUS")

;;; PROGRAM-NAME function
;; Form no. 1
(meta-sexp:defrule rule2045? () ()
  (:and "PROGRAM-NAME" "(" (:rule rule2044?) ")"))

;; n
(meta-sexp:defrule rule2044? () ()
)

;;; PROGRESS function
;; Form no. 1
(meta-sexp:defrule rule2046? () ()
  "PROGRESS")

;;; PROMSGS function
;; Form no. 1
(meta-sexp:defrule rule2047? () ()
  "PROMSGS")

;;; PROPATH function
;; Form no. 1
(meta-sexp:defrule rule2048? () ()
  "PROPATH")

;;; PROVERSION function
;; Form no. 1
(meta-sexp:defrule rule2049? () ()
  "PROVERSION")

;;; QUERY-OFF-END function
;; Form no. 1
(meta-sexp:defrule rule2051? () ()
  (:and "QUERY-OFF-END" "(" (:rule rule2050?) ")"))

;; query-name
(meta-sexp:defrule rule2050? () ()
)

;;; QUOTER function
;; Form no. 1
(meta-sexp:defrule rule2055? () ()
  (:and "QUOTER" "(" (:rule rule2052?)
   (:? (:and "," (:rule rule2053?) (:? (:and "," (:rule rule2054?))))) ")"))

;; expression
(meta-sexp:defrule rule2052? () ()
)

;; quote-char
(meta-sexp:defrule rule2053? () ()
)

;; null-string
(meta-sexp:defrule rule2054? () ()
)

;;; R-INDEX function
;; Form no. 1
(meta-sexp:defrule rule2059? () ()
  (:and "R-INDEX" "(" (:rule rule2056?) "," (:rule rule2057?)
   (:? (:and "," (:rule rule2058?))) ")"))

;; source
(meta-sexp:defrule rule2056? () ()
)

;; target
(meta-sexp:defrule rule2057? () ()
)

;; starting
(meta-sexp:defrule rule2058? () ()
)

;;; RANDOM function
;; Form no. 1
(meta-sexp:defrule rule2062? () ()
  (:and "RANDOM" "(" (:rule rule2060?) "," (:rule rule2061?) ")"))

;; low
(meta-sexp:defrule rule2060? () ()
)

;; high
(meta-sexp:defrule rule2061? () ()
)

;;; RAW function
;; Form no. 1
(meta-sexp:defrule rule2066? () ()
  (:and "RAW" "(" (:rule rule2063?)
   (:? (:and "," (:rule rule2064?) (:? (:and "," (:rule rule2065?))))) ")"))

;; field
(meta-sexp:defrule rule2063? () ()
)

;; position
(meta-sexp:defrule rule2064? () ()
)

;; length
(meta-sexp:defrule rule2065? () ()
)

;;; RECID function
;; Form no. 1
(meta-sexp:defrule rule2068? () ()
  (:and "RECID" "(" (:rule rule2067?) ")"))

;; record
(meta-sexp:defrule rule2067? () ()
)

;;; RECORD-LENGTH function
;; Form no. 1
(meta-sexp:defrule rule2070? () ()
  (:and "RECORD-LENGTH" "(" (:rule rule2069?) ")"))

;; buffer
(meta-sexp:defrule rule2069? () ()
)

;;; REJECTED function
;; Form no. 1
(meta-sexp:defrule rule2072? () ()
  (:and "REJECTED" "(" (:rule rule2071?) ")"))

;; buffer-name
(meta-sexp:defrule rule2071? () ()
)

;;; REPLACE function
;; Form no. 1
(meta-sexp:defrule rule2076? () ()
  (:and "REPLACE" "(" (:rule rule2073?) "," (:rule rule2074?) ","
   (:rule rule2075?) ")"))

;; source-string
(meta-sexp:defrule rule2073? () ()
)

;; from-string
(meta-sexp:defrule rule2074? () ()
)

;; to-string
(meta-sexp:defrule rule2075? () ()
)

;;; RETRY function
;; Form no. 1
(meta-sexp:defrule rule2077? () ()
  "RETRY")

;;; RETURN-VALUE function
;; Form no. 1
(meta-sexp:defrule rule2078? () ()
  "RETURN-VALUE")

;;; RGB-VALUE function
;; Form no. 1
(meta-sexp:defrule rule2082? () ()
  (:and "RGB-VALUE" "(" (:rule rule2079?) "," (:rule rule2080?) ","
   (:rule rule2081?) ")"))

;; redval
(meta-sexp:defrule rule2079? () ()
)

;; greenval
(meta-sexp:defrule rule2080? () ()
)

;; blueval
(meta-sexp:defrule rule2081? () ()
)

;;; RIGHT-TRIM function
;; Form no. 1
(meta-sexp:defrule rule2085? () ()
  (:and "RIGHT-TRIM" "(" (:rule rule2083?) (:? (:and "," (:rule rule2084?)))
   ")"))

;; expression
(meta-sexp:defrule rule2083? () ()
)

;; trim-chars
(meta-sexp:defrule rule2084? () ()
)

;;; ROUND function
;; Form no. 1
(meta-sexp:defrule rule2088? () ()
  (:and "ROUND" "(" (:rule rule2086?) "," (:rule rule2087?) ")"))

;; expression
(meta-sexp:defrule rule2086? () ()
)

;; precision
(meta-sexp:defrule rule2087? () ()
)

;;; ROW-STATE function
;; Form no. 1
(meta-sexp:defrule rule2090? () ()
  (:and "ROW-STATE" "(" (:rule rule2089?) ")"))

;; buffer-name
(meta-sexp:defrule rule2089? () ()
)

;;; ROWID function
;; Form no. 1
(meta-sexp:defrule rule2092? () ()
  (:and "ROWID" "(" (:rule rule2091?) ")"))

;; record
(meta-sexp:defrule rule2091? () ()
)

;;; SCREEN-LINES function
;; Form no. 1
(meta-sexp:defrule rule2093? () ()
  "SCREEN-LINES")

;;; SDBNAME function
;; Form no. 1
(meta-sexp:defrule rule2097? () ()
  (:and "SDBNAME" "("
   (:or (:rule rule2094?) (:rule rule2095?) (:rule rule2096?)) ")"))

;; integer-expression
(meta-sexp:defrule rule2094? () ()
)

;; logical-name
(meta-sexp:defrule rule2095? () ()
)

;; alias
(meta-sexp:defrule rule2096? () ()
)

;;; SEARCH function
;; Form no. 1
(meta-sexp:defrule rule2099? () ()
  (:and "SEARCH" "(" (:rule rule2098?) ")"))

;; opsys-file
(meta-sexp:defrule rule2098? () ()
)

;;; SEEK function
;; Form no. 1
(meta-sexp:defrule rule2101? () ()
  (:and "SEEK" "(" (:or "INPUT" "OUTPUT" (:rule rule2100?)) ")"))

;; name
(meta-sexp:defrule rule2100? () ()
)

;;; SET-DB-CLIENT function
;; Form no. 1
(meta-sexp:defrule rule2106? () ()
  (:and "SET-DB-CLIENT" "(" (:rule rule2102?)
   (:? (:or (:and "," (:rule rule2103?)) (:rule rule2104?) (:rule rule2105?)))
   ")"))

;; client-principal-handle
(meta-sexp:defrule rule2102? () ()
)

;; integer-expression
(meta-sexp:defrule rule2103? () ()
)

;; logical-name
(meta-sexp:defrule rule2104? () ()
)

;; alias
(meta-sexp:defrule rule2105? () ()
)

;;; SETUSERID function
;; Form no. 1
(meta-sexp:defrule rule2110? () ()
  (:and "SETUSERID" "(" (:rule rule2107?) "," (:rule rule2108?)
   (:? (:and "," (:rule rule2109?))) ")"))

;; userid
(meta-sexp:defrule rule2107? () ()
)

;; password
(meta-sexp:defrule rule2108? () ()
)

;; logical-dbname
(meta-sexp:defrule rule2109? () ()
)

;;; SHA1-DIGEST function
;; Form no. 1
(meta-sexp:defrule rule2113? () ()
  (:and "SHA1-DIGEST" "(" (:rule rule2111?) (:? (:and "," (:rule rule2112?)))
   ")"))

;; data-to-hash
(meta-sexp:defrule rule2111? () ()
)

;; hash-key
(meta-sexp:defrule rule2112? () ()
)

;;; SQRT function
;; Form no. 1
(meta-sexp:defrule rule2115? () ()
  (:and "SQRT" "(" (:rule rule2114?) ")"))

;; expression
(meta-sexp:defrule rule2114? () ()
)

;;; SSL-SERVER-NAME function
;; Form no. 1
(meta-sexp:defrule rule2117? () ()
  (:and "SSL-SERVER-NAME" "(" (:rule rule2116?) ")"))

;; logical-database-name
(meta-sexp:defrule rule2116? () ()
)

;;; STRING function
;; Form no. 1
(meta-sexp:defrule rule2120? () ()
  (:and "STRING" "(" (:rule rule2118?) (:? (:and "," (:rule rule2119?))) ")"))

;; source
(meta-sexp:defrule rule2118? () ()
)

;; format
(meta-sexp:defrule rule2119? () ()
)

;;; SUBSTITUTE function
;; Form no. 1
(meta-sexp:defrule rule2123? () ()
  (:and "SUBSTITUTE" "(" (:rule rule2121?)
   (:* (:? (:and "," (:rule rule2122?)))) (:? (:rule whitespace?)) ")"))

;; base-string
(meta-sexp:defrule rule2121? () ()
)

;; arg
(meta-sexp:defrule rule2122? () ()
)

;;; SUBSTRING function
;; Form no. 1
(meta-sexp:defrule rule2128? () ()
  (:and "SUBSTRING" "(" (:rule rule2124?) "," (:rule rule2125?)
   (:? (:and "," (:rule rule2126?) (:? (:and "," (:rule rule2127?))))) ")"))

;; source
(meta-sexp:defrule rule2124? () ()
)

;; position
(meta-sexp:defrule rule2125? () ()
)

;; length
(meta-sexp:defrule rule2126? () ()
)

;; type
(meta-sexp:defrule rule2127? () ()
)

;;; SUPER function
;; Form no. 1
(meta-sexp:defrule rule2131? () ()
  (:and "SUPER"
   (:?
    (:and "(" (:rule rule2129?) (:* (:? (:and "," (:rule rule2130?))))
     (:? (:rule whitespace?)) ")"))))

;; parameter
(meta-sexp:defrule rule2129? () ()
)

;; parameter
(meta-sexp:defrule rule2130? () ()
)

;;; TERMINAL function
;; Form no. 1
(meta-sexp:defrule rule2132? () ()
  "TERMINAL")

;;; TIME function
;; Form no. 1
(meta-sexp:defrule rule2133? () ()
  "TIME")

;;; TIMEZONE function
;; Form no. 1
(meta-sexp:defrule rule2136? () ()
  (:and "TIMEZONE" "(" (:? (:or (:rule rule2134?) (:rule rule2135?))) ")"))

;; datetime-tz-expression
(meta-sexp:defrule rule2134? () ()
)

;; char-expression
(meta-sexp:defrule rule2135? () ()
)

;;; TODAY function
;; Form no. 1
(meta-sexp:defrule rule2137? () ()
  "TODAY")

;;; TO-ROWID function
;; Form no. 1
(meta-sexp:defrule rule2139? () ()
  (:and "TO-ROWID" "(" (:rule rule2138?) ")"))

;; rowid-string
(meta-sexp:defrule rule2138? () ()
)

;;; TRANSACTION function
;; Form no. 1
(meta-sexp:defrule rule2140? () ()
  "TRANSACTION")

;;; TRIM function
;; Form no. 1
(meta-sexp:defrule rule2143? () ()
  (:and "TRIM" "(" (:rule rule2141?) (:? (:and "," (:rule rule2142?))) ")"))

;; expression
(meta-sexp:defrule rule2141? () ()
)

;; trim-chars
(meta-sexp:defrule rule2142? () ()
)

;;; TRUNCATE function
;; Form no. 1
(meta-sexp:defrule rule2146? () ()
  (:and "TRUNCATE" "(" (:rule rule2144?) "," (:rule rule2145?) ")"))

;; expression
(meta-sexp:defrule rule2144? () ()
)

;; decimal-places
(meta-sexp:defrule rule2145? () ()
)

;;; TYPE-OF function
;; Form no. 1
(meta-sexp:defrule rule2149? () ()
  (:and "TYPE-OF" "(" (:rule rule2147?) "," (:rule rule2148?) ")"))

;; object-reference
(meta-sexp:defrule rule2147? () ()
)

;; type-name
(meta-sexp:defrule rule2148? () ()
)

;;; USERID function
;; Form no. 1
(meta-sexp:defrule rule2151? () ()
  (:and "USERID" (:? (:and "(" (:rule rule2150?) ")"))))

;; logical-dbname
(meta-sexp:defrule rule2150? () ()
)

;;; VALID-EVENT function
;; Form no. 1
(meta-sexp:defrule rule2155? () ()
  (:and "VALID-EVENT" "(" (:rule rule2152?) "," (:rule rule2153?)
   (:? (:and "," (:rule rule2154?))) ")"))

;; handle
(meta-sexp:defrule rule2152? () ()
)

;; event-name
(meta-sexp:defrule rule2153? () ()
)

;; platform
(meta-sexp:defrule rule2154? () ()
)

;;; VALID-HANDLE function
;; Form no. 1
(meta-sexp:defrule rule2157? () ()
  (:and "VALID-HANDLE" "(" (:rule rule2156?) ")"))

;; handle
(meta-sexp:defrule rule2156? () ()
)

;;; VALID-OBJECT function
;; Form no. 1
(meta-sexp:defrule rule2159? () ()
  (:and "VALID-OBJECT" "(" (:rule rule2158?) ")"))

;; object-reference
(meta-sexp:defrule rule2158? () ()
)

;;; WEEKDAY function
;; Form no. 1
(meta-sexp:defrule rule2161? () ()
  (:and "WEEKDAY" "(" (:rule rule2160?) ")"))

;; datetime-expression
(meta-sexp:defrule rule2160? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2163? () ()
  (:and "WEEKDAY" "(" (:rule rule2162?) ")"))

;; date
(meta-sexp:defrule rule2162? () ()
)

;;; WIDGET-HANDLE function
;; Form no. 1
(meta-sexp:defrule rule2165? () ()
  (:and "WIDGET-HANDLE" "(" (:rule rule2164?) ")"))

;; handle-string
(meta-sexp:defrule rule2164? () ()
)

;;; YEAR function
;; Form no. 1
(meta-sexp:defrule rule2167? () ()
  (:and "YEAR" "(" (:rule rule2166?) ")"))

;; datetime-expression
(meta-sexp:defrule rule2166? () ()
)


;; Form no. 2
(meta-sexp:defrule rule2169? () ()
  (:and "YEAR" "(" (:rule rule2168?) ")"))

;; date
(meta-sexp:defrule rule2168? () ()
)
