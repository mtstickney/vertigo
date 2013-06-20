(in-package #:vertigo)

(defstruct int-value
  (val))

;;; DECIMAL types are represented as rationals (probably not in
;;; simplified form)
(defstruct rational-value
  ;; integer part
  (int)
  ;; (integral) fractional part
  (frac)
  ;; Number of decimal digits, useful for sig-fig checks
  (decimals))

(defstruct date-value
  (month)
  (day)
  (year))

(defstruct time-value
  (hour)
  (minute)
  (second)
  (sec-frac)
  (sec-decimals)
  (tz-hr)
  (tz-min)
  (tz-present))

(defstruct datetime-value
  (date)
  (time))

(defstruct string-value
  (str)
  (justify)
  (translatable)
  (reserved))

(defstruct boolean-value
  (val))

(defstruct ident
  (name))

(defstruct op-node
  (op)
  (lhs)
  (rhs))

(defstruct unary-op-node
  (op)
  (val))

(defstruct param
  (type :input)
  (val))

(defstruct param-list
  (params))

(defstruct call
  (type)
  (func)
  (params))
