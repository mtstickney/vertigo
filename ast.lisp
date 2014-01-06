(in-package #:vertigo)

(defstruct token
  (type)
  (value))

(defstruct symb
  (name))

(defstruct number-value
  (val))

(defstruct (int-value (:include number-value)))

(defstruct statement
  (parts))

;;; DECIMAL types are represented as rationals (probably not in
;;; simplified form)
(defstruct (rational-value (:include number-value)))

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
  (justify :none)
  (translatable t)
  (reserved))

(defstruct comment
  (str))

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

(defstruct list-box
  (list))

(defstruct call
  (type)
  (func)
  (params))

(defstruct widget
  (type)
  (widget)
  (parent))
