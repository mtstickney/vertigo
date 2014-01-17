(in-package #:vertigo)

(defstruct ast-node)

(defstruct (token (:include ast-node))
  (type)
  (value))

(defstruct (symb (:include ast-node))
  (name))

(defstruct (number-value (:include ast-node))
  (val))

(defstruct (int-value (:include number-value)))

(defstruct (statement (:include ast-node))
  (parts)
  ;; Some statements introduce blocks and may have a label
  (label))

;;; DECIMAL types are represented as rationals (probably not in
;;; simplified form)
(defstruct (rational-value (:include number-value)))

(defstruct (date-value (:include ast-node))
  (month)
  (day)
  (year))

(defstruct (time-value (:include ast-node))
  (hour)
  (minute)
  (second)
  (sec-frac)
  (sec-decimals)
  (tz-hr)
  (tz-min)
  (tz-present))

(defstruct (datetime-value (:include ast-node))
  (date)
  (time))

(defstruct (string-value (:include ast-node))
  (str)
  (justify :none)
  (translatable t)
  (reserved))

(defstruct (comment (:include ast-node))
  (str))

(defstruct (op-node (:include ast-node))
  (op)
  (lhs)
  (rhs))

(defstruct (unary-op-node (:include ast-node))
  (op)
  (val))

(defstruct (param (:include ast-node))
  (direction :input)
  (val)
  (type))

(defstruct (lambda-list (:include ast-node))
  (params))

(defstruct (arg (:include ast-node))
  (direction)
  (val))

(defstruct list-box
  (list))

(defstruct (statement-block (:include ast-node))
  (statements))
