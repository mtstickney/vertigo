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
