(in-package :vertigo)

(defgeneric transform-tree (transform-op tree)
  (:documentation "Perform the TRANSFORM-OP transformation on the AST tree TREE."))

(defmethod transform-tree ((transform-op (eql 'parse)) (tree ast-node))
  (loop for op in '(collect-lambda-lists
                    collect-blocks
                    convert-eq-to-assign ; Convert '=' ops to ':='
                                        ; when they're a top-level form
                    )
     for tree = (transform-tree op tree)
     finally (return tree)))

;; collect lambda lists into actual lists
;; TODO: these could use a dirty-p flag to avoid consing up a new copy
;; of the whole tree
(defmethod transform-tree ((op (eql 'collect-lambda-lists)) (tree statement-block))
  (let ((new-block (copy-statement-block tree)))
    (setf (statement-block-statements new-block)
          (loop for s in (statement-block-statements tree)
             collect (transform-tree op s)))
    new-block))

(optima:defpattern symbol (&rest args)
  (destructuring-bind (&optional name) args
    (if args
        `(symb- (name (equalp ,name)))
        `(structure symb-))))

(optima:defpattern token (&rest args)
  (destructuring-bind (&optional type) args
    (if args
        `(token- (type (eq ,type)))
        `(structure token-))))

(meta-sexp:defrule param? (&aux direction param type) ()
  (:? (:assign direction (:optima ((and s (or (symbol "INPUT")
                                              (symbol "OUTPUT")
                                              (symbol "INPUT-OUTPUT")))
                                   s))))
  (:assign param (:optima ((and s (symbol)) s)))
  (:optima ((symbol "AS") t))
  (:assign type (:optima ((and s (symbol)) s)))
  (let ((args (list :val param :type type)))
    (when direction
      (setf (getf args :direction) direction))
    (apply #'make-param args)))

(meta-sexp:defrule lambda-list? (&aux p (params '())) ()
  (:optima ((token :lparen) t))
  (:delimited* (:optima ((token :comma) t))
               (:and (:assign p (:rule param?))
                     (:list-push p params)))
  (:optima ((token :rparen) t))
  (make-lambda-list :params (nreverse params)))

(meta-sexp:defrule collect-lambda-lists (&aux obj (items '())) ()
  (:* (:assign obj (:or (:rule lambda-list?)
                        (:type t)))
      (:list-push obj items))
  (nreverse items))

(defmethod transform-tree ((op (eql 'collect-lambda-lists)) (tree statement))
  (let ((new-statement (copy-statement tree))
        (ctx (meta-sexp:create-parser-context (statement-parts statement))))
    (setf (statement-parts new-statement)
          (collect-lambda-lists (statement-parts statement)))))

(defmethod transform-tree ((op (eql 'collect-blocks)) (tree ast-node))
  (collect-blocks tree))

(defgeneric collect-blocks (tree)
  (:documentation "Collect sequences of statements that form a block into STATEMENT-BLOCKs in TREE."))

(defun block-header-p (statement)
  (check-type statement statement)
  (optima:match statement
    ;; Block header statements that
    ((statement- (parts (list* (and ) (or (equalp)))))))
  )

(defmethod collect-blocks ((tree statement))

  )

(defgeneric compile-to-lisp (node)
  (:documentation "Given an AST node NODE, produce the equivalent lisp code for that node."))

;; TODO: figure out how to deal with packages here
(defmethod compile-to-lisp ((node symb))
  (make-symbol (symb-name node)))

(defmethod compile-to-lisp ((node number-value))
  (number-value-val node))

;; Note: ABL time is universal time with millisecond precision
(defun encode-abl-time (second minute hour date month year &optional timezone)
  (check-type second alexandria:non-negative-real)
  (let ((milliseconds (truncate (* 1000 (nth-value 1 (truncate second))))))
    (+  (* (encode-universal-time (truncate second) minute hour date month year timezone)
           1000)
        milliseconds)))

(defun timezone (hour minutes)
  (+ hour (/ minutes 60)))

(defun time-value-timezone (val)
  (if (time-value-tz-present val)
      (timezone (time-value-tz-hr val)
                (time-value-tz-min val))
      nil))

;; Note: DATE and DATETIME values don't have a timezone, but we're
;; using the local one here
(defun encode-abl-date (day month year &optional timezone)
  (encode-abl-time 0 0 0 day month year timezone))

(defmethod compile-to-lisp ((node date-value))
  (encode-abl-date (date-value-day node)
                   (date-value-month node)
                   (date-value-year node)))

(defmethod compile-to-lisp ((node time-value))
  (with-accessors ((hour time-value-hour)
                   (minute time-value-minute)
                   (second time-value-second)
                   (sec-frac time-value-sec-frac)
                   (decimals time-value-sec-decimals)
                   (tz-hour time-value-tz-hr)
                   (tz-min time-value-tz-min))
      node
    (let ((seconds (+ second (/ sec-frac (expt 10 decimals))))
          (time-zone (time-value-timezone node)))
      (encode-abl-time seconds minute hour 1 1 1 time-zone))))

;; Duplicating code, eh? Such a shame.....
(defmethod compile-to-lisp ((node datetime-value))
  (let* ((time (datetime-value-time node))
         (timezone (time-value-timezone time))
         (date (datetime-value-date node))
         (base-time (encode-abl-time 0 0 0 1 1 1 timezone))
         (relative-time (- (compile-to-lisp time)
                           base-time)))
    (+ (encode-abl-date (date-value-day date)
                        (date-value-month date)
                        (date-value-year date)
                        timezone)
       relative-time)))

;; TODO: going to have to deal with the string options somehow (also,
;; an over-sized string probably doesn't behave like an abl string
;; with reserved space -- should probably ignore reserved space since
;; it's a compiler thing)
(defmethod compile-to-lisp ((node string-value))
  (let ((str (string-value-str node))
        (reserved-size (string-value-reserved node)))
    (if (and reserved-size (> reserved-size (length str)))
        (let ((new-str (make-string reserved-size)))
          (replace new-str str)
          new-str)
        str)))

;; TODO: figure out symbol packages here (just use the runtime
;; package, inherits symbols from CL will work)
(defmethod compile-to-lisp ((node op-node))
  (list (make-symbol (op-node-op node))
        (compile-to-lisp (op-node-lhs node))
        (compile-to-lisp (op-node-rhs node))))

(defmethod compile-to-lisp ((node symb))
  (make-symbol (symb-name node)))

(defmethod compile-to-lisp ((node unary-op-node))
  (list (make-symbol (unary-op-node-op node))
        (compile-to-lisp (unary-op-node-val node))))

