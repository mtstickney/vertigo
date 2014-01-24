(in-package :vertigo)

(defgeneric transform-tree (transform-op tree)
  (:documentation "Perform the TRANSFORM-OP transformation on the AST tree TREE."))

;; Some useful default traversal methods
(defmethod transform-tree (op (tree statement-block))
  (make-statement-block :statements (loop for s in (statement-block-statements tree)
                                          collect (transform-tree op s))))

(defmethod transform-tree (op (tree statement))
  (make-statement :label (statement-label tree)
                  :parts (loop for p in (statement-parts tree)
                               collect (transform-tree op p))))

;; Default NO-OP method for other atoms
(defmethod transform-tree (op (tree ast-node))
  tree)

(defmethod transform-tree ((transform-op (eql 'parse)) (tree ast-node))
  (loop for op in '(collect-lambda-lists
                    collect-blocks
                    convert-eq-to-assign ; Convert '=' ops to ':='
                                        ; when they're a top-level
                                        ; form
                    fixup-arglists
                    ;; Overly-aggressive stuff that needs to go last
                    collect-db-fields)
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

(optima:defpattern statement (&rest args)
  (if args
      `(statement- (parts (list ,@args)))
      `(structure statement-)))

(optima:defpattern statement* (&rest args)
  (if args
      `(statement- (parts (list* ,@args)))
      `(structure statement-)))

(optima:defpattern block (&rest args)
  (if args
      `(statement-block- (statements (list ,@args)))
      `(structure statement-block-)))

(optima:defpattern block* (&rest args)
  (if args
      `(statement-block- (statements (list* ,@args)))
      `(structure statement-block-)))

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
        (ctx (meta-sexp:create-parser-context (statement-parts tree))))
    (setf (statement-parts new-statement)
          (collect-lambda-lists ctx))))

(meta-sexp:defrule block-header-statement? () ()
  (:optima
    ;; Block header statements that
   ((and s (statement* (or (symbol "DO")
                           (symbol "FOR")
                           (symbol "REPEAT")
                           (symbol "PROCEDURE")
                           (symbol "METHOD")
                           (symbol "INTERFACE")
                           (symbol "FUNCTION")
                           (symbol "DESTRUCTOR")
                           (symbol "CONSTRUCTOR")
                           (symbol "CLASS")
                           (symbol "CASE")
                           (symbol "GET")
                           (symbol "SET"))
                       _))
    s)))

(meta-sexp:defrule collected-block-statement? (&aux s) ()
  ;; Returns S if S is an AST statement that contains a
  ;; statement-block followed by an END symbol, nil otherwise.
  (:assign s (:optima ((and s (statement)) s)))
  (:with-context-for ((statement-parts s))
    (:* (:not (:optima ((block) t)))
        (:not (:optima ((statement* (symbol "END") _) t)))
        (:optima ((structure ast-node-) t)))
    (:optima ((block) t))
    (:optima ((symbol "END") t)))
  s)

(meta-sexp:defrule block-or-statement? (&aux head s (statements '())) ()
  (:or (:rule collected-block-statement?)
       (:checkpoint (:assign head (:rule block-header-statement?))
                    (:* (:not (:optima ((statement* (symbol "END") _) t)))
                        (:assign s (:rule block-or-statement?))
                        (:list-push s statements))
                    ;; TODO: check the end type in here eventually
                    (:assign s (:optima ((and s (statement* (symbol "END") _)) s)))
                    (make-statement :parts (append (statement-parts head)
                                                   (list (make-statement-block :statements (nreverse statements)))
                                                   ;; END statement
                                                   (statement-parts s))
                                    :label (statement-label head)))
       (:optima ((and s (statement)) s))))

(meta-sexp:defrule collect-blocks (&aux s (statements '())) ()
  (:* (:assign s (:rule block-or-statement?))
      (:list-push s statements))
  (:eof)
  (make-statement-block :statements (nreverse statements)))

(defmethod transform-tree ((op (eql 'collect-blocks)) (tree statement-block))
  "Collect sequences of statements that form a block into STATEMENT-BLOCKs in TREE."
  (let ((new-block (collect-blocks (meta-sexp:create-parser-context (statement-block-statements tree)))))
    (unless new-block
      ;; TODO: this should go in one of the defrules so we can give a
      ;; better error message
      (error "Unable to collect blocks in statement block ~S~%" tree))
    new-block))

(defmethod transform-tree ((op (eql 'convert-eq-to-assign)) (tree statement))
  (optima:match tree
    ((statement (op-node- (op (equalp "="))
                          (lhs lhs)
                          (rhs rhs)))
     (make-statement :parts (list (make-op-node :lhs lhs
                                                :rhs rhs
                                                :op ":="))
                     :label (statement-label tree)))
    (s s)))

(meta-sexp:defrule separated-list? (separator &aux (items '())) ()
  ;; AST nodes separated by separator tokens
  (:? (:delimited* (:optima ((token separator) t))
                   (:optima ((and x
                                  (structure ast-node-)
                                  (not (token)))
                             (push x items) t))))
  (:eof)
  (:return (make-list-box :list (nreverse items))))

(defmethod transform-tree ((op (eql 'fixup-arglists)) (tree (eql '())))
  tree)

(defmethod transform-tree ((op (eql 'fixup-arglists)) (tree op-node))
  (let ((operator (op-node-op tree))
        (lhs (op-node-lhs tree))
        (rhs (op-node-rhs tree)))
    (if (or (equal operator "(")
            (equal operator "["))
        (let ((box (separated-list? (meta-sexp:create-parser-context rhs) :comma)))
          (unless box
            (error "RHS ~S for operator '~A' is not a comma-separated list" rhs operator))
          (make-op-node :op operator
                        :lhs (transform-tree op lhs)
                        :rhs (mapcar (lambda (n) (transform-tree op n))
                                     (list-box-list box))))
        (make-op-node :op operator
                      :lhs (transform-tree op lhs)
                      :rhs (transform-tree op rhs)))))

(defmethod transform-tree ((op (eql 'collect-db-fields)) (tree (eql '())))
  nil)

(defmethod transform-tree ((op (eql 'collect-db-fields)) (tree op-node))
  "Convert [a.]b.c references into database field accesses."
  ;; This is a little state machine:
  ;; q0 x -> q0 ; regular transform-tree recursion
  ;; q0 (op-node :op ".") -> q1
  ;; q1 (op-node :op ".") -> q1 ; accumulation recursion
  ;; q1 x -> q0 ; out of accumulation, back to regular transform-tree
  (labels ((accum-field-refs (tree)
             (cond
               ((and (typep tree 'op-node)
                     (equal (op-node-op tree) "."))
                (cons (transform-tree op (op-node-rhs tree))
                      (accum-field-refs (op-node-lhs tree))))
               (t (list tree)))))
    (let ((operator (op-node-op tree))
          (lhs (op-node-lhs tree))
          (rhs (op-node-rhs tree)))
      (if (equal operator ".")
          (make-field-ref :parts (accum-field-refs tree))
          (make-op-node :op operator
                        :lhs (transform-tree op lhs)
                        :rhs (transform-tree op rhs))))))

(defgeneric compile-to-lisp (node &rest r)
  (:documentation "Given an AST node NODE, produce the equivalent lisp code for that node."))

;; TODO: figure out how to deal with packages here
(defmethod compile-to-lisp ((node symb) &key)
  (make-symbol (symb-name node)))

(defmethod compile-to-lisp ((node number-value) &key)
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

(defmethod compile-to-lisp ((node date-value) &key)
  (encode-abl-date (date-value-day node)
                   (date-value-month node)
                   (date-value-year node)))

(defmethod compile-to-lisp ((node time-value) &key)
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
(defmethod compile-to-lisp ((node datetime-value) &key)
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


(defun defaulted-padding-opts (node &rest defaults &key size justify)
  (declare (ignore size justify))
  (let ((node-justify (string-value-justify node))
        (node-size (string-value-reserved node)))
    (when node-justify
      (setf (getf defaults :justify) node-justify))
    (when node-size
      (setf (getf defaults :size) node-size))
    defaults))

;; TODO: Deal with :translatable, probably via cl-i18n
;; Produce a padded string, using :size and :justify arguments as defaults
(defmethod compile-to-lisp ((node string-value) &rest opts &key size justify)
  (declare (ignore size justify))
  (let ((defaulted-opts (apply #'defaulted-padding-opts node opts)))
    (apply #'vrt:make-padded-string (string-value-str node) defaulted-opts)))

;; TODO: figure out symbol packages here (just use the runtime
;; package, inherits symbols from CL will work)
(defmethod compile-to-lisp ((node op-node) &key)
  (let ((op (op-node-op node)))
    (cond
      ;; Function call
      ;; TODO: add support for foo:bar() methods without using funcall

      ((equal op "(") (let ((lhs (op-node-lhs node))
                            (rhs (op-node-rhs node)))
                        (if (symb-p lhs)
                            (cons (compile-to-lisp lhs)
                                  (mapcar #'compile-to-lisp rhs))
                            (cons '#:funcall
                                  (mapcar #'compile-to-lisp
                                          (cons lhs rhs))))))
      ((equal op "[") (let ((lhs (op-node-lhs node))
                            (rhs (op-node-rhs node)))
                        (cons '#:aref
                              (mapcar #'compile-to-lisp
                                      (cons lhs rhs)))))
      (t (list (make-symbol (op-node-op node))
               (compile-to-lisp (op-node-lhs node))
               (compile-to-lisp (op-node-rhs node)))))))

;; TODO: Handle converting case and hyphenation to lisp-names
(defmethod compile-to-lisp ((node symb) &key)
  (make-symbol (symb-name node)))

(defmethod compile-to-lisp ((node unary-op-node) &key)
  (list (make-symbol (unary-op-node-op node))
        (compile-to-lisp (unary-op-node-val node))))
