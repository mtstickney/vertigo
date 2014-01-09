;;;; ablisp.lisp

(in-package #:ablisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-range (a b)
    (loop for i from (char-code a) to (char-code b)
       collect (code-char i))))

(deftype caps () `(member ,@(char-range #\A #\Z)))
(deftype lowercase () `(member ,@(char-range #\a #\z)))
(deftype letter () `(or caps lowercase))
(deftype identifier-char () `(or letter meta-sexp:digit? (member #\- #\_)))
(deftype keyword-char () `(or caps meta-sexp:digit? (member #\- #\_)))
;; isspace()-equivalent whitespace chars
(deftype whitespace () `(member #\Space
                                #\Tab
                                #\Linefeed
                                ,(code-char #x0b) ;; vertical tab
                                ,(code-char #x0c) ;; form feed
                                ,(code-char #x0d) ;; CR
                                #\No-break_space  ;; non-breaking
                                ;; space, probably not very portable
                                ))
(deftype non-whitespace () `(not whitespace))

(defun is-not-squote (c)
  (not (eql c #\')))
(deftype non-squote () `(satisfies is-not-squote))
(defun is-not-dquote (c)
  (not (or  (eql c #\")
            (eql c #\Left_double_quotation_mark)
            (eql c #\Right_double_quotation_mark))))
(deftype non-dquote () `(satisfies is-not-dquote))

(defun is-not-bquote (c)
  (not (eql c #\`)))
(deftype non-bquote () `(satisfies is-not-bquote))

(defun unreserved-char-p (c)
  (not (find c "`[]{}|")))
(deftype unreserved () '(satisfies unreserved-char-p))

(deftype non-whitespace-unreserved () '(and non-whitespace unreserved))

(deftype literal-char () '(member #\& #\+ #\- #\/ #\*
                           #\, #\: #\( #\) #\=
                           #\" #\Left_double_quotation_mark
                           #\Right_double_quotation_mark))

(defstruct (ast-node (:conc-name node-))
  string
  type
  datum)

(defun dbg (msg &rest args)
  (declare (ignorable msg args))
  (progn
    ;;(apply #'warn (cons msg args))
    nil))

(defmacro mdo (&body body)
  `(progn
     ,@body
     t))



(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :mdo)) &optional args)
  (declare (ignore ret ctx))
  `(progn
     (meta-sexp:meta ,@args)
     t))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :cursor)) &optional args)
  (declare (ignore args))
  `(meta-sexp::parser-context-cursor ,ctx))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :data)) &optional args)
  (declare (ignore args))
  `(meta-sexp::parser-context-data ,ctx))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :matched-since)) &optional args)
  `(subseq ,(meta-sexp:transform-grammar ret ctx t :data)
           ,(first args)
           ,(meta-sexp:transform-grammar ret ctx t :cursor)))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :with-stored-match)) &optional args)
  (declare (special *debug*))
  (let* ((index-var (gensym))
         (match-save (meta-sexp:transform-grammar
                      ret ctx t :assign
                      (list (first args)
                            (meta-sexp:transform-grammar ret ctx t :matched-since (list index-var))))))
    `(if *debug*
         (let ((,index-var ,(meta-sexp:transform-grammar ret ctx t :cursor)))
           ;; (:and ,@body (:assign var (:matched-since index)))
           ,(meta-sexp:transform-grammar ret ctx t :and
                                         (concatenate 'list (cdr args)
                                                      (list match-save))))
         ,(meta-sexp:transform-grammar ret ctx t :and (cdr args)))))


;; Enable debug string saving in the following definitions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug* nil))

;; choice-expression ::= sequence-expression { [ whitespace ] "|" [
;; whitespace ] sequence-expression }*
;; sequence-expression ::= repeat-expression { [ whitespace ]
;; repeat-expression }*
;; repeat-expression ::= primary [ whitespace ] ["..."]
;; primary ::= { var-slot | group | option | keyword | literal }
;; var-slot ::= "`" { [whitespace] { identifier | keyword | literal }
;; }+ "`"
;; group ::= "{" [whitespace] choice-expression [whitespace] "}"
;; option ::= "[" [whitespace] choice-expression [whitespace] "]"
;; keyword ::= @CAPS @KEYWORD-CHAR*
;; identifier ::= @LETTER @IDENTIFIER-CHAR
;; literal ::= @NON-WHITESPACE-NON-RESERVED+
(meta-sexp:defrule choice-expression? (&aux matched expr (seq (meta-sexp:make-list-accum))) ()
  (:with-stored-match (matched)
    (:? (:rule whitespace?))
    (:assign expr (:rule sequence-expression?))
    (:list-push expr seq)
    (:* (:? (:rule whitespace?)) "|" (:? (:rule whitespace?))
        (:assign expr (:rule sequence-expression?))
        (:list-push expr seq))
    (:? (:rule whitespace?)))

  (:return (make-ast-node :type :choice
                          :string matched
                          :datum (nreverse seq))))


(meta-sexp:defrule sequence-expression? (&aux matched item (seq (meta-sexp:make-list-accum))) ()
  (:with-stored-match (matched)
    (:+
     (:assign item (:rule repeat-expression?))
     (:list-push item seq)
     (:? (:assign item (:rule whitespace?))
         (:list-push item seq))))

  (:return (make-ast-node :type :sequence
                          :string matched
                          :datum (nreverse seq))))

(meta-sexp:defrule repeat-expression? (&aux matched item repeat-p) ()
  (:with-stored-match (matched)
    (:assign item (:rule primary?))
    (:? (:rule whitespace?))
    (:? "..."
        (:assign repeat-p t)))
  (mdo
    (when repeat-p
      (setf item (make-ast-node :type :repeat
                                :string matched
                                :datum item))))
  (:return item))

(meta-sexp:defrule var-slot? (&aux matched item (seq (meta-sexp:make-list-accum))) ()
  (:with-stored-match (matched)
    #\`
    (:+
     (:assign item (:rule non-keyword?))
     (:list-push item seq)
     (:?
      (:assign item (:rule whitespace?))
      (:list-push item seq)))
    #\`)

  (:return (make-ast-node :type :sequence
                          :string matched
                          :datum (nreverse seq))))

(meta-sexp:defrule non-keyword? () ()
  (:or (:rule identifier?)
       (:rule misc-literal?)))

(meta-sexp:defrule quoted-literal? (&aux c (seq (meta-sexp:make-char-accum))) ()
  #\'
  (:+ (:assign c (:type non-squote))
      (:char-push c seq))
  #\'

  (:return (make-ast-node :type :literal
                          :string seq
                          :datum seq)))

(meta-sexp:defrule primary? () ()
  (:or (:rule var-slot?)
       (:rule group?)
       (:rule option?)
       (:rule keyword?)
       (:rule quoted-literal?)
       (:rule non-keyword?)))

(meta-sexp:defrule group? (&aux expr matched) ()
  (:with-stored-match (matched)
    #\{
    (:? (:rule whitespace?))
    (:assign expr (:rule choice-expression?))
    (:? (:rule whitespace?))
    #\})

  ;; We've already got the contents of a group as a single expr, no
  ;; reason to keep the group itself.
  (:return expr))

(meta-sexp:defrule option? (&aux expr matched) ()
  (:with-stored-match (matched)
    #\[
    (:? (:rule whitespace?))
    (:assign expr (:rule choice-expression?))
    (:? (:rule whitespace?))
    #\])

  (:return (make-ast-node :type :optional
                          :string matched
                          :datum expr)))

(meta-sexp:defrule whitespace? (&aux c (str (meta-sexp:make-char-accum))) ()
  (:+ (:assign c (:type whitespace))
      (:char-push c str))
  (:return (make-ast-node :type :relax
                          :string str
                          :datum :relax)))

(meta-sexp:defrule keyword? (&aux c (str (meta-sexp:make-char-accum))) ()
  (:assign c (:type caps))
  (:char-push c str)
  (:* (:assign c (:type keyword-char))
      (:char-push c str))
  (make-ast-node :type :keyword
                 :string str
                 :datum str))

(meta-sexp:defrule misc-literal? (&aux c (str (meta-sexp:make-char-accum))) ()
  (:+ (:assign c (:type literal-char))
      (:char-push c str))
  (:return (make-ast-node :type :literal
                          :string str
                          :datum str)))

(meta-sexp:defrule identifier? (&aux c (str (meta-sexp:make-char-accum))) ()
  (:assign c (:type letter))
  (:char-push c str)
  (:* (:assign c (:type identifier-char))
      (:char-push c str))
  (:return (make-ast-node :type :identifier
                          :string str
                          :datum str)))



(defun ast-map (fn ast)
  (check-type ast ast-node)
  (check-type fn function)
  (labels ((datum-map (fn datum)
             (etypecase datum
               (ast-node
                (let ((new-node (copy-ast-node datum))
                      (new-datum (datum-map fn (node-datum datum))))
                  (setf (node-datum new-node) new-datum)
                  ;; What happens if we do a datum-map here?
                  (funcall fn new-node)))
               (list
                (mapcar #'(lambda (d) (if (ast-node-p d)
                                          (ast-map fn d)
                                          d))
                        datum))
               (t datum))))
    (datum-map fn ast)))

;; Verify checks
;; TODO: check for types of children (:literal/:keyword has string,
;; :choice/:sequence has list of ast-nodes, etc.)

(defun group-in-group (node)
  (when (and (eq (node-type node) :group)
             (eq (node-type (node-datum node)) :group))
    (warn "Group in group at node ~S" node))
  node)

;; Simplification transforms
(defun hoist-singleton (node)
  (if (and (member (node-type node) '(:choice :sequence))
           (and (=  (length (node-datum node)) 1)))
      (first (node-datum node))
      node))

(defun flatten-sequences (node)
  (cond
    ((and (eq (node-type node) :sequence))
     (let ((result '())
           (new-node (copy-ast-node node)))
       (loop for d in (node-datum node)
          do (if (eq (node-type d) :sequence)
                 (loop for child-d in (node-datum d)
                    do (push child-d result))
                 (push d result)))
       (setf (node-datum new-node) (nreverse result))
       new-node))
    (t node)))

(defun simplify-ast (ast)
  (check-type ast ast-node)
  (let ((transforms (list #'hoist-singleton
                          #'flatten-sequences
                          #'group-in-group)))
    (reduce #'(lambda (tree fn)
                (ast-map fn tree))
            transforms
            :initial-value ast)))

(defun match-has-warning (path)
  (handler-case (progn (simplify-ast (mmatch-file #'choice-expression? path))
                       nil)
    (warning (c)
      (declare (ignore c))
      (return-from match-has-warning t))))

(defun find-quoted-whitespace (file)
  (loop while (listen file)
     with in-quote = nil
     do (let ((c (read-char file)))
          (cond
            ((and in-quote (typep c 'whitespace))
             (return t))
            ((eql c #\`)
             (setf in-quote (not in-quote)))))))

;; The matcher-compiler AST node types: '(:choice :sequence :repeat
;; :literal :optional :relax :keyword :identifier)
(defgeneric compile-matcher-ast (type datum))

(defmethod compile-matcher-ast ((type (eql :keyword)) (datum string))
  (string-upcase datum))

(defmethod compile-matcher-ast ((type (eql :literal)) (datum string))
  datum)

(defmethod compile-matcher-ast ((type (eql :relax)) datum)
  (declare (ignore datum))
  `(:? (:rule whitespace?)))

(defmethod compile-matcher-ast ((type (eql :optional)) (datum ast-node))
  `(:? ,(compile-matcher-ast (node-type datum) (node-datum datum))))

(defmethod compile-matcher-ast ((type (eql :sequence)) (datum list))
  `(:and ,@(loop for d in datum
              collect (compile-matcher-ast (node-type d) (node-datum d)))))

(defmethod compile-matcher-ast ((type (eql :repeat)) (datum ast-node))
  `(:* ,(compile-matcher-ast (node-type datum) (node-datum datum))))

(defmethod compile-matcher-ast ((type (eql :choice)) (datum list))
  `(:or ,@(loop for d in datum
             collect (compile-matcher-ast (node-type d) (node-datum d)))))

(defun phrase-identifier-p (identifier)
  (let ((index (search "-phrase" identifier :from-end t :test #'equalp)))
    (and index
         (= index
            (- (length identifier)
               (length "-phrase"))))))

(defun make-rule-symbol (pkg)
  (intern (concatenate 'string
                       (symbol-name
                        (gensym "RULE"))
                       "?")
          pkg))

(defun rule-name (identifier pkg)
  (declare (special *identifier-rules*))
  (let ((rule-symb (make-rule-symbol pkg)))
    (setf (gethash rule-symb *identifier-rules*) identifier)
    rule-symb))

(defmethod compile-matcher-ast ((type (eql :identifier)) (datum string))
  (declare (special *matcher-package*))
  `(:rule ,(rule-name datum *matcher-package*)))

(defun compile-ast (ast)
  "Return the code to parse the syntax described by AST."
  (check-type ast ast-node)
  (let ((*identifier-rules* (make-hash-table))
        (ast (funcall (mts.util:compose #'simplify-ast)
                      ast)))
    (declare (special *identifier-rules*))
    (values (compile-matcher-ast (node-type ast) (node-datum ast))
            *identifier-rules*)))

(defun pprint-defrule (*standard-output* form)
  (pprint-logical-block (nil form :prefix "(" :suffix ")")
    ;; DEFRULE symbol
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)

    ;; Rule name
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-newline :fill)

    ;; Rule args
    (let ((thing (pprint-pop)))
      (if (null thing)
          (write-string "()")
          (write thing)))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-newline :fill)

    ;; Rule attachment var
    (let ((thing (pprint-pop)))
      (if (null thing)
          (write-string "()")
          (write thing)))

    ;; Force the body to be on a new line, even if it's short
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 1)
    (pprint-newline :mandatory)
    (write (pprint-pop))
    (loop
       (pprint-exit-if-list-exhausted)
       (write-char #\Space)
       (pprint-newline :linear)
       (write (pprint-pop)))))

(defun format-ast-primary (stream match-body &optional title)
  (declare (special *matcher-package*))
  (let ((*print-case* :downcase))
    (when title
      (format stream ";; ~A~%" title))
    (pprint-defrule
     stream
     `(meta-sexp:defrule ,(make-rule-symbol *matcher-package*) () ()
        ,match-body))
    (write-char #\Newline stream)))

(defun format-identifier-rules (stream rules)
  (let ((*print-case* :downcase))
    (maphash (lambda (k v)
               (format stream "~%;; ~A~%" v)
               (format stream "(meta-sexp:defrule ~S () ()~%)~%" k))
             rules)))

(defun parse-file (path &optional (fn #'choice-expression?))
  "Parse the syntax description file PATH into an AST."
  (with-open-file (fh path :direction :input :element-type '(unsigned-byte 8))
    (funcall fn (meta-sexp:create-parser-context
                 (trivial-utf-8:read-utf-8-string fh
                                                  :stop-at-eof t)))))

(defun compile-syntax-page-to-stream (path stream)
  ;; TODO: This is a hack to avoid printing package names in output
  ;; files by using the current package. Fix it.
  (let ((*matcher-package* *package*))
    (declare (special *matcher-package*))
    (format stream "~%;;; ~A~%" (mts.util:page-title path))
    (loop for e in (mts.util:syntax-entries path)
       for i from 1
       do (multiple-value-bind (match-body id-rules)
              (compile-ast (parse-file e))
            (when (> i 1)
              (format stream "~%~%"))
            (format-ast-primary stream match-body
                                (format nil "Form no. ~A" i))
            (format-identifier-rules stream id-rules)))))

(defun compile-pages-to-file (path pages)
  (with-open-file (out path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (setf out (flexi-streams:make-flexi-stream out :external-format :utf-8))
    (loop for p in pages
       do (compile-syntax-page-to-stream p out))))

(defun compile-tree2 (src dest)
  "Compile a tree of syntax descriptions, SRC into Lisp parser files in DEST."
  (let ((pages (remove-if-not #'cl-fad:directory-pathname-p
                              (cl-fad:list-directory src))))
    (multiple-value-bind (fn-pages
                          statement-pages
                          handle-pages
                          operator-pages
                          preproc-pages
                          phrase-pages
                          misc-pages)
        (mts.util:set-partition pages (list #'mts.util:fn-page-p
                                   #'mts.util:statement-page-p
                                   #'mts.util:handle-page-p
                                   #'mts.util:operator-page-p
                                   #'mts.util:preproc-page-p
                                   #'mts.util:phrase-page-p))
      (let ((fn-statement-pages (mts.util:fn-statement-pages fn-pages statement-pages)))
        (setf statement-pages (set-difference statement-pages fn-statement-pages :test #'equal))
        (macrolet ((compile-pages ((dest) &body pages)
                     `(progn
                        ,@(loop for (file ps) in pages
                             collect `(compile-pages-to-file
                                       (merge-pathnames ,file ,dest)
                                       ,ps)))))
          (compile-pages (dest)
                         ("functions.lisp" fn-pages)
                         ("statements.lisp" statement-pages)
                         ("function-statements.lisp" fn-statement-pages)
                         ("handles.lisp" handle-pages)
                         ("operators.lisp" operator-pages)
                         ("preprocessor.lisp" preproc-pages)
                         ("phrases.lisp" phrase-pages)
                         ("other.lisp" misc-pages)))))))

;;; "ablisp" goes here. Hacks and glory await!
