(in-package #:vertigo)

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited)) &optional args)
  (let ((delimiter (first args)))
    (meta-sexp:transform-grammar ret ctx t :checkpoint (join-list delimiter (cdr args)))))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited*)) &optional args)
  (let ((delimiter (first args))
        (term-items (cdr args)))
    (meta-sexp:transform-grammar ret ctx t :checkpoint
                                 `(,@term-items
                                   (:* (:checkpoint ,delimiter
                                                    ,@term-items))))))

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
  (let* ((index-var (gensym))
         (match-save (meta-sexp:transform-grammar
                      ret ctx t :assign
                      (list (first args)
                            (meta-sexp:transform-grammar ret ctx t :matched-since (list index-var))))))
    `(let ((,index-var ,(meta-sexp:transform-grammar ret ctx t :cursor)))
       ;; (:and ,@body (:assign var (:matched-since index)))
       ,(meta-sexp:transform-grammar ret ctx t :and
                                     (concatenate 'list (cdr args)
                                                  (list match-save))))))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :n-times)) &optional args)
  (let ((count (first args)))
    (meta-sexp:transform-grammar ret ctx t :and (loop for i from 1 to count
                                                   append (cdr args)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-binds)) &optional args)
  (let ((*bind-vars* '()))
    (declare (special *bind-vars*))
    ;; Reserve first arg for future arglist (e.g (:with-binds () ...))
    (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint
                                                  (cdr args))))
      `(let ,(reverse *bind-vars*)
         ,body-code))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :bind)) &optional args)
  (declare (special *bind-vars*))
  (when (not (boundp '*bind-vars*))
    (error ":BIND outside of :WITH-BINDS form"))
  (flet ((var-spec-var (spec) (if (consp spec) (first spec) spec)))
    (destructuring-bind (form var-spec &aux (var (var-spec-var var-spec))) args
      (restart-case (progn
                      (when (member var *bind-vars*
                                    :key #'var-spec-var)
                        (error "Variable ~S is already bound by a :BIND form~%" var))
                      (push var-spec *bind-vars*)
                      (meta-sexp:transform-grammar ret ctx t :assign
                                                   (list var form)))
        (use-value (new-var-spec)
          :report "Use a different variable spec"
          :interactive (lambda ()
                         (prompt-until "Enter a new variable spec"
                                       (lambda (s)
                                         (let ((primary-value (first s)))
                                           ;; True if either it's a
                                           ;; var or a (var binding)
                                           ;; form
                                           (or (symbolp primary-value)
                                               (and (listp primary-value)
                                                    (symbolp (first primary-value))
                                                    (endp (cddr primary-value)))
                                               (format *query-io* "~&Not a valid variable spec, try again.~%"))))))

          (setf var-spec new-var-spec))))))

;; TODO: the actual dict appears to be lexically scoped; is this a
;; problem for e.g. cross-rule binds?
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-dict-binds)) &optional args)
  (let ((*dict-var* (gensym "DICT")))
    (declare (special *dict-var*))
    ;; First arg is reserved for future arguments
    (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint (cdr args))))
      `(let ((,*dict-var* (make-instance 'cl-persist:persistent-map)))
         (and ,body-code
              ,*dict-var*)))))

;; TODO: :bind/:kbind checkpointing (only do assigns if all
;; checkpointed forms succeed.
;; TODO: what about if/when a key is non-literal? *bind-keys* probably
;; breaks, ',key stops being legit...
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :kbind)) &optional args)
  (declare (special *dict-var*))
  (when (not (boundp '*dict-var*))
    (error ":KBIND outside of :WITH-DICT-BINDS form"))
  (destructuring-bind (form key &optional (key-func (quote #'identity))) args
    (restart-case (progn
                    (let ((form-code (meta-sexp:transform-grammar ret ctx t form))
                          (result-var (gensym "RESULT")))
                      `(let ((,result-var ,form-code))
                         (when ,result-var
                           (setf ,*dict-var*
                                 (cl-persist:add ,*dict-var* ',key (funcall ,key-func ,result-var)))
                           ,result-var))))
        (use-value (new-key)
          :report "Use a different key"
          :interactive (lambda ()
                         (prompt-for-val "Enter a new key"))
          (setf key new-key)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :dict-checkpoint)) &optional args)
  (declare (special *dict-var*))
  (when (not (boundp '*dict-var*))
    (error ":DICT-CHECKPOINT outside of :WITH-DICT-BINDS form"))
  (let ((stored-dict-var (gensym "STORED-DICT"))
        (result-var (gensym "RESULT"))
        (body-code (meta-sexp:transform-grammar ret ctx t :and args)))
    `(let ((,stored-dict-var ,*dict-var*))
       (let ((,result-var ,body-code))
         (unless ,result-var
           (setf ,*dict-var* ,stored-dict-var))
         ,result-var))))

;; Combined dict/parser checkpoint
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :checkpoint*)) &optional args)
  (declare (special *dict-var*))
  (meta-sexp:transform-grammar ret ctx t :checkpoint
                               (if (boundp '*dict-var*)
                                   (list (cons :dict-checkpoint args))
                                   args)))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :key-flags)) &optional args)
  (meta-sexp:transform-grammar ret ctx t :and
                               (loop for spec in args
                                  collect (if (listp spec)
                                              `(:opt (:kbind,(first spec) ,(second spec) (constantly t)))
                                              `(:opt (:kbind ,spec ,spec (constantly t)))))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :opt)) &optional args)
  (if (list-length-p 1 args)
      ;; We assume a singleton form doesn't any needed checkpointing
      (meta-sexp:transform-grammar ret ctx t :? args)
      (meta-sexp:transform-grammar ret ctx t :? (list (cons :checkpoint* args)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :k)) &optional args)
  (if (list-length-p 1 args)
      ;; Single argument doesn't need an extra checkpoint
      (meta-sexp:transform-grammar ret ctx t :rule
                                   (cons 'keyword? args))
      (meta-sexp:transform-grammar ret ctx t :checkpoint
                                   (mapcar (lambda (form)
                                             `(:rule keyword? ,form))
                                           args))))

;; Match a keyword as a :k form
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive symbol) &optional args)
  "Transforms a symbol form (only keywords are transformed, other symbols are left alone)."
  (declare (ignore args))
  (if (keywordp directive)
      (meta-sexp:transform-grammar ret ctx t :k (list (symbol-name directive)))
      directive))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :whole-match)) &optional args)
  "Transform for matching a form that must consume all remaining input."
  (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint args))
        (post-code (meta-sexp:transform-grammar ret ctx t :not
                                                (list '(:type character))))
        (result-var (gensym "RESULT")))
    `(let ((,result-var ,body-code))
       (and ,post-code ,result-var))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :eof)) &optional args)
  (meta-sexp:transform-grammar ret ctx t :not (list '(:type character))))

(deftype whitespace-char ()
  '(or (eql #\Tab)
    (eql #\Newline)
    (eql #\Linefeed)
    (eql #\Page)
    (eql #\Return)
    (eql #\Space)))

;; TODO: Not all of these character names are portable, use codes for
;; everything but #\Newline and #\Space
(meta-sexp:defrule whitespace? () ()
  (:+ (:type whitespace-char)))
