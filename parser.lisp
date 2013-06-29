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
    (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint
                                                  args)))
      `(let ,(reverse *bind-vars*)
         ,body-code))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :bind)) &optional args)
  (declare (special *bind-vars*))
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

(meta-sexp:defrule whitespace? () ()
  (:+ (:type meta-sexp:white-space?)))
