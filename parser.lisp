(in-package #:vertigo)

;; TODO: this should go in a global utils file
(defun join-list (list delimiter)
  (loop for x on list
     collect (car x)
     if (cdr x)
     collect delimiter))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited)) &optional args)
  (let ((delimiter (first args)))
    (meta-sexp:transform-grammar ret ctx t :checkpoint (join-list (cdr args) delimiter))))

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

(meta-sexp:defrule whitespace? () ()
  (:+ (:type meta-sexp:white-space?)))
