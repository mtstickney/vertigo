(in-package #:vertigo)

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited)) &optional args)
  (let ((delimiter (first args)))
    (transform-grammar ret ctx t :and
                       `(,@(cdr args)
                           (:* ,delimiter
                               ,@(cdr args))))))
