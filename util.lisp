(in-package :vertigo)

(defun join-list (delimiter list)
  (loop for x on list
     collect (car x)
     if (cdr x)
     collect delimiter))

(defun dict (&rest keys)
  (loop for (key . rest) on keys by #'cddr
     with m = (make-hash-table :test 'equal)
     if (endp rest)
     do (error "Even number of arguments required for dict constructor.")
     do (setf (gethash key m) (car rest))
     finally (return m)))

(defun dict->assoc (d)
  (let ((l '()))
    (maphash (lambda (k v)
               (push (cons k v) l))
             d)
    l))

(defun stmt->assoc (s)
  (dict->assoc (statement-data s)))

(defun prompt-for-val (prompt)
  (format *query-io* (format *query-io* "~&~A: " prompt))
  (multiple-value-list (eval (read))))

(defun prompt-until (prompt &optional (test (constantly t)))
  (loop for val = (prompt-for-val prompt)
     until (funcall test val)
     finally (return val)))

(defun -> (initial &rest funcs)
  (reduce (lambda (v f)
            (funcall f v))
          funcs
          :initial-value initial))
