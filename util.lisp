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
