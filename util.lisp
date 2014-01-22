(in-package :vertigo)

(defun join-list (delimiter list)
  (loop for x on list
     collect (car x)
     if (cdr x)
     collect delimiter))

(defun list-length-p (n list)
  (when (and (= n 0)
             (endp list))
    (return-from list-length-p t))
  (loop for c on list
     for i from 1 to n
     finally (return (and (= i n)
                          (endp (cdr c))))))

(defun to-bool (val)
  (if val t nil))

(defun pdict (&rest keys)
  (let ((m (make-instance 'cl-persist:persistent-map)))
    (apply #'cl-persist:add m keys)))

(defun dict (&rest keys)
  (list->dict keys))

(defun list->dict (list)
  (loop for (key . rest) on list by #'cddr
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

(defun thread-it (&rest funcs)
  (lambda (val)
    (reduce (lambda (v f)
              (funcall f v))
            funcs
            :initial-value val)))

(defun -> (initial &rest funcs)
  (funcall (thread-it funcs) initial))

(defun slurp-file (path)
  (with-open-file (fh path)
    (let ((str (make-string (file-length fh))))
      (read-sequence str fh)
      str)))
