(in-package #:vertigo-runtime)

;; The behavior here is a little odd (:trim doesn't behave like :left
;; and :right wrt :size, etc), but it's what ABL does.
(defun make-padded-string (str &rest args &key (size (length str)) (justify :left))
  (check-type str string)
  (check-type size (integer 0))
  (check-type justify keyword)

  (let* ((str (if (eq justify :trim)
                  (string-trim '(#\Space #\Tab) str)
                  str))
         (str-size (length str))
         (padding-size (max 0 (- size str-size))))
    (cond
      ((= size str-size) str)
      ((< size str-size) (subseq str 0 size))
      ((> size str-size)
       (ecase justify
         (:left (let ((s (make-string size)))
                  (replace s str)
                  (fill s #\Space :start str-size)
                  s))
         (:right (let ((s (make-string size)))
                   (fill s #\Space :end padding-size)
                   (replace s str :start1 padding-size)
                   s))
         (:center (let ((s (make-string size))
                        (left-size (floor padding-size 2)))
                    (fill s #\Space :end left-size)
                    (replace s str :start1 left-size)
                    (fill s #\Space :start (+ left-size str-size))
                    s))
         ;; Note: the TRIM function does more than space and tab, but
         ;; the literal version only appears to trim those two. Also
         ;; note that string has already been trimmed.
         (:trim str))))))
