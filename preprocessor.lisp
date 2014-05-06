(in-package :vertigo)

;;; Stuff for special parser contexts

(defclass text-position-mixin ()
  ;; Interesting note: if this is initialized to 0, then line-num is
  ;; the number of unix-style lines we've read (i.e. "it's not a line
  ;; unless it ends with a newline", a la ed(1)).
  ((line :accessor line-num :initarg :line)
   (column :accessor col-num :initarg :column)
   (line-ending-style :accessor eol-style :initarg :eol-style)
   (carriage-return-p :accessor returnp :initform nil)
   (in-newline-p :accessor newlinep :initform nil)
   (checkpoint-stack :accessor checkpoints :initform nil))
  (:default-initargs :eol-style :unix
    :line 1
    :column 0))

;; The spec:

;; 1. Every character (including newlines) increments the column (CRLF
;; counts as one character, and doesn't increment until both
;; characters have been read).

;; 2. If the last character read was a newline, the next character
;; read (including newlines) will be at the next line number, column
;; 1.
(defgeneric process-char (context char)
  (:documentation "Process the CHAR, updating file position state as necessary.")
  (:method ((context text-position-mixin) char)
    (check-type char character)
    (check-type (eol-style context) (member :unix :dos :both))
    (let* ((mode (eol-style context))
           (crlfp (member mode '(:dos :both))))
      ;; Before we do anything else, CR followed by LF is a NOP
      ;; character, so just set returnp and bail.
      (when (and crlfp
                 (eql char #\Return)
                 (eql (meta-sexp:peek-atom context) #\Linefeed))
        (setf (returnp context) t)
        (return-from process-char (values)))

      ;; If we just saw a newline, increment the line number and make
      ;; sure the column increment will land at 1. Also reset the
      ;; newline state (it will be re-set if necessary)
      (when (newlinep context)
        (setf (col-num context) 0
              (newlinep context) nil)
        (incf (line-num context)))
      ;; Always increment
      (incf (col-num context))
      ;; Now see if /this/ character is a newline
      (when (and (eql char #\Linefeed)
                 (or (not (eq mode :dos))
                     (returnp context)))
        (setf (newlinep context) t))

      ;; Anything other than an \r\n clears returnp
      (setf (returnp context) nil))))


(defmethod meta-sexp:read-atom :around ((ctx text-position-mixin))
  (multiple-value-bind (atom eofp) (call-next-method)
    (unless eofp
      (process-char ctx atom))
    (values atom eofp)))

(defmethod meta-sexp:checkpoint :after ((ctx text-position-mixin))
  (push (list (line-num ctx)
              (col-num ctx)
              (returnp ctx)
              (newlinep ctx))
        (checkpoints ctx)))

(defmethod meta-sexp:commit :after ((ctx text-position-mixin))
  (pop (checkpoints ctx)))

(defmethod meta-sexp:rollback :after ((ctx text-position-mixin))
  (destructuring-bind (line col ret newline) (first (checkpoints ctx))
    (setf (line-num ctx) line
          (col-num ctx) col
          (returnp ctx) ret
          (newlinep ctx) newline)))

(defclass counting-string-context (meta-sexp::string-parser-context text-position-mixin)
  ())

;; We want to use counting-string-context by default for strings
(defmethod meta-sexp:create-parser-context :around
    ((input string) &key attachment (end (length input)) (start 0) (line 1) (column 0) (eol-style :both))
  (make-instance 'counting-string-context
                 :data input
                 :attachment attachment
                 :start start
                 :size end
                 :attachment attachment
                 :eol-style eol-style
                 :column column
                 :line line
                 :column column))

(defun test-context (mode str)
  (let ((context (make-instance 'counting-context
                                :data str
                                :start 0
                                :size (length str)
                                :eol-style mode)))
    (loop for atom = (meta-sexp:read-atom context)
       while atom)
    (list :line (line-num context) :col (col-num context) :returnp (returnp context) :newlinep (newlinep context))))
