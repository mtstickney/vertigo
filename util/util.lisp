(defpackage #:mts.util
  (:use #:cl)
  (:export #:values-of
           #:compose
           #:set-exclude
           #:set-partition
           #:list-tree
           #:fn-page-p
           #:syntax-entries
           #:count-syntax-entries
           #:page-title
           #:tree-pages
           #:fn-page-fn
           #:title-begins-p
           #:title-ends-p
           #:statement-page-p
           #:statement-page-statement
           #:fn-statement-page-p
           #:fn-statement-pages
           #:handle-page-p
           #:operator-page-p
           #:preproc-page-p
           #:phrase-page-p
           #:slurp-file
           ))

(in-package :mts.util)

(defmacro values-of (form)
  `(apply #'values (multiple-value-list ,form)))

;; TODO: let function slots be a cons of (FN WRAPPER) where wrapper is
;; used to control the number/order of return values from FN.
(defun compose (f &rest gs)
  (if (endp gs)
      (lambda (&rest args) (apply f args))
      (lambda (&rest args)
        (apply f (multiple-value-list
                  (apply (apply #'compose gs) args))))))

(defun list-tree (dir &rest args)
  (let ((lst '()))
    (apply #'cl-fad:walk-directory
           dir
           (lambda (p) (push p lst))
           args)
    lst))

;;; A "page" as used here is the folder for each dvref page as
;;; produced by docstrip.py (e.g. #p"tree/dvref-08-33/").
(defun syntax-entries (page)
  "Return a list of the syntax entry files that are part of PAGE."
  (let ((entries '()))
    (cl-fad:walk-directory
     page
     (lambda (p) (push p entries))
     :test (lambda (p) (not (equal (pathname-name p) "title"))))
    entries))

(defun page-title (p)
  "Return the title for page P. The title of a page is the page's header, not the filename."
  (let ((title-path (merge-pathnames #p"title" p)))
    (with-open-file (fh title-path :element-type '(unsigned-byte 8))
      (trivial-utf-8:read-utf-8-string fh :stop-at-eof t))))

(defun tree-pages (p)
  "Return a list of all the pages in the tree P, as produced by docstrip.py"
  (let ((pages '()))
    (cl-fad:walk-directory
     p (lambda (p) (push p pages))
     :directories t
     :test #'cl-fad:directory-pathname-p)))

(defun title-ends-p (p str)
  "Return t if P's page title ends with STR, nil otherwise."
  (let* ((title (page-title p))
         (index (search str title :from-end t)))
    (and index
         (= index
            (- (length title)
               (length str))))))

(defun fn-page-p (p)
  "Return t if the page is for a function, nil otherwise."
  ;; page-title must end with "function"
  (title-ends-p p "function"))

(defun fn-page-fn (p)
  "Return the function name of a function page."
  (let* ((title (page-title p))
         (index (search "function" title :from-end t)))
    (string-trim '(#\Space #\Tab) (subseq title 0 index))))

(defun statement-page-p (p)
  (title-ends-p p "statement"))

(defun statement-page-statement (p)
  "Return the statement name of a statement page."
  (let* ((title (page-title p))
         (index (search "statement" title :from-end t)))
    (string-trim '(#\Space #\Tab) (subseq title 0 index))))

(defun title-begins-p (p str)
  "Return t if the page title for P beings with STR, nil otherwise."
  (let* ((title (page-title p))
         (index (search str title)))
    (and index
         (= index 0))))

(defun fn-statement-page-p (p fn)
  "Return true if P is a page for the statement version of function FN."
  (and (statement-page-p p)
       (title-begins-p p fn)))

(defun fn-statement-pages (fn-pages statement-pages)
  "Return a list of the pages in STATEMENTS-PAGES which are function-statements for functions in FN-PAGES."
  (let ((stat-ps (make-hash-table :test 'equal)))
    (loop for p in statement-pages
       do (setf (gethash (statement-page-statement p) stat-ps) p))
    (loop for p in fn-pages
       if (nth-value 1 (gethash (fn-page-fn p) stat-ps))
       collect (gethash (fn-page-fn p) stat-ps)))
  ;; (loop for p in fn-pages
  ;;    if (find-if (lambda (sp) (title-begins-p sp (fn-page-fn p)))
  ;;                statement-pages)
  ;;    collect p)
  )

(defun handle-page-p (p)
  "Return true if P is a page about a handle type."
  (title-ends-p p "handle"))

(defun operator-page-p (p)
  "Return true if P is a page about an operator."
  (title-ends-p p "operator"))

(defun preproc-page-p (p)
  "Return true if P is a page about a preprocessor directive."
  (title-ends-p p "preprocessor directive"))

(defun phrase-page-p (p)
  "Return true if P is page about a particular kind of phrase."
  (title-ends-p p "phrase"))

(defun count-syntax-entries (page-list)
  "Return the number of syntax entries that are part of the pages in PAGE-LIST."
  (loop for p in page-list
     summing (length (syntax-entries p))))

(defun set-exclude (set &rest sets)
  (reduce #'set-difference sets :initial-value set))

;; TODO: Add (set-partition (set membership-predicate-lst)) here

(defun set-partition (set predicates)
  (let ((preds (append predicates (list (constantly t)))))
    (if (null set)
        ;; Return |predicates|+1 empty sets (mapcar (constantly '())
        ;; (cons nil predicates)) would be faster...
        (apply #'values (mapcar (constantly '()) preds))
        (let* ((partitions (multiple-value-list (set-partition (cdr set) predicates)))
               (set-index (position-if (lambda (p) (funcall p (car set)))
                                       preds))
               (part (nthcdr set-index partitions)))
          (pushnew (car set) (car part))
          (apply #'values partitions)))))
