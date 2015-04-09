(in-package #:cl-talk)

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))

(defun kwd (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   keyword symbol.
   If the input is symbol/s then the output is a regular keyword
   If the input is string/s, then the output is
   a :|keyword like this|"
  (values (intern (apply #'mkstr args) "KEYWORD")))

(defun mkstr (&rest args)
  "Takes a list of strings or symbols and returns one string
   of them concatenated together. For example:
    EXAMPLES> (mkstr 'jam 'ham')
     'JAMHAM'
    EXAMPLES> (mkstr 'jam' 'ham')
     'jamham'"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (apply #'mkstr args))))

(defun symb-package (package &rest args)
  (values (intern (apply #'mkstr args) package)))

(defun flatten (x)
  "Walks a list tree and flattens it (returns a 1d list
   containing all the elements from the tree)"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(defun split-seq-by-seq (delim sequence)
  (let* ((delim-len (length delim))
         (seq-len (length sequence))
         (result nil)
         (last 0)
         (i 0))
    (loop :do
       (setf i (1+ i))
       (if (> (+ i delim-len) seq-len)
           (progn (push (subseq sequence last) result)
                  (return (reverse result)))
           (when (equal (subseq sequence i (+ i delim-len)) delim)
             (push (subseq sequence last i) result)
             (setf i (+ -1 i delim-len)
                   last (1+ i)))))))

(defun listify (x) (if (listp x) x (list x)))


(defun map-hash (function hash-table)
  "map through a hash and actually return something"
  (let* ((head (list nil))
         (tail head))
    (labels ((do-it (k v)
               (rplacd tail (setq tail (list (funcall function k v))))))
      (maphash #'do-it hash-table))
    (cdr head)))


(defun arg-names (args)
  (remove-if (lambda (x) (member x lambda-list-keywords))
             (mapcar (lambda (x) (if (listp x) (first x) x))
                     args)))
