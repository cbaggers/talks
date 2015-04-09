(in-package :cl-talk)

;; Simple function to make and populate a hash-table
(defun gen-hash-table (data-pairs)
  (let ((ht (make-hash-table)))
    (loop :for (key val) :in data-pairs :do
       (setf (gethash key ht) val))
    ht))


;; This is the function that reads the syntax
;; and emits the AST
(defun ht-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((elements (read stream t nil t))
         (pairs (group elements 2)))
    `(gen-hash-table ',pairs)))


;; make a readtable which is just like the default
;; readtable but with our hash-table syntax as well
(defreadtable hash-table-literal
  (:merge :standard)
  (:dispatch-macro-char #\# #\h #'ht-reader))


;; equivalent of in-package but for reader macros
(in-readtable hash-table-literal)
