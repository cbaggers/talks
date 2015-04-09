(in-package :cl-talk)

;; Syntax for named function
(defun test (n &optional (start 0))
  (when (< start n)
    (cons start (test n (+ start 1)))))


;; syntax we currently have to use
(lambda (n &optional (start 0))
  (labels ((this (n start)
             (when (< start n)
               (cons start (this n (+ start 1))))))
    (this n start)))

;; syntax we would like to use
(rlambda (n &optional (start 0))
  (when (< start n)
    (cons start (this n (+ start 1)))))

(defmacro rlambda (args &body body)
  (let ((clean (arg-names args)))
    `(lambda ,args
       (labels ((this ,clean
                  ,@body))
         (this ,@clean)))))
