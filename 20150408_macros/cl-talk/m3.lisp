(in-package :cl-talk)


(defmethod dispose ((thing t))
  (print "nothing to dispose"))

(defmethod dispose ((thing string))
  (print "woah, freeing a string resource"))


;; How we want it to look
(using (some-var (random 10))
  (print some-var))


;; The macro
;; (defmacro using (var-binding &body body)
;;   `(let ((,(first var-binding) ,(second var-binding)))
;;      (unwind-protect
;;           (progn ,@body)
;;        (dispose ,(first var-binding)))))
