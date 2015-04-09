(in-package :cl-talk)

(defmacro dumb-add-1 (x)
  (list '+ x 1))

(dumb-add-1 10)
