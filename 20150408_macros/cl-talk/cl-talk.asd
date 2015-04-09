;;;; cl-talk.asd

(asdf:defsystem #:cl-talk
  :description "Describe cl-talk here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:temporal-functions
               #:fn_
               #:cells
               #:cl-ppcre
               #:split-sequence
               #:alexandria
               #:named-readtables)
  :components ((:file "package")
               (:file "cl-talk")))
