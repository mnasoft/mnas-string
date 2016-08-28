;;;; mnas-string.asd

(asdf:defsystem #:mnas-string
  :description "Describe mnas-string here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "mnas-string")
	       (:file "mnas-string-sort-designation")
	       (:file "mnas-string-quote")
	       ))

