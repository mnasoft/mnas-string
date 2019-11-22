;;;; mnas-string.asd

(defsystem #:mnas-string
  :description "Describe mnas-string here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
	       (:file "mnas-string-month"            :depends-on ("package"))
	       (:file "mnas-string"                  :depends-on ("package" "mnas-string-month"));
	       (:file "mnas-string-quote"            :depends-on ("package" "mnas-string"))
	       (:file "mnas-string-translit"         :depends-on ("package" "mnas-string"))
	       (:file "mnas-string-sort-designation" :depends-on ("package" "mnas-string"))
	       (:file "demo"                         :depends-on ("package" "mnas-string-sort-designation" "mnas-string-translit"))
	       ))

