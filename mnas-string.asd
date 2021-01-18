;;;; mnas-string.asd

(defsystem "mnas-string"
  :description "Describe mnas-string here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("cl-ppcre" "mnas-string/print")
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-string-month" )
     (:file "mnas-string"                  :depends-on ("mnas-string-month"))
     (:file "mnas-string-quote"            :depends-on ("mnas-string"))
     (:file "mnas-string-translit"         :depends-on ("mnas-string"))
     (:file "mnas-string-sort-designation" :depends-on ("mnas-string"))
     (:file "demo"                         :depends-on ("mnas-string-sort-designation" "mnas-string-translit"))))))

(defsystem "mnas-string/print"
  :description "Describe mnas-string here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("cl-ppcre")		; #:codex #:cl-annot
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-string-month" )
     (:file "mnas-string"                  :depends-on ("mnas-string-month"))
     (:file "mnas-string-quote"            :depends-on ("mnas-string"))
     (:file "mnas-string-translit"         :depends-on ("mnas-string"))
     (:file "mnas-string-sort-designation" :depends-on ("mnas-string"))
     (:file "demo"                         :depends-on ("mnas-string-sort-designation" "mnas-string-translit"))))))

(defsystem "mnas-string/print"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "mnas-string/print/tests")))
;;;;  :depends-on ()
  :components ((:module "src/print"
		:serial nil
                :components ((:file "print")))))

(defsystem "mnas-string/zm"
  :description "Содержит функции сортировки обозначений принятые на ZM."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
;;;;  :depends-on ()
  :components ((:module "src/zm"
		:serial nil
                :components ((:file "zm")))))


(defsystem "mnas-string/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "mnas-package" "codex"))
