;;;; mnas-string.asd

(defsystem "mnas-string"
  :description "Describe mnas-string here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("cl-ppcre" "mnas-string/print" "mnas-string/translit")
  :components
  ((:module "src" 
    :serial nil
    :components
    ((:file "mnas-string" )
     (:file "demo" )))))

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

(defsystem "mnas-string/translit"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/print/tests")))
  :components ((:module "src/translit"
		:serial nil
                :components ((:file "translit")))))

(defsystem "mnas-string/zm"
  :description "Содержит функции сортировки обозначений принятые на ZM."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("mnas-string")
  :serial nil
  :in-order-to ((test-op (test-op "math/core/tests")))
  :components ((:module "src/zm"
		:serial nil
                :components ((:file "zm")))))

(defsystem "mnas-string/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "mnas-package" "codex"))
