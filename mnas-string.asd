;;;; mnas-string.asd

(defsystem "mnas-string"
  :description "Система @b(mnas-string) предназначена для:
@begin(list) 
 @item(парсинга вещественного числа;)
 @item(разделения строки на подстроки;)
 @item(замены всех вхождений подстроки в строке;)
 @item(замены множественного вхождения паттерна единичным;)
 @item(подготовки строки в качестве аргумента для like запроса SQL;)
 @item(обрамления строки префиксом и постфиксом;)
 @item(вывода представления даты и времени в поток или строку;)
 @item(траслитерации строки.)
@end(list) 
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/tests")))
  :depends-on ("mnas-string/core" "mnas-string/print" "mnas-string/translit" "mnas-string/db" "mnas-string/parse")
  )

(defsystem "mnas-string/core"
  :description "Система @b(mnas-string/core) содержит функции:
@begin(list) 
 @item(парсинга вещественного числа;)
 @item(разделения строки на подстроки;)
 @item(замены всех вхождений подстроки в строке;)
 @item(замены множественного вхождения паттерна единичным;)
 @item(обрамления строки префиксом и постфиксом.)
@end(list)
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/core/tests")))
  :depends-on ("cl-ppcre")
  :components ((:module "src/core"
		:serial nil
                :components ((:file "core")))
               (:module "src/app-numbering"
		:serial nil
                :components ((:file "app-numbering")))))


(defsystem "mnas-string/parse"
  :description "Система @b(mnas-string/parse) содержит функции:
@begin(list) 
 @item(парсинга вещественного числа.)
@end(list)"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/core/tests")))
  :depends-on ("cl-ppcre" "mnas-string/core")
  :in-order-to ((test-op (test-op "mnas-string/parse/tests")))
  :components ((:module "src/parse"
		:serial nil
                :components ((:file "parse")))))

(defsystem "mnas-string/db"
  :description "Система @b(mnas-string/db) содержит функции:
@begin(list) 
 @item(подготовки строки в качестве аргумента для like запроса SQL;)
@end(list)
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("mnas-string/core")
  :in-order-to ((test-op (test-op "mnas-string/db/tests")))
  :components ((:module "src/db"
		:serial nil
                :components ((:file "db")))))

(defsystem "mnas-string/print"
  :description "Система @b(mnas-string/print) содержит в
 своем составе функции вывода представления даты и времени в поток."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/print/tests")))
  :depends-on ("mnas-string/translit")
  :components ((:module "src/print"
		:serial nil
                :components ((:file "print")))))

(defsystem "mnas-string/translit"
  :description "Система @b(mnas-string/translit) cодержит функции
  траслитерации строки."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("mnas-hash-table")
  :in-order-to ((test-op (test-op "mnas-string/translit/tests")))
  :components ((:module "src/translit"
		:serial nil
                :components ((:file "translit")))))

(defsystem "mnas-string/docs"
  :description "Зависимости для сборки документации"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))


(defsystem "mnas-string/tests"
  :description "Тестирование систем, входящих  в проект mnas-string"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "fiveam" "sb-posix")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-string/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "tests")))
               (:module "src/tests/suites"
                :depends-on ("src/tests")
		:serial nil
                :components ((:file "core"     )
                             (:file "parse"    )
                             (:file "translit" )
                             (:file "db"       )
                             (:file "print"    )))
               (:module "src/tests/run"
                :depends-on ("src/tests/suites")
		:serial nil
                :components ((:file "run")))))

