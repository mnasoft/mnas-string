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
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("mnas-string/core" "mnas-string/print" "mnas-string/translit")
)

(defsystem "mnas-string/core"
  :description "Система @b(mnas-string/core) содержит функции:
@begin(list) 
 @item(парсинга вещественного числа;)
 @item(разделения строки на подстроки;)
 @item(замены всех вхождений подстроки в строке;)
 @item(замены множественного вхождения паттерна единичным;)
 @item(подготовки строки в качестве аргумента для like запроса SQL;)
 @item(обрамления строки префиксом и постфиксом.
@end(list)"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :depends-on ("cl-ppcre")
  :in-order-to ((test-op (test-op "mnas-string/core/tests")))
  :components ((:module "src/core"
		:serial nil
                :components ((:file "core")))))

(defsystem "mnas-string/print"
  :description "Система @b(mnas-string/print) содержит в
 своем составе функции вывода представления даты и времени в поток."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "mnas-string/print/tests")))
;;;;  :depends-on ()
  :components ((:module "src/print"
		:serial nil
                :components ((:file "print")))))

(defsystem "mnas-string/translit"
  :description "Система @b(mnas-string/translit) cодержит функции
  траслитерации строки."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-string/print/tests")))
  :components ((:module "src/translit"
		:serial nil
                :components ((:file "translit")))))

(defsystem "mnas-string/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "mnas-package" "codex"))
