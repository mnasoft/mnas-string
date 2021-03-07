;;;; ./src/db/db.lisp

(defpackage #:mnas-string/db
  (:use #:cl) 
  (:export prepare-to-query)
  (:documentation
   "Пакет @b(mnas-string) экспортирует следующие функции:
 @begin(list) 
  @item(@b(prepare-to-query) - подготовка строки в качестве аргумента для like запроса SQL.)
 @end(list)
"))

(in-package #:mnas-string/db)

(defun prepare-to-query (str)
  " @b(Описание:) функция @b(prepare-to-query) возвращает строку
 подготовленную для участия в запросе к базе данных, основанную на
 содержимом строки @b(str).

 Подготовка заключется в исключении начальных и конечных пробелов и
 замене оставшихся пробелов на знаки %.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (prepare-to-query \"  гайки  с   квадр гол  \")
   ; => \"%гайки%с%квадр%гол%\"
@end(code)
"
  (substitute #\% #\Space
              (concatenate 'string "%"
                           (mnas-string:mpattern-to-spattern " " (string-trim " " str)) "%")))
