;;;; ./src/parse/parse.lisp

(defpackage :mnas-string/parse
  (:use #:cl) 
  (:export read-number
           parse-number)
  (:export read-integer-alt)
  (:documentation
   "Пакет @b(mnas-string) содержит в своем составе
 следующие основные функции:
 @begin(list) 
  @item(@b(read-number) - парсинг вещественного числа при
        помощи считывателя Common Lisp;)
  @item(@b(parse-number) - парсинг вещественного числа;)
  @item(@b(read-integer-alt) - выбор одного из целых чисел;)
 @end(list)
"))

(in-package :mnas-string/parse)

(defun read-number (str &optional (default 0.0))
  "@b(Описание:) функция @b(read-number) возвращает число
 класса @b(number) при чтении из строки @b(str).

 При считывании используется стандартный считыватель Common Lisp.
"
  (let ((val (read-from-string str)))
    (cond
      ((numberp val) val)
      (t default))))

(defun parse-number (str &optional (default 0.0))
  " @b(Описание:) parse-number выполняет чтение из строки
  @b(str) вещественного числа.

 Если число не удалось считать - возвращается default. 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (parse-number \"3.14\")     => 3.14, 4
 (parse-number \"3,14\")     => 3.14, 4
 (parse-number \"3,14e2\")   => 314.0, 6
 (parse-number \"-3,14e-2\") => -0.0314, 8
 (parse-number \"3,14d+2\")  => 314.0d0, 7
 (parse-number \"-3,14d-2\") => -0.0314d0, 8
@end(code)
"
  (let ((val (cl-ppcre:scan-to-strings "(([+-]?\\d+)[.,]?)\\d*([ed][+-]?\\d+)?" str))) 
    (cond
      ((stringp val) (read-from-string (mnas-string:replace-all val "," "."))) 
      (t default))))

(defun read-integer-alt (lst &optional (prompt "Enter one of the alternatives:"))
  (loop
    (format t "~&~A ~S : " prompt lst)
    (let* ((*read-eval* nil)
           (number (read)))
      (when (member number lst) (return-from read-integer-alt number)))))

