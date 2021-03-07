;;;; ./src/print/print.lisp

(defpackage #:mnas-string/print
  (:use #:cl)
  (:export date
           date-time
           date-time-fname
           day-time)
  (:export  *mon-ru*
            *mon-ua*
            *mon-en*)
  (:export *default-month-language*)
  (:documentation
   "Пакет @b(mnas-string/print) экспортирует следующие функции:
@begin(list)
 @item(@b(date) - вывод даты;)
 @item(@b(date-time) - вывод даты и времени;)
 @item(@b(day-time) - вывод времени;)
 @item(@b(date-time-fname) - вывод даты и времени для именования файла.)
@end(list)

 Все вышеперечисленые функций первым аргументом принимают:
@begin(list)
 @item(@b(u-time) - время, заданное в универсальном формате;)
@end(list)

  Кроме того, все они принимают ключевой аргумент @b(stream):
@begin(list)
@item(при @b(stream)=@b(nil) - вывод осуществляется в строку;)
@item(при @b(stream)=@b(t)   - вывод на стандартный вывод.)
@end(list)

 Остальными ключевыми параметрами этих фукций являются:
@begin(list)

 @item(@b(year) - позволяет выводить (при @b(year)=@b(t)) или не
                  выводить (при @b(year)=@b(nil)) представление года;)
 
 @item(@b(day) - позволяет выводить (при @b(day)=@b(t)) или не
                 выводить (при @b(day)=@b(nil)) число дня;)

 @item(@b(ss) - позволяет выводить (при @b(ss)=@b(t)) или не
                выводить (при @b(ss)=@b(nil)) представление секунд;)

 @item(@b(month-language) - задает язык для вывода месяца. Значением
 аргумента должна быть хеш-таблица. Ключи - порядковые номера месяцев
 от 1 до 12. Значения - строки с наименованиями месяцев.)
@end(list)
"))

(in-package #:mnas-string/print)

(defparameter  *mon-ru* (make-hash-table)
  "@b(Описание:) хеш-таблица @b(*mon-ru*) содержит названия месяцев на
  русском языке.

 Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ru*) =>  \"Январь\", T
 (gethash 12 *mon-ru*) => \"Декабрь\", T
 @end(code)
")

(defparameter  *mon-ua* (make-hash-table)
  "@b(Описание:) хеш-таблица @b(*mon-ua*) содержит названия месяцев на
  украинском языке.

 Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ua*)  => \"Січень\", T
 (gethash 12 *mon-ua*) => \"Грудень\", T
 @end(code)
")

(defparameter *mon-en* (make-hash-table)
  "@b(Описание:) хеш-таблица @b(*mon-en*) содержит названия месяцев на
  английском языке.

 Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-en*)  => \"January\" ,T
 (gethash 12 *mon-en*) => \"December\" ,T
 @end(code)
")

(defun init-month-ht ()
  "@b(Описание:) функция @b(init-month-ht) выполняет инициализацию
 хеш-таблиц: @b(*mon-ru*); @b(*mon-ua*); @b(*mon-en*), содержащих
 наименования месяцев."
  (loop
    :for i :from 1 :upto 12
    :for m-ru :in '("Январь"  "Февраль"   "Март"    "Апрель"  "Май"     "Июнь"    "Июль"   "Август"  "Сентябрь"  "Октябрь" "Ноябрь"   "Декабрь")
    :for m-ua :in '("Січень"  "Лютий"    "Березень" "Квітень" "Травень" "Червень" "Липень" "Серпень" "Вересень"  "Жовтень" "Листопад" "Грудень")
    :for m-en :in '("January" "February" "March"    "April"   "May"     "Jun"     "July"   "August"  "September" "October" "November" "December")
    :do
       (setf (gethash i *mon-ru*) m-ru)
       (setf (gethash i *mon-ua*) m-ua)
       (setf (gethash i *mon-en*) m-en)))

(defparameter *default-month-language* *mon-ru*
  "@b(Описание:) *default-month-language* язык по-умолчанию для наименования месяца.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *default-month-language*)
@end(code)
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-month-ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"
 @begin(enum)
  @item(при @b(stream)=@b(nil) - вывод осуществляется в строку;)
  @item(при @b(stream)=@b(t)   - вывод на стандартный вывод;)
 @end(enum)
"
(defun date (u-time &key (stream t) (year t) (day t) (month-language *default-month-language*))
  "@b(Описание:) функция @b(date) выводит в поток @b(stream) 
строковое представление даты. 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (date (get-universal-time) :stream nil) => \"2019-12-04\"
 (date (get-universal-time) :stream nil :year nil :day nil) => \"Январь\"
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (list date-year date-month date-day time-hour time-minute time-second)
    (cond
      ((and year           day ) (format stream "~d-~2,'0d-~2,'0d" date-year date-month date-day))
      ((and year      (not day)) (format stream "~A ~A"           (gethash date-month month-language) date-year))
      ((and (not year)     day ) (format stream "~2,'0d-~2,'0d"    date-month date-day))
      ((and (not year)(not day)) (format stream "~A" (gethash date-month month-language))))))

(defun date-time-fname (u-time &key (stream t) (year t) (ss t))
  "@b(Описание:) функция @b(date-time-fname) выводит дату и время в
пригодном для формирования имени файла формате.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((ut (get-universal-time)))
    (date-time-fname ut :year t   :ss t)    ;-> 2021-03-06_23-03-11 
    (date-time-fname ut :year nil :ss t)    ;-> 03-06_23-03-11
    (date-time-fname ut :year t   :ss nil)  ;-> 2021-03-06_23-03
    (date-time-fname ut :year nil :ss nil)) ;-> 03-06_23-03 
  => NIL
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d"                     date-month date-day time-hour time-minute)))))

(defun day-time (u-time &key (stream t) (ss t))
  "@b(Описание:) day-time выводит в поток @b(stream) строковое
представление времени. 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (day-time (get-universal-time))         ;-> 23:06:15 =>NIL
 (day-time (get-universal-time) :ss nil) ;-> 23:07 =>NIL
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (list date-year date-month date-day time-hour time-minute time-second)
    (cond
      (ss (format stream "~2,'0d:~2,'0d:~2,'0d" time-hour time-minute time-second))
      (t  (format stream "~2,'0d:~2,'0d"        time-hour time-minute)))))

(defun date-time (u-time &key (stream t) (year t) (ss t))
  "@b(Описание:) date-time выводит в поток @b(stream) 
строковое представление даты и времени. 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (date-time (get-universal-time))                   ;-> 2021-03-06_21:17:11 => NIL
 (date-time (get-universal-time) :year nil        ) ;->03-06_21:18:57       => NIL
 (date-time (get-universal-time) :year nil :ss nil) ;-> 03-06_21:19         => NIL
 (date-time (get-universal-time)           :ss nil) ;-> 2021-03-06_21:20    => NIL
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d"                     date-month date-day time-hour time-minute)))))
