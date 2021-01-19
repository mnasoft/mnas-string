;;;; ./src/print/print.lisp

(defpackage #:mnas-string/print
  (:use #:cl)
  (:export print-universal-date
           print-universal-date-time-fname
           print-universal-time
           print-universal-date-time
           )
  (:documentation
   " MNAS-string содержит в своем составе функции 
@begin(list)
 @item(вывода даты и времени)
@end(list)
"))

(in-package #:mnas-string/print)

(defparameter  *mon-ru* (make-hash-table)
  "@b(Описание:) *mon-ru* - хеш-таблица, которая содержит названия месяцев на русском языке.

Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ru*) =>  \"Январь\", T
 (gethash 12 *mon-ru*) => \"Декабрь\", T
 @end(code)
")

(defparameter  *mon-ua* (make-hash-table)
  "@b(Описание:) *mon-ua* - хеш-таблица, которая содержит названия месяцев на украинском языке.

Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ua*)  => \"Січень\", T
 (gethash 12 *mon-ua*) => \"Грудень\", T
 @end(code)
")

(defparameter *mon-en* (make-hash-table)
  "@b(Описание:) *mon-en* - хеш-таблица, которая содержит названия месяцев на английском языке. 

 Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-en*)  => \"January\" ,T
 (gethash 12 *mon-en*) => \"December\" ,T
 @end(code)
")

(defun init-month-names ()
"init-month-names выполняет инициализацию хеш-таблиц, содержащих наименования месяцев."
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
 (gethash 1 *mon-en*)
@end(code)
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-month-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-universal-date (u-time &key (stream t) (year t) (day t) (month-language *default-month-language*))
  "@b(Описание:) print-universal-date выводит в поток @b(stream) 
строковое представление даты. 

@b(Параметры:)
@begin(list)
 @item(@b(u-time) - время, заданное в универсальном формате;)
 @item(@b(stream) - поток, в который выводится время.
Если @b(nil) - вывод осуществляется в строку.
Если @b(t)   - вывод на стандартный вывод;)
 @item(@b(ss) - отвечает за отображение секунд.)
@end(list)

Пример использования:
@begin[lang=lisp](code)
 (print-universal-date (get-universal-time) :stream nil) => \"2019-12-04\"
 (print-universal-date (get-universal-time) :stream nil :year nil :day nil) => \"Январь\"
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

(defun print-universal-date-time-fname (u-time &key (stream t) (year t) (ss t))
"Выводит дату и время в пригодном для формирования имени файла формате"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d"                     date-month date-day time-hour time-minute)))))

(defun print-universal-time (u-time &key (stream t) (ss t) )
"@b(Описание:) print-universal-time выводит в поток @b(stream) строковое
представление времени. 

@b(Параметры:)
@begin(list)
 @item(@b(u-time) - время, заданное в универсальном формате;)
 @item(@b(stream) - поток, в который выводится время.
Если @b(nil) - вывод осуществляется в строку.
Если @b(t)   - вывод на стандартный вывод;)
 @item(@b(ss) - отвечает за отображение секунд.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (print-universal-time (get-universal-time) :stream nil) => \"17:07:03\"
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (list date-year date-month date-day time-hour time-minute time-second)
    (cond
      (ss (format stream "~2,'0d:~2,'0d:~2,'0d" time-hour time-minute time-second))
      (t  (format stream "~2,'0d:~2,'0d"        time-hour time-minute)))))

(defun print-universal-date-time (u-time &key (stream t) (year t) (ss t))
"@b(Описание:) print-universal-date-time выводит в поток @b(stream) 
строковое представление времени. 

 @b(Параметры:)
@begin(list)
 @item(@b(u-time) - время, заданное в универсальном формате;)
 @item(@b(stream) - поток, в который выводится время.
Если @b(nil) - вывод осуществляется в строку.
Если @b(t)   - вывод на стандартный вывод.)
 @item(@b(year) - отвечает за отображение года.) 
 @item(@b(ss)   - отвечает за отображение секунд.)
@end(list)
 @b(Пример использования:)

@begin[lang=lisp](code)
 (print-universal-date-time (get-universal-time) :stream nil) => \"2019-12-04_17:16:58\"
@end(code)
"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d"                     date-month date-day time-hour time-minute)))))
