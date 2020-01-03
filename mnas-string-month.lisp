;;;; mnas-string-month.lisp

(in-package #:mnas-string)

(annot:enable-annot-syntax)

@export
(defparameter  *mon-ru* (make-hash-table)
  "Хеш-таблица, содержащая наименования месяцев на русском языке.
Пример использования:
 (gethash 12 *mon-ru*)")

@export
(defparameter  *mon-ua* (make-hash-table)
  "Хеш-таблица, содержащая наименования месяцев на украинском языке.
Пример использования:
 (gethash 12 *mon-ua*)")

@export
(defparameter  *mon-en* (make-hash-table)
  "Хеш-таблица, содержащая наименования месяцев на английском языке.
Пример использования:
 (gethash 12 *mon-en*)")

(defun init-month-names ()
  "Выполняет инициализацию хеш-таблиц, содержащих наименования месяцев."
  (loop
     :for i :from 1 :upto 12
     :for m-ru :in '("Январь"  "Февраль"   "Март"    "Апрель"  "Май"     "Июнь"    "Июль"   "Август"  "Сентябрь"  "Октябрь" "Ноябрь"   "Декабрь")
     :for m-ua :in '("Січень"  "Лютий"    "Березень" "Квітень" "Травень" "Червень" "Липень" "Серпень" "Вересень"  "Жовтень" "Листопад" "Грудень")
     :for m-en :in '("January" "February" "March"    "April"   "May"     "Jun"     "July"   "August"  "September" "October" "November" "December")
     :do
       (setf (gethash i *mon-ru*) m-ru)
       (setf (gethash i *mon-ua*) m-ua)
       (setf (gethash i *mon-en*) m-en)))

(init-month-names)

@export
(defparameter *default-month-language* *mon-ru*
  "Язык по-умолчанию для вывода наименования месяца.")
