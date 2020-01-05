;;;; mnas-string-month.lisp

(in-package #:mnas-string)

(annot:enable-annot-syntax)

@export
(defparameter  *mon-ru* (make-hash-table)
  "@b(Описание:) *mon-ru* - хеш-таблица, которая содержит названия месяцев на русском языке.

Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ru*) =>  \"Январь\", T
 (gethash 12 *mon-ru*) => \"Декабрь\", T
 @end(code)
")

@export
(defparameter  *mon-ua* (make-hash-table)
  "@b(Описание:) *mon-ua* - хеш-таблица, которая содержит названия месяцев на украинском языке.

Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-ua*)  => \"Січень\", T
 (gethash 12 *mon-ua*) => \"Грудень\", T
 @end(code)
")

@export
(defparameter  *mon-en* (make-hash-table)
  "@b(Описание:) *mon-en* - хеш-таблица, которая содержит названия месяцев на английском языке. 

Ключами являются порядковые номера месяцев от 1 до 12.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gethash 1 *mon-en*)  => \"Січень\", T
 (gethash 12 *mon-en*) => \"Грудень\", T
 @end(code)
"
  )

@annot.doc:doc
"init-month-names выполняет инициализацию хеш-таблиц, содержащих наименования месяцев."
(defun init-month-names ()
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
  "@b(Описание:) *default-month-language* язык по-умолчанию для наименования месяца.")
