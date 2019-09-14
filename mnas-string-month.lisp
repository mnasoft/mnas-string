;;;; mnas-string-month.lisp

(in-package #:mnas-string)

(export '*mon-ru*)
(export '*mon-ua*)
(export '*mon-en*)

(progn 
  (defparameter  *mon-ru* (make-hash-table))
  (defparameter  *mon-ua* (make-hash-table))
  (defparameter  *mon-en* (make-hash-table))

  (loop
     for i from 1 upto 12
     for m-ru in '("Январь"  "Февраль"   "Март"    "Апрель"  "Май"     "Июнь"    "Июль"   "Август"  "Сентябрь"  "Октябрь" "Ноябрь"   "Декабрь")
     for m-ua in '("Січень"  "Лютий"    "Березень" "Квітень" "Травень" "Червень" "Липень" "Серпень" "Вересень"  "Жовтень" "Листопад" "Грудень")
     for m-en in '("January" "February" "March"    "April"   "May"     "Jun"     "July"   "August"  "September" "October" "November" "December")
     do
       (setf (gethash i *mon-ru*) m-ru)
       (setf (gethash i *mon-ua*) m-ua)
       (setf (gethash i *mon-en*) m-en)))
