;;;; mnas-string.lisp

(in-package #:mnas-string)

(export 'string-replace-all)

(defun string-replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement"
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(export 'string-mpattern-to-spattern)

(defun string-mpattern-to-spattern (pattern str)
  "Исключает из строки str повторяющиеся подстроки pattern сводя их количество до одного включения
Пример использования:
;;;;(string-mpattern-to-spattern  \"Baden\" \"Наш самолет осуществит посадку в городе BadenBaden.\")
"
  (do
   ((str1
     (string-replace-all str (concatenate 'string pattern pattern) pattern)
     (string-replace-all str (concatenate 'string pattern pattern) pattern)))
   ((= (length str1) (length str)) str1)
    (setf str str1)))

(export 'string-prepare-to-query)

(defun string-prepare-to-query(str)
  "Подготавливает строку, введенную пользователем, для участия в запросе
Подготовка заключется в отсечении начальных и конечных пробелов и замене оставшихся пробелов на знаки %"
  (substitute #\% #\Space (concatenate 'string "%" (string-mpattern-to-spattern " " (string-trim " " str)) "%")))

(export 'read-from-string-number)

(defun read-from-string-number (str &optional (default 0.0))
  (let ((val (read-from-string str)))
    (cond
      ((numberp val) val)
      (t default))))

(export 'read-number-from-string)

(defun read-number-from-string (str &optional (default 0.0))
"Выполняет чтение из строки вещественного числа.
Если число не удалось считать - возвращается default."
  (let ((val (scan-to-strings "(([+-]?\\d+)[.,]?)\\d*([ed][+-]?\\d+)?" str))) 
    (cond
      ((stringp val) (read-from-string (string-replace-all val "," "."))) 
      (t default))))

(export 'string-add-prefix)

(defun string-add-prefix (str  &key (prefix " ") (overal-length (length str)))
  "Пример использования
;;;;(string-add-prefix \"45\" :overal-length 10 :prefix \"-\" )
"
  (dotimes (i (- overal-length (length str)) str)
    (setf str (concatenate 'string prefix str))))

(export 'print-universal-date-time)

(defun print-universal-date-time (u-time &key (stream t) (year t) (ss t))
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d:~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d:~2,'0d"                     date-month date-day time-hour time-minute)))))

(export 'print-universal-time)

(defun print-universal-time (u-time &key (stream t) (ss t) )
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (list date-year date-month date-day time-hour time-minute time-second)
    (cond
      (ss (format stream "~2,'0d:~2,'0d:~2,'0d" time-hour time-minute time-second))
      (t  (format stream "~2,'0d:~2,'0d" time-hour time-minute)))))

(export 'print-universal-date)

(defun print-universal-date (u-time &key (stream t) (year t) (day t) (month-language *mon-ru*))
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (list date-year date-month date-day time-hour time-minute time-second)
    (cond
      ((and year           day ) (format stream "~d-~2,'0d-~2,'0d" date-year date-month date-day))
      ((and year      (not day)) (format stream "~A ~A"           (gethash date-month month-language) date-year))
      ((and (not year)     day ) (format stream "~2,'0d-~2,'0d"    date-month date-day))
      ((and (not year)(not day)) (format stream "~A" (gethash date-month month-language))))))

(export 'print-universal-date-time-fname)

(defun print-universal-date-time-fname (u-time &key (stream t) (year t) (ss t))
  "Выводит дату и время в пригодном для формирования имени файла формате"
  (multiple-value-bind (time-second  time-minute time-hour date-day date-month date-year)
      (decode-universal-time u-time)
    (cond
      ((and year ss)        (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d" date-year date-month date-day time-hour time-minute time-second))
      ((and (null year) ss) (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"              date-month date-day time-hour time-minute time-second))
      ((and year (null ss)) (format stream "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d"        date-year date-month date-day time-hour time-minute))
      (t                    (format stream "~2,'0d-~2,'0d_~2,'0d-~2,'0d"                     date-month date-day time-hour time-minute)))))
