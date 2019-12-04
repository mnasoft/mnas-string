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
  (let ((val (cl-ppcre:scan-to-strings "(([+-]?\\d+)[.,]?)\\d*([ed][+-]?\\d+)?" str))) 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'map-to-list)
(defun map-to-list (sequence)
  (map 'list #'(lambda (el) el) sequence))

(export 'make-populated-hash-table)
(defun make-populated-hash-table (sequence &key
					     (key-function    #'(lambda (el) el))
					     (value-function  #'(lambda (el) el))
					     (test #'equal))
  (reduce
   #'(lambda (ht el)
       (setf (gethash (funcall key-function el) ht) (funcall value-function el))
       ht)
   sequence
   :initial-value (make-hash-table :test test)))

(export '*omit-nulls*)
(defparameter *omit-nulls* t)

(export 'split)
(defun split (char-bag string &key (omit-nulls *omit-nulls*))
  "Разделяет строку string на подстроки.
@begin(list)
 @item(в качестве разделителей используются символы из строки char-bag;)
 @item(возвращает список подстрок;)
 @item(если omit-nulls не равно nil пустые подстроки из результирующего списока исключаются.)
@end(list)

Пример использования:
@begin[lang=lisp](code)
 (split-m \"; \" \" 1111 ; +5550650456540; 55\" )
@end(code)
"
  (let ((char-bag-hash (make-populated-hash-table (map-to-list char-bag)))
	(rez nil)
	(rezult))
    (loop :for i :from 0 :below (length string) :do
	 (if (gethash (char string i) char-bag-hash)
	     (push i rez)))
    (setf rez (nreverse (push (length string) rez)))
    (setf rezult
	  (mapcar
	   #'(lambda (el)
	       (subseq string (first el) (second el)))
	   (mapcar #'(lambda (el1 el2) (list (1+ el1) el2)) (push -1 rez) (cdr rez))))
    (if omit-nulls 
	(mapcan  #'(lambda (x)
		     (if (= (length x) 0) nil (list x)))
		 rezult)
	rezult)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'trd-rename)
(defun trd-rename (f-name)
  "Пример использования:
 (trd-rename \"150819_082056\")
"
  (let* ((ddmmyy_hhmmss_ext (split "_" f-name)) dd mon yy hh mm ss)
    (assert (= 2 (length ddmmyy_hhmmss_ext)))
    (assert (= 6 (length (first  ddmmyy_hhmmss_ext))))
    (assert (= 6 (length (second ddmmyy_hhmmss_ext))))
    (assert (string= "trd" (third ddmmyy_hhmmss_ext)))
    (setf dd  (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 0 2)))
    (setf mon (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 2 4)))
    (setf yy  (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 4 6)))

    (setf hh (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 0 2)))
    (setf mm (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 2 4)))
    (setf ss (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 4 6)))   
    (values dd mon yy hh mm ss ddmmyy_hhmmss_ext)
    (format nil "~4d-~2,'0d-~2,'0d_~2,'0d~2,'0d~2,'0d" (+ 2000 yy) mon dd  hh mm ss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'getenv)
(defun getenv (x &optional (default ""))
  "Пример использования:
 (getenv \"SBCL_HOME\")
 (getenv \"PATH\")
"
  (cond
    ((uiop:getenv x))
    (t default)))
