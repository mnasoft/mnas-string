;;;; mnas-string.lisp

(in-package #:mnas-string)

(export 'string-replace-all )
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

(export 'replace-all )
(defun replace-all (string part replacement &key (test #'char=))
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

(export 'string-mpattern-to-spattern )
(defun string-mpattern-to-spattern (pattern str)
"@b(Описание:) @b(string-mpattern-to-spattern) исключает из строки 
str повторяющиеся подстроки pattern сводя их количество до одного включения.

@b(Пример использования:)
@begin[lang=lisp](code)
 (string-mpattern-to-spattern  \"Baden\" \"Наш самолет осуществит посадку в городе BadenBaden.\") 
 => \"Наш самолет осуществит посадку в городе Baden.\"
@end(code)
"
  (do
   ((str1
     (string-replace-all str (concatenate 'string pattern pattern) pattern)
     (string-replace-all str (concatenate 'string pattern pattern) pattern)))
   ((= (length str1) (length str)) str1)
    (setf str str1)))

(export 'string-prepare-to-query )
(defun string-prepare-to-query (str)
"@b(Описание:) string-prepare-to-query подготавливает строку, 
введенную пользователем, для участия в запросе.

Подготовка заключется в отсечении начальных и конечных пробелов и
 замене оставшихся пробелов на знаки %"
  (substitute #\% #\Space (concatenate 'string "%" (string-mpattern-to-spattern " " (string-trim " " str)) "%")))
(export 'read-from-string-number )
(defun read-from-string-number (str &optional (default 0.0))
"@b(Описание:) read-from-string-number выполняет чтение из строки @b(str) вещественного числа.

При считываии используется стандартный считыватель Common Lisp.
"
  (let ((val (read-from-string str)))
    (cond
      ((numberp val) val)
      (t default))))

(export 'read-number-from-string )
(defun read-number-from-string (str &optional (default 0.0))
" @b(Описание:) read-number-from-string выполняет чтение из строки @b(str) вещественного числа. 

Если число не удалось считать - возвращается default. 

 @b(Пример использования:)

@begin[lang=lisp](code)
 (read-number-from-string \"3.14\")
 (read-number-from-string \"3,14\")
@end(code)
"
  (let ((val (cl-ppcre:scan-to-strings "(([+-]?\\d+)[.,]?)\\d*([ed][+-]?\\d+)?" str))) 
    (cond
      ((stringp val) (read-from-string (string-replace-all val "," "."))) 
      (t default))))

(export 'string-add-prefix )
(defun string-add-prefix (str  &key (prefix " ") (overal-length (length str)))
" @b(Описание:) string-add-prefix 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (string-add-prefix \"45\" :overal-length 10 :prefix \"-\" )
@end(code)
"
  (dotimes (i (- overal-length (length str)) str)
    (setf str (concatenate 'string prefix str))))

(export 'print-universal-date-time )
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

(export 'print-universal-time )
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

(export 'print-universal-date )
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

(export 'print-universal-date-time-fname )
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

(export 'map-to-list )
(defun map-to-list (sequence)
"@b(Описание:) map-to-list выполняет преобразование последовательности @b(sequence)
в список.

@b(Равнозначно следующему:)
@begin[lang=lisp](code)
 (map 'list #'(lambda (el) el) sequence)
@end(code)
"
  (map 'list #'(lambda (el) el) sequence))

(export 'make-populated-hash-table )
(defun make-populated-hash-table (sequence &key
					     (key-function    #'(lambda (el) el))
					     (value-function  #'(lambda (el) el))
					     (test #'equal))
"@b(Описание:)
make-populated-hash-table создает хеш-таблицу и наполняет ее элементами."
  (reduce
   #'(lambda (ht el)
       (setf (gethash (funcall key-function el) ht) (funcall value-function el))
       ht)
   sequence
   :initial-value (make-hash-table :test test)))

(export '*omit-nulls*)
(defparameter *omit-nulls* t
  "Используется в функции @b(split) для определеия характера исключения|неисключения
пустых подстрок при разделенни строки на подстроки.")

(export 'split )
(defun split (char-bag string &key (omit-nulls *omit-nulls*))
"@b(Описание:)

 @b(split) разделяет строку @b(string) на подстроки.

 @b(Возвращает:) список подстрок.

 @b(Переменые:)

@begin(list)
 @item(@b(char-bag) - символы из этой строки используются в качестве разделителей;)
 @item(@b(string) - строка, подлежащая разделению на подтсроки;)
 @item(если omit-nulls не равно nil пустые подстроки из результирующего списока исключаются.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (split \"; \" \" 1111 ; +5550650456540; 55\" ) => (\"1111\" \"+5550650456540\" \"55\")
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

(export 'trd-rename )
(defun trd-rename (f-name &optional (ext "trd"))
"@b(Описание:) trd-rename выполняет преобразование имени файла, заданого в формате
DDMMYY_hhmmss.ext в формат YYYYMMDD_hhmmss.ext.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-rename \"150819_082056.trd\") => 2019-08-15_082056.trd
@end(code)
"
  (let* ((ddmmyy_hhmmss_ext (split "_." f-name)) dd mon yy hh mm ss)
    (assert (= 3 (length ddmmyy_hhmmss_ext)))
    (assert (= 6 (length (first  ddmmyy_hhmmss_ext))))
    (assert (= 6 (length (second ddmmyy_hhmmss_ext))))
    (assert (string= ext (third ddmmyy_hhmmss_ext)))
    (setf dd  (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 0 2)))
    (setf mon (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 2 4)))
    (setf yy  (parse-integer (subseq (first  ddmmyy_hhmmss_ext) 4 6)))

    (setf hh (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 0 2)))
    (setf mm (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 2 4)))
    (setf ss (parse-integer (subseq (second  ddmmyy_hhmmss_ext) 4 6)))   
    (values dd mon yy hh mm ss ddmmyy_hhmmss_ext)
    (format nil "~4d-~2,'0d-~2,'0d_~2,'0d~2,'0d~2,'0d.~a" (+ 2000 yy) mon dd  hh mm ss ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'getenv )
(defun getenv (x &optional (default ""))
  "@b(Пример использования:)
@begin[lang=lisp](code) 
 (getenv \"SBCL_HOME\") 
 (getenv \"PATH\")
@end(code)
"
  (cond
    ((uiop:getenv x))
    (t default)))
