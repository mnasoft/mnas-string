;;;; ./src/mnas-string.lisp

(defpackage #:mnas-string
  (:use #:cl #:mnas-string/print #:mnas-string/translit)
  (:export read-from-string-number
           read-number-from-string)
  (:export split           
	   replace-all)
  (:export add-prefix
	   mpattern-to-spattern
	   prepare-to-query)
  (:export trd-rename
           getenv)
  (:export print-universal-time
           print-universal-date-time
           print-universal-date
           print-universal-date-time-fname)
  (:export make-populated-hash-table
           map-to-list
           )
  (:documentation
   " MNAS-string содержит в своем составе функции 
@begin(list)
 @item(вывода даты и времени;)
 @item(преобразования строк;)
 @item(демонстрационные;) 
@end(list)
"))

(in-package #:mnas-string)

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

(export 'mpattern-to-spattern )
(defun mpattern-to-spattern (pattern str)
"@b(Описание:) @b(mpattern-to-spattern) исключает из строки 
str повторяющиеся подстроки pattern сводя их количество до одного включения.

@b(Пример использования:)
@begin[lang=lisp](code)
 (mpattern-to-spattern  \"Baden\" \"Наш самолет осуществит посадку в городе BadenBaden.\") 
 => \"Наш самолет осуществит посадку в городе Baden.\"
@end(code)
"
  (do
   ((str1
     (string-replace-all str (concatenate 'string pattern pattern) pattern)
     (string-replace-all str (concatenate 'string pattern pattern) pattern)))
   ((= (length str1) (length str)) str1)
    (setf str str1)))

(export 'prepare-to-query )
(defun prepare-to-query (str)
"@b(Описание:) prepare-to-query подготавливает строку, 
введенную пользователем, для участия в запросе.

Подготовка заключется в отсечении начальных и конечных пробелов и
 замене оставшихся пробелов на знаки %"
  (substitute #\% #\Space (concatenate 'string "%" (mpattern-to-spattern " " (string-trim " " str)) "%")))
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

(export 'add-prefix )
(defun add-prefix (str  &key (prefix " ") (overal-length (length str)))
" @b(Описание:) add-prefix 

 @b(Пример использования:)
@begin[lang=lisp](code)
 (add-prefix \"45\" :overal-length 10 :prefix \"-\" )
@end(code)
"
  (dotimes (i (- overal-length (length str)) str)
    (setf str (concatenate 'string prefix str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-to-list (sequence)
  "@b(Описание:) map-to-list выполняет преобразование последовательности @b(sequence)
в список.

@b(Равнозначно следующему:)
@begin[lang=lisp](code)
 (map 'list #'(lambda (el) el) sequence)
@end(code)
"
  (map 'list #'(lambda (el) el) sequence))

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

(defun split (char-bag string &key (omit-nulls t))
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

(defun string-quote (string &optional (pre-post-string "\"" ))
  "Добавляет в начао и конец строки string строку pre-post-string"
  (concatenate 'string pre-post-string string pre-post-string))
