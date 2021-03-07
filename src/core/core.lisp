;;;; ./src/core/core.lisp

(defpackage #:mnas-string
  (:use #:cl) 
  (:export split           
	   replace-all
           mpattern-to-spattern
           pre-post)
  (:export trd-rename
           getenv)
  (:export make-populated-hash-table
           map-to-list
           )
  (:documentation
   "Пакет @b(mnas-string) содержит в своем составе
 следующие основные функции:
 @begin(list) 
  @item(@b(split) - разделение строки на подстроки;)
  @item(@b(replace-all) - замена всех вхождений подстроки в строке;)
  @item(@b(mpattern-to-spattern) - замена множественного вхождения паттерна единичным;)
  @item(@b(pre-post) - обрамление строки префиксом и постфиксом.)
 @end(list)
"))

(in-package #:mnas-string)

(defun replace-all (string part replacement &key (test #'char=))
  "@b(Описание:) функция @b(replace-all) возвращает строку, в которой
все вхождения @b(part) заменяется на @b(replacement).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (replace-all \"Paris, Paris? Paris!\" \"Pa\" \"Bo\")
   => \"Boris, Boris? Boris!\"
@end(code)
"
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

(defun mpattern-to-spattern (pattern str)
  "@b(Описание:) @b(mpattern-to-spattern) возвращает строку, у которой
 повторяющиеся подряд подстроки @b(pattern) сведены до одного включения.

@b(Пример использования:)
@begin[lang=lisp](code)
 (mpattern-to-spattern  \" \" \"Our    plane  will   land  in the city of    BadenBaden.\") 
  => \"Our plane will land in the city of BadenBaden.\"
@end(code)
"
  (do
   ((str1
     (replace-all str (concatenate 'string pattern pattern) pattern)
     (replace-all str (concatenate 'string pattern pattern) pattern)))
   ((= (length str1) (length str)) str1)
    (setf str str1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-to-list (sequence &key (key #'(lambda (el) el)))
  "@b(Описание:) map-to-list выполняет преобразование
 последовательности @b(sequence) в список.

 @b(Пример использования:)
@begin[lang=lisp](code)
(map-to-list '((1 2 )(2 3)(3 4)(4 5)(5 6)(6 7)))
 ; => ((1 2) (2 3) (3 4) (4 5) (5 6) (6 7))
(map-to-list '((1 2 )(2 3)(3 4)(4 5)(5 6)(6 7)) :key #'first)
  ; => (1 2 3 4 5 6)
(map-to-list '((1 2 )(2 3)(3 4)(4 5)(5 6)(6 7)) :key #'second)
 ; => (2 3 4 5 6 7)

@end(code)
"
  (map 'list key sequence))

(defun make-populated-hash-table (sequence &key
					     (key-function    #'(lambda (el) el))
					     (value-function  #'(lambda (el) el))
					     (test #'equal))
  "@b(Описание:) функция @b(make-populated-hash-table) возвращает
 наполненную элементами хеш-таблицу. Хеш-таблица формируется на
 основании содержимого последовательности @b(sequence).

 Для каждого элемента последовательности при добавлении в хеш-таблицу:
@begin(list)
 @item(ключи вычисляются при помощи функции, заданной аргументом @b(key-function);)
 @item(значения вычисляются при помощи функции, заданной аргументом @b(value-function).)
@end(list)
"
  (reduce
   #'(lambda (ht el)
       (setf (gethash (funcall key-function el) ht) (funcall value-function el))
       ht)
   sequence
   :initial-value (make-hash-table :test test)))

(defun split (char-bag string &key (omit-nulls t))
  "@b(Описание:) функция: @b(split) разделяет строку @b(string) на подстроки.

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

(defun trd-rename (f-name &optional (ext "trd"))
  "@b(Описание:) функция @b(trd-rename) возвращает строку,
 представляющую имя файла в формате \"YYYY-MM-DD_hhmmss.ext\".

 Преобразуемая строка дожна удовлетворять формату
 \"DDMMYY_hhmmss.ext\".

 Здесь:
 @begin(list)
 @item(YYYY - год;)
 @item(YY - год;)
 @item(MM - месяца;)
 @item(DD - день;)
 @item(hh - час;)
 @item(mm - минута;)
 @item(ss - секунда;)
 @item(ext - расширение файла.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-rename \"150819_082056.trd\") => \"2019-08-15_082056.trd\"
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

(defun getenv (sys-var &optional (default ""))
  "@b(Описание:) функция @b(getenv) возвращает строку, представляющую
  значение системной переменной, заданное аргументом @b(sys-var).

 @b(Пример использования:)
@begin[lang=lisp](code) 
 (getenv \"SBCL_HOME\") 
 (getenv \"PATH\")
@end(code)
"
  (cond
    ((uiop:getenv sys-var))
    (t default)))

(defun pre-post (string &optional (prefix "\"" ) (postfix prefix))
  "@b(Описание:) функция @b(pre-post) возвращает строку,
 основанную на @b(string) с добавлением перед и после нее префикса
 @b(prefix) и постфикса @b(postfix) pre-post-string"
  (concatenate 'string prefix string postfix))
