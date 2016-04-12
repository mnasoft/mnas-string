;;;; mnas-string.lisp

(in-package #:mnas-string)

;;; "mnas-string" goes here. Hacks and glory await!

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

(defun string-mpattern-to-spattern(pattern str)
  "Исключает из строки str повторяющиеся подстроки pattern сводя их количество до одного включения
Пример использования
(string-mpattern-to-spattern  \"Baden\" \"Наш самолет осуществит посадку в городе BadenBaden.\")
"
  (do
   ((str1
     (string-replace-all str (concatenate 'string pattern pattern) pattern)
     (string-replace-all str (concatenate 'string pattern pattern) pattern)))
   ((= (length str1) (length str)) str1)
    (setf str str1)))

(defun string-prepare-to-query(str)
  "Подготавливает строку, введенную пользователем, для участия в запросе
Подготовка заключется в отсечении начальных и конечных пробелов и замене оставшихся пробелов на знаки %"
  (substitute #\% #\Space (concatenate 'string "%" (string-mpattern-to-spattern " " (string-trim " " str)) "%")))

(defun read-number-from-string (str &optional (default 0.0))
"Выполняет чтение из строки вещественного числа.
Если число не удалось считать - возвращается default."
  (let ((val (scan-to-strings "(([+-]?\\d+)[.,]?)\\d*([ed][+-]?\\d+)?" str))) 
    (cond
      ((stringp val) (read-from-string (string-replace-all val "," "."))) 
      (t default))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-ppcre:split "(\\(*\\))" "(1+2(3+4) ) ")

(cl-ppcre:scan-to-strings "(abcd|abef)" "abf")

(cl-ppcre:scan-to-strings "ab(cd|ef)" "abef")

(cl-ppcre:scan-to-strings "(abcd|abef)" "abf")

(defun weight (x) 
  (cond
    ((EQ x #\+) 1) 
    ((EQ x #\-) 1) 
    ((EQ x #\*) 2) 
    ((EQ x #\\) 2) 
    ((EQ x #\/) 2) 
    ((EQ x #\^) 3) 
    (T 5)))

(defun opcode (op) 
  (cond ((EQ op #\+) #\+)
	((EQ op #\-) #\-)
	((EQ op #\*) #\*)
	((EQ op #\\) #\\)
	((EQ op #\/) #\/)
	((EQ op #\^) #\^) 
	(T (break "~S~S" "Неверен код операции " op))))

(defun inf-aux  (ae operators operands) 
  (inf-iter (CDR ae) operators (CONS (CAR ae) operands))) 
 
 
(defun inf-iter (ae operators operands) 
  (PROG NIL 
     (cond ((AND (NULL ae) (NULL operators)) (RETURN (CAR operands))))
     (cond ((AND 
	     (NOT (NULL ae)) 
	     (OR (NULL operators) (GREATERP (weight (CAR ae)) (weight (CAR operators))))
	     )
	    (RETURN (inf-aux (CDR ae) (CONS (CAR ae) operators) operands)))) 
     (RETURN (inf-iter ae (CDR operators) 
		       (CONS (LIST (opcode (CAR operators))
				   (CADR operands) (CAR operands))
			     (CDDR operands))))))
 
(defun inf2pref (x) 
  (PROG (hd tl cc xx rr) 
     (cond ((atomlist x) (RETURN (inf-aux x NIL NIL))))
     (SETQ rr NIL)
     (SETQ xx x) 
     LOOP (SETQ hd (CAR xx)) 
     (SETQ tl (CDR xx)) 
     (cond ((memb hd (QUOTE (SIN COS LOG EXP ATN ASN ACS SH CH SQR SIGN ABS))) 
	    (PROGN (SETQ rr (APPEND rr (LIST (LIST hd (inf2pref (CAR tl))))))
		   (SETQ tl (CDR tl))))
	   ((ATOM hd) 
	    (SETQ rr (APPEND rr (LIST hd))))
	   (T (SETQ rr (APPEND rr (LIST (inf2pref hd))))))
     (cond ((NULL tl)
	    (RETURN (inf-aux rr NIL NIL)))) (SETQ xx tl) (GO LOOP)))
 
(defun CalcExpr (expression) (EVAL (inf2pref (PARSE expression))))
