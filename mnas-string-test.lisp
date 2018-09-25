;;;; mnas-string-test.lisp

(in-package #:mnas-string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (defparameter *en-kbd-characters* "qwertyuiop[]asdfghjkl;'\zxcvbnm,./`QWERTYUIOP[]ASDFGHJKL;'\ZXCVBNM,./`" "Английские символы, при наборе с клавиатуры")
  (defparameter *ru-kbd-characters* "йцукенгшщзхъфывапролджэ\ячсмитьбю.ёЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭ\ЯЧСМИТЬБЮ.Ё" "Русские символы, при наборе с клавиатуры")
  (defparameter *el-kbd-characters* ";ςερτυθιοπ[]ασδφγηξκλ΄'\ζχψωβνμ,./`;ςΕΡΤΥΘΙΟΠ[]ΑΣΔΦΓΗΞΚΛ΄'\ΖΧΨΩΒΝΜ,./`" "Греческие символы, при наборе с клавиатуры"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Заброшенный код
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
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
  "Пример использования:
;;;; (opcode #\a)"
  (if (member op '(#\+ #\- #\* #\\ #\/ #\^))
      op
      (error "~A ~S" "Неверен код операции: " op)))

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


