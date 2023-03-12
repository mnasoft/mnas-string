;;;; mnas-string-test.lisp

(in-package :mnas-string)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *GPL* (uiop:read-file-string "~/quicklisp/local-projects/mnas/mnas-string/LICENSE"))

(dotimes (i 500)
 (setf *GPL*-500 (concatenate 'string *GPL*-500 *GPL*)
    ))

(require :sb-sprof)

(declaim (optimize speed))

(defun str-split-1 ()
  (dotimes (i 500)
    (str:split " " *GPL*  :omit-nulls t)
    ))

(defun str-split-2 ()
  (dotimes (i 500)
    (mnas-string:split " " *GPL*)
    ))


(sb-sprof:with-profiling (:max-samples 1000
                          :report :flat
                          :loop nil)
  (str-split-1)
  (str-split-2))



(sb-sprof:reset)

(defun test-split ()
  (dotimes (i 5000)
    (str:split " " *GPL*  :omit-nulls t)
    (mnas-string:split " " *GPL*)))

(progn
  (sb-profile:profile test-split
                      mnas-string:split
                      str:split
                      )
  (str-splittt)
  (sb-profile:report)
  (sb-profile:reset)
  (sb-profile:unprofile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sb-sprof:report)

(declaim (optimize speed))

(defun cpu-test-inner (a i)
  (logxor a
          (* i 5)
          (+ a i)))

(defun cpu-test (n)
  (let ((a 0))
    (dotimes (i (expt 2 n) a)
      (setf a (cpu-test-inner a i)))))

(sb-sprof:with-profiling (:max-samples 1000
                          :report :flat
                          :loop nil)
  (cpu-test 26))


(sb-sprof:profile-call-counts "CL-USER")

(sb-sprof:with-profiling (:max-samples 1000
                          :report :flat
                          :loop t
                          :show-progress t)
  (cpu-test 24))

