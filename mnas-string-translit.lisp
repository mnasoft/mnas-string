;;;;mnas-string-translit.lisp

(in-package #:mnas-string)

(annot:enable-annot-syntax)

(export '*cir-gr->en*)
(export '*space-cir-gr->en*)

(defparameter *space* " .()")

(defparameter *minus* "-+!!")

(defparameter *greek-capital-letter*           "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")

(defparameter *greek-small-letter*             "αβγδεζηθικλμνξοπρστυφχψω")

(defparameter *greek->english-capital-letter*  "ABGDEFHQIKLMNZOPRSTYUXVW") ; "CJ"

(defparameter *greek->english-small-letter*    "abgdefhqiklmnzoprstyuxvw") ; "cj"

(defparameter *cyrillic-capital-letter*       "ЁАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")

(defparameter *cyrillic-small-letter*         "ёабвгдежзийклмнопрстуфхцчшщъыьэюя")

(defparameter *cyrillic->english-capital-letter* '("IO" "A" "B"  "V" "G" "D" "E"  "ZH" "Z" "I" "IY" "K" "L" "M" "N" "O" "P" "R" "S" "T" "U" "F" "H" "TS" "CH" "SH" "SHCH" "_" "Y" "-" "E" "YU" "YA"))

(defparameter *cyrillic->english-small-letter*   '("io" "a" "b"  "v" "g" "d" "e"  "zh" "z" "i" "iy" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "f" "h" "ts" "ch" "sh" "shch" "_" "y" "-" "e" "yu" "ya"))
  
(defparameter *cir-gr->en* (make-hash-table)
  "Хеш-таблица. 

Служит для преобразования кириллических и греческих символов
в английские символы.")

(defparameter *space-cir-gr->en* (make-hash-table)
    "Хеш-таблица. 

Служит для преобразования кириллических и греческих символов
в английские символы.

При преобразовании с использованием функции translit пробельные и специальные символы заменяются такими,
что преобразованная строка может стпользоваться в качестве имени (символа).
")

(defun init-cir-gr->en ()
  (let ((ht *cir-gr->en*))
    (flet ((add-to-ht (key val) (setf (gethash key ht) val)))  
      (mapc #'add-to-ht
	    (concatenate 'list *cyrillic-capital-letter*  *cyrillic-small-letter* )
	    (concatenate 'list *cyrillic->english-capital-letter* *cyrillic->english-small-letter*))
      (mapc #'add-to-ht
	    (concatenate 'list *greek-capital-letter* *greek-small-letter*)
	    (concatenate 'list *greek->english-capital-letter*  *greek->english-small-letter*)))))


(defun init-space-cir-gr->en ()
  (let ((ht *space-cir-gr->en*))
    (flet ((add-to-ht (key val) (setf (gethash key ht) val)))  
      (mapc #'add-to-ht
	    (concatenate 'list *cyrillic-capital-letter*  *cyrillic-small-letter* )
	    (concatenate 'list *cyrillic->english-capital-letter* *cyrillic->english-small-letter*))
      (mapc #'add-to-ht
	    (concatenate 'list *greek-capital-letter* *greek-small-letter*)
	    (concatenate 'list *greek->english-capital-letter*  *greek->english-small-letter*))
      (mapc #'add-to-ht (concatenate 'list *space*) (concatenate 'list *minus*)))))

(progn (init-cir-gr->en) (init-space-cir-gr->en))

@export
@annot.doc:doc
"@b(Описание:) translit выполняет транслитерацию (замену) символов, 
находящихся в строке str используя для преобразования хеш-таблицу ht.

В качестве таблиц перобразования (хеш-таблицы ht) рекомендуется использовать:
@begin(list)
 @item(*cir-gr->en* - с пробельными символами;)
 @item(*space-cir-gr->en* - с исключением пробельных символов;)
@end(list)
@b(Пример использования:)
@begin[lang=lisp](code)
 (translit \"Что это?\" :ht *cir-gr->en*)       => \"CHto eto?\"
 (translit \"Что это?\" :ht *space-cir-gr->en*) => \"CHto-eto?\"
@end(code)
"
(defun translit (str &key (ht *cir-gr->en*))
  (declare (type string str) )
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (multiple-value-bind (ch found) (gethash el ht)
	   (if found
	       (etypecase ch
		 (character (push ch rez))
		 (string    (mapc
			     #'(lambda (el) (push el rez))
			      (coerce ch 'list))))
	       (push el rez))))
     (coerce str 'list))
    (concatenate 'string (nreverse rez))))
