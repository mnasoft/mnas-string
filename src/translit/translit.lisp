;;;; ./src/translit/translit.lisp

(defpackage #:mnas-string/translit
  (:use #:cl)
  (:export translit
           )
  (:export *cir-gr->en*
           *space-cir-gr->en*
           *cfx->en*
           )
  (:documentation
   "Пакет @b(mnas-string/translit) экспортирует следующие функции:
@begin(list)
 @item(@b(translit) - транслитерация строки.)
@end(list)

 Транслитерация производится с использованием хеш-таблицы
преобразования, в которой каждому заменяемому символу (ключ)
соответствует строка (значение) на которую он заменяется.

 Пакет @b(mnas-string/translit) экспортирует следующие хеш-таблицы
преобразования:

@begin(list)
 @item(*cir-gr->en* - преобразование кириллических и греческих
   символов в латинские;)
 @item(*space-cir-gr->en* - преобразование кириллических и греческих
   символов в латинские, такое что результат мог использоваться в
   качестве символа Common Lisp.)  
 @item(*cfx->en* - преобразование кириллических и греческих и
   символов, запрещенных в пакете ANSYS CFX, в английские символы
   разрешенніе в пакете CFX.)
@end(list)
"))

(in-package #:mnas-string/translit)

(defparameter *forbidden-characters* " .()"
  "Символы, запрещенные для использования в имени символа Common Lisp.")

(defparameter *allowed-characters*   "-+!!"
  "Символы, разрешенные для использования в имени символа Common
  Lisp. Знена для *forbidden-characters*.")

(defparameter *greek-capital-letter*          "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")

(defparameter *greek-small-letter*            "αβγδεζηθικλμνξοπρστυφχψω")

(defparameter *greek->english-capital-letter* "ABGDEFHQIKLMNZOPRSTYUXVW") ; "CJ"

(defparameter *greek->english-small-letter*   "abgdefhqiklmnzoprstyuxvw") ; "cj"

(defparameter *cyrillic-capital-letter*       "ЁАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")

(defparameter *cyrillic-small-letter*         "ёабвгдежзийклмнопрстуфхцчшщъыьэюя")

(defparameter *cyrillic->english-capital-letter* '("IO" "A" "B"  "V" "G" "D" "E"  "ZH" "Z" "I" "IY" "K" "L" "M" "N" "O" "P" "R" "S" "T" "U" "F" "H" "TS" "CH" "SH" "SHCH" "_" "Y" "-" "E" "YU" "YA"))

(defparameter *cyrillic->english-small-letter*   '("io" "a" "b"  "v" "g" "d" "e"  "zh" "z" "i" "iy" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "f" "h" "ts" "ch" "sh" "shch" "_" "y" "-" "e" "yu" "ya"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *forbidden-cfx-letter*            ".-+_:,")

(defparameter *forbidden-cfx->english-letter* '("i" "m" "p" " " " " " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cir-gr->en* (make-hash-table)
  "@b(Описание:) хеш-таблица @b(*cir-gr->en*) служит для
                 преобразования кириллических и греческих символов в
                 английские символы.")

(defparameter *space-cir-gr->en* (make-hash-table)
    "@b(Описание:) хеш-таблица @b(*space-cir-gr->en*) служит для
 преобразования кириллических и греческих символов в английские
 символы.

 При преобразовании с использованием функции @b(translit) пробельные и
специальные символы заменяются такими, что преобразованная строка
может использоваться в качестве имени (символа).")

(defparameter *cfx->en* (make-hash-table)
  "@b(Описание:) хеш-таблица @b(*cfx->en*) служит для преобразования
 кириллических, греческих и символов запрещенных в пакете ANSYS CFX в
 английские символы.

 При преобразовании с использованием функции @b(translit) пробельные и
специальные символы заменяются такими, что преобразованная строка
может использоваться в качестве имени (символа).")

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
      (mapc #'add-to-ht
            (concatenate 'list *forbidden-characters*)
            (concatenate 'list *allowed-characters*)))))

(defun init-cfx ()
  (let ((ht *cfx->en*))
    (flet ((add-to-ht (key val) (setf (gethash key ht) val)))  
      (mapc #'add-to-ht
	    (concatenate 'list *cyrillic-capital-letter*  *cyrillic-small-letter* )
	    (concatenate 'list *cyrillic->english-capital-letter* *cyrillic->english-small-letter*))
      (mapc #'add-to-ht
	    (concatenate 'list *greek-capital-letter* *greek-small-letter*)
	    (concatenate 'list *greek->english-capital-letter*  *greek->english-small-letter*))
      (mapc #'add-to-ht
            (concatenate 'list *forbidden-characters*)
            (concatenate 'list *allowed-characters*))
      (mapc #'add-to-ht
            (concatenate 'list *forbidden-cfx-letter*)
            (concatenate 'list *forbidden-cfx->english-letter*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translit (str &key (ht *cir-gr->en*))
  "@b(Описание:) функция @b(translit) возвращает транслитерированную
 строку, символов находящихся в строке str. Для транслитерации
 исползуется хеш-таблица ht.

 В качестве таблиц перобразования (хеш-таблицы ht) рекомендуется использовать:
@begin(list)
 @item(*cir-gr->en* - с пробельными символами;)
 @item(*space-cir-gr->en* - с исключением пробельных символов.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (translit \"Что это?\" :ht *cir-gr->en*)       => \"CHto eto?\"
 (translit \"Что это?\" :ht *space-cir-gr->en*) => \"CHto-eto?\"
 (translit \"Съешь же ещё этих мягких французских булочек да выпей чаю!\")
 ; => \"S_esh- zhe eshchio etih myagkih frantsuzskih bulochek da vypeiy chayu!\"
@end(code)
"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn (init-cir-gr->en) (init-space-cir-gr->en) (init-cfx))
