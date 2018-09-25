;;;;mnas-string-translit.lisp

(in-package #:mnas-string)

(progn
  (defparameter *greek-capital-letter*  "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")
  (defparameter *greek-small-letter*    "αβγδεζηθικλμνξοπρστυφχψω")
  (defparameter *greek->english-capital-letter*  "ABGDEFHQIKLMNZOPRSTYUXVW") ; "CJ"
  (defparameter *greek->english-small-letter*    "abgdefhqiklmnzoprstyuxvw") ; "cj"
  (defparameter *cyrillic-capital-letter*  "ЁАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")
  (defparameter *cyrillic-small-letter*    "ёабвгдежзийклмнопрстуфхцчшщъыьэюя")
  (defparameter *cyrillic->english-capital-letter* '("IO" "A" "B"  "V" "G" "D" "E"  "ZH" "Z" "I" "IY" "K" "L" "M" "N" "O" "P" "R" "S" "T" "U" "F" "H" "TS" "CH" "SH" "SHCH" "_" "Y" "-" "E" "YU" "YA"))
  (defparameter *cyrillic->english-small-letter*   '("io" "a" "b"  "v" "g" "d" "e"  "zh" "z" "i" "iy" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "f" "h" "ts" "ch" "sh" "shch" "_" "y" "-" "e" "yu" "ya"))
  (defparameter *cir-gr->en* (make-hash-table))
  (let ((ht *cir-gr->en*))
    (flet ((add-to-ht (key val) (setf (gethash key ht) val)))  
      (mapc #'add-to-ht
	    (concatenate 'list  *cyrillic-capital-letter*  *cyrillic-small-letter* )
	    (append *cyrillic->english-capital-letter* *cyrillic->english-small-letter*))
      (mapc #'add-to-ht
	    (concatenate 'list *greek-capital-letter* *greek-small-letter*)
	    (concatenate 'list *greek->english-capital-letter*  *greek->english-small-letter*)))))
  
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
