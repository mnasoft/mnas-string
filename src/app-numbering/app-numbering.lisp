;;;; ./app-numbering.lisp

(in-package :mnas-string)

(defparameter *abc-ru*
  "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ")

(defparameter *abc-ru-depricated*
  "ЁЗЙОЧЬЫЪ")

(defparameter *abc-en*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter *abc-en-depricated*
  (concatenate 'string "IO" "ABCEHKMOPT"))


(defparameter *abc-app*
  (remove-if #'(lambda (el)
                 (member el
                         (coerce
                          (concatenate
                           'string *abc-ru-depricated*
                           *abc-en-depricated*)
                          'list)))
             (concatenate
              'string
              *abc-ru*
              *abc-en*)))

(defun application-name (index
                 &key (start-index 0)
                   (abc *abc-app*)
                 &aux (ind (+ index start-index))
                   (len-abc (length abc)))
  "@b(Описание:) функция @b(app-name) возвращает строку, представляющую
   номер приложения (см. п.4.3.8 ГОСТ 2.105-95).

 @b(Переменые:)
@begin(list)
 @item(index - целое (0 соответствует А);)
 @item(start-index - смещение начального индекса.)
@end(list)"
  (if (< ind len-abc)
      (format nil "~C" (char *abc-app* ind))
      (format nil "~A" (- ind len-abc -1))))
