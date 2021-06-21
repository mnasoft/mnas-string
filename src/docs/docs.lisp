(defpackage #:mnas-string/docs
  (:use #:cl ) 
  (:nicknames "MSTR/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-string/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-string/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-string          :mnas-string)
      (:mnas-string/parse    nil)
      (:mnas-string/print    nil)
      (:mnas-string/translit nil)
      (:mnas-string/db       nil)
      (:mnas-string/docs     nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-string
      :mnas-string/parse
      :mnas-string/print
      :mnas-string/translit
      :mnas-string/db
      :mnas-string/docs 
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all ()
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-string.
"
    (make-document)
    (make-graphs)
    (codex:document :mnas-string)
    (make-graphs))

;;;; (make-all)
