(defpackage #:mnas-string/docs
  (:use #:cl ) 
  (:nicknames "MSTR/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-string/docs) содержит функции
  генерирования и публикации документации.
"))

(in-package :mnas-string/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-string          :mnas-string)
      (:mnas-string/parse    nil)
      (:mnas-string/print    nil)
      (:mnas-string/translit nil)
      (:mnas-string/db       nil)
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
      )
    :do (mnas-package:make-codex-graphs i i)))

  (defun make-all ()
    (make-document)
    (make-graphs)
    (codex:document :mnas-string)
    (make-graphs))

;;;; (make-all)
