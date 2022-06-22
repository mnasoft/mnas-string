;;;; ./src/docs/docs.lisp

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

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-string.
"
  (mnas-package:make-html-path :mnas-string)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-string :mnas-string/docs)
   "Mnas-String"
   '("Mykola Matvyeyev")
   (mnas-package:find-sources "mnas-string")
   :output-format of)
  (codex:document :mnas-string)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-string")
  (mnas-package:rsync-doc "mnas-string"))

;;;; (make-all)
