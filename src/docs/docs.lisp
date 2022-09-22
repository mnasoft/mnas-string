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
    :for j :from 1
    :for i :in
    '((:mnas-string          :mnas-string)
      (:mnas-string/parse    nil)
      (:mnas-string/print    nil)
      (:mnas-string/translit nil)
      (:mnas-string/db       nil)
;;;;      (:mnas-string/docs     nil)
      )
    :do
       (progn
         (apply #'mnas-package:document i)
         (format t "~A ~A~%" j i))))

(defun make-graphs ()
  (loop
    :for j :from 1
    :for i :in
    '(:mnas-string
      :mnas-string/parse
      :mnas-string/print
      :mnas-string/translit
      :mnas-string/db
;;;;      :mnas-string/docs 
      )
    :do (progn
          (mnas-package:make-codex-graphs i i)
                   (format t "~A ~A~%" j i))))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :mnas-string)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
