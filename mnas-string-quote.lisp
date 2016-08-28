;;;; mnas-string-quote.lisp

(in-package #:mnas-string)

;;; "mnas-string" goes here. Hacks and glory await!

(defun string-quote (string &optional (pre-post-string "\"" ))
  "Додавляет в нвчвло и конец строки string строку pre-post-string"
  (concatenate 'string pre-post-string string pre-post-string))

