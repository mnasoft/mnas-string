;;;; package.lisp

;;(defpackage #:mnas-string)

(defpackage #:mnas-string
  (:use #:cl #:cl-ppcre)
;;;; mnas-string.lisp  
  (:export string-replace-all)
  (:export string-mpattern-to-spattern)
  (:export string-prepare-to-query)
  (:export read-from-string-number)
  (:export read-number-from-string)
  (:export string-add-prefix)
  (:export print-universal-date-time)
  (:export print-universal-time)
  (:export print-universal-date)
  (:export print-universal-date-time-fname)
;;;;mnas-string-translit.lisp  
  (:export translit)

  (:export sort-designation-zm)
  (:export demo-translit demo-zm-sort)
  (:export *mon-ru*
	   *mon-ua*
	   *mon-en*)
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
