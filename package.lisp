;;;; package.lisp

(defpackage #:mnas-string)

(defpackage #:mnas-string
  (:use #:cl #:cl-ppcre)
;;;; mnas-string.lisp  
  (:export mnas-string::string-replace-all)
  (:export mnas-string::string-mpattern-to-spattern)
  (:export mnas-string::string-prepare-to-query)
  (:export mnas-string::read-from-string-number)
  (:export mnas-string::read-number-from-string)
  (:export mnas-string::string-add-prefix)
  (:export mnas-string::print-universal-date-time)
  (:export mnas-string::print-universal-time)
  (:export mnas-string::print-universal-date)
  (:export mnas-string::print-universal-date-time-fname)
;;;;mnas-string-translit.lisp  
  (:export mnas-string::translit)

  (:export mnas-string::sort-designation-zm)
  (:export mnas-string::demo-translit mnas-string::demo-zm-sort)
  (:export mnas-string::*mon-ru*
	   mnas-string::*mon-ua*
	   mnas-string::*mon-en*)
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

