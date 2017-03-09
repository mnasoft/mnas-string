;;;; package.lisp

(defpackage #:mnas-string
  (:use #:cl #:cl-ppcre)
  (:export string-replace-all
	   string-mpattern-to-spattern
	   string-prepare-to-query
           read-number-from-string
	   sort-designation-zm
	   read-from-string-number
	   string-quote
	   string-add-prefix
	   print-universal-date-time
	   print-universal-date
	   print-universal-time
	   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
