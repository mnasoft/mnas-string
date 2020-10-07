;;;; package.lisp

(defpackage #:mnas-string
  (:use #:cl)
  (:export print-universal-date
	   read-from-string-number
	   replace-all
	   make-populated-hash-table
	   string-add-prefix
	   string-mpattern-to-spattern
	   string-replace-all map-to-list
	   read-number-from-string
	   print-universal-date-time
	   demo-zm-sort
	   sort-designation-zm split
	   print-universal-time
	   string-prepare-to-query
	   demo-translit translit
	   trd-rename getenv
	   print-universal-date-time-fname)
  (:export *cir-gr->en*
	   *space-cir-gr->en*
	   *mon-en*
	   *mon-ru*
	   *mon-ua*
	   *omit-nulls*
	   *default-month-language*))

