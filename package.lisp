;;;; package.lisp

(defpackage #:mnas-string
  (:use #:cl #:mnas-string/print)
  (:export read-from-string-number
	   replace-all
	   make-populated-hash-table
	   string-add-prefix
	   string-mpattern-to-spattern
	   string-replace-all map-to-list
	   read-number-from-string
	   demo-zm-sort
           split
	   string-prepare-to-query
	   demo-translit translit
	   trd-rename
           getenv)
  (:export sort-designation-zm)
  (:export print-universal-time
           print-universal-date-time
           print-universal-date
           print-universal-date-time-fname)
  (:export *cir-gr->en*
	   *space-cir-gr->en*
	   *mon-en*
	   *mon-ru*
	   *mon-ua*
	   *omit-nulls*
	   *default-month-language*)
  (:documentation
   " MNAS-string содержит в своем составе функции 
@begin(list)
 @item(вывода даты и времени;)
 @item(преобразования строк;)
 @item(демонстрационные;) 
@end(list)
"))
