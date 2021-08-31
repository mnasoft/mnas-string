;;;; ./src/tests/suites/parse.lisp

(in-package :mnas-string/tests)

(def-suite parse
  :description "Мастер-набор всех тестов системы mnas-string/parse."
  :in all)

(in-suite parse)

(def-test read-number ()
  (is-true (= (mnas-string/parse:read-number "3.14") 3.14))
  (is-true (= (mnas-string/parse:read-number "3,14") 3)))

(def-test parse-number ()
  (is-true (= (mnas-string/parse:parse-number "3.14") 3.14))
  (is-true (= (mnas-string/parse:parse-number "3,14") 3.14)))

