;;;; tests/main.lisp

(in-package :mnas-string/tests)

(def-suite db
  :description "Мастер-набор всех тестов системы mnas-string/db."
  :in all)

(in-suite db)

(def-test prepare-to-query ()
  (is-true
   (string= (mnas-string/db:prepare-to-query "  гайки  с   квадр гол  ")
            "%гайки%с%квадр%гол%")))
