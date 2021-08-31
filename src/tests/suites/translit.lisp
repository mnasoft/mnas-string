;;;; ./src/tests/suites/translit.lisp

(in-package :mnas-string/tests)

(def-suite translit
  :description "Мастер-набор всех тестов системы mnas-string/translit."
  :in all)

(in-suite translit)

(def-test translit ()
  (is-true
   (equalp (mnas-string/translit:translit "Что это?" :ht mnas-string/translit:*cir-gr->en*)
           "CHto eto?"))
  (is-true
   (equalp (mnas-string/translit:translit "Что это?" :ht mnas-string/translit:*space-cir-gr->en*)
           "CHto-eto?"))
  (is-true
   (equalp (mnas-string/translit:translit "Съешь же ещё этих мягких французских булочек да выпей чаю!")
           "S_esh- zhe eshchio etih myagkih frantsuzskih bulochek da vypeiy chayu!")))





