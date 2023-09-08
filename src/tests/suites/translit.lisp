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
           "S_esh- zhe eshchio etih myagkih frantsuzskih bulochek da vypeiy chayu!"))
  (is-true
   (equalp (mnas-string/translit:translit "-3.45;+32.89" :ht mnas-string/translit:*cfx->en* )
           "m3i45;p32i89"))
  (is-true
   (equalp (mnas-string/translit:translit "-3.45,+32.89" :ht (mnas-string/translit:make-transliter ",.+-" "!|pm"))
           "m3|45!p32|89")))



