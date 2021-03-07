;;;; ./src/tests/print.lisp

(in-package :mnas-string/tests)

(def-suite print
  :description "Мастер-набор всех тестов системы mnas-string/print."
  :in all)

(in-suite print)

(defparameter *ut* (encode-universal-time 16 0 23 7 3 2021))

(def-test date ()
  (is-true
   (equalp (mnas-string/print:date *ut* :stream nil)
            "2021-03-07"))
   (is-true
    (equalp (mnas-string/print:date *ut* :stream nil :year nil :day nil)
             "Март")))

(def-test date-time ()
  (is-true
   (equalp  (mnas-string/print:date-time *ut* :stream nil                  )
            "2021-03-07_23:00:16"))
  (is-true
   (equalp  (mnas-string/print:date-time *ut* :stream nil :year nil        )
            "03-07_23:00:16"))
  (is-true
   (equalp  (mnas-string/print:date-time *ut* :stream nil :year nil :ss nil)
            "03-07_23:00"
            ))
  (is-true
   (equalp  (mnas-string/print:date-time *ut* :stream nil           :ss nil)
            "2021-03-07_23:00")))

(def-test date-time-fname ()
  (is-true
   (equalp (mnas-string/print:date-time-fname *ut* :stream nil :year t   :ss t)
            "2021-03-07_23-00-16"))
    (is-true
     (equalp (mnas-string/print:date-time-fname *ut* :stream nil :year nil :ss t)
              "03-07_23-00-16"))
    (is-true
     (equalp (mnas-string/print:date-time-fname *ut* :stream nil :year t   :ss nil)
             "2021-03-07_23-00"))
    (is-true
     (equalp (mnas-string/print:date-time-fname *ut* :stream nil :year nil :ss nil)
              "03-07_23-00")))

(def-test day-time ()
  (is-true
   (equalp (mnas-string/print:day-time *ut* :stream nil)
           "23:00:16"))
    (is-true
   (equalp (mnas-string/print:day-time *ut* :stream nil :ss nil)
            "23:00")))

