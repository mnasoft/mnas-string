;;;; ./src/tests/core.lisp

(in-package :mnas-string/tests)

(def-suite core
  :description "Мастер-набор всех тестов системы mnas-string/core."
  :in all)

(in-suite core)

(defparameter *str-01* "Мастер-набор всех тестов системы mnas-string/core.")

(defparameter *str-02* " Мастер-набор   всех  тестов   системы  mnas-string/core. ")

(def-test split ()
  (is-true (equalp (mnas-string:split " " *str-01*) (mnas-string:split " " *str-02*)))
  (is-true (equalp (mnas-string:split " " *str-02*) '("Мастер-набор" "всех" "тестов" "системы" "mnas-string/core.")))
  )

(def-test replace-all ()
  (is-true (string= (mnas-string:replace-all *str-01* "те" "ти")
                    "Мастир-набор всех тистов систимы mnas-string/core."))
  (is-true (string= (mnas-string:replace-all *str-02* "те" "ти")
                    " Мастир-набор   всех  тистов   систимы  mnas-string/core. "))
  (is-true (string= (mnas-string:replace-all *str-02* "те" "")
                    " Маср-набор   всех  стов   сисмы  mnas-string/core. ")))

(def-test mpattern-to-spattern ()
    (is-true (equalp (mnas-string:mpattern-to-spattern " " *str-02*)
                   " Мастер-набор всех тестов системы mnas-string/core. "))
  (is-true (equalp (mnas-string:mpattern-to-spattern "-" *str-02*)
                   " Мастер-набор   всех  тестов   системы  mnas-string/core. ")))

(def-test pre-post ()
  (is-true (equalp (mnas-string:pre-post *str-01* " " )
                   " Мастер-набор всех тестов системы mnas-string/core. "))
  (is-true (equalp (mnas-string:pre-post *str-01* " " "!")
                   " Мастер-набор всех тестов системы mnas-string/core.!")))

(def-test trd-rename ()
  (is-true
   (string= (mnas-string:trd-rename "150819_082056.trd" "trd")
            "2019-08-15_082056.trd")
   (is-true
    (signals simple-type-error
      (mnas-string:trd-rename "150819_082056.trd" "txt")))))

(def-test getenv ()
  (sb-posix:setenv "BELEBERDA" "BELEBERDA" 0)
  (is-true
   (string= (mnas-string:getenv "BELEBERDA")
            "BELEBERDA"))
  (is-true (string= (mnas-string:getenv "BLA_BLA_BERDA" "BLA_BLA_BLA")
                    "BLA_BLA_BLA"))
  (sb-posix:unsetenv "BELEBERDA"))

(def-test make-populated-hash-table ()
  (let* ((lst '(1 2 3 10 20 30))
         (ht (mnas-string:make-populated-hash-table lst)))
    (dolist (i lst)
      (is-true (= i (gethash i ht))))))

(def-test map-to-list ()
  (let* ((lst '((1 2 )(2 3)(3 4)(4 5)(5 6)(6 7))))
    (is-true (equalp (mnas-string:map-to-list lst) lst))
    (is-true (equalp (mnas-string:map-to-list lst :key #'first)
                     '(1 2 3 4 5 6)))
    (is-true (equalp (mnas-string:map-to-list lst :key #'second)
                     '(2 3 4 5 6 7)))))



