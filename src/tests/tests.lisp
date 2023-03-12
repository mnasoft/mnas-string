;;;; .src/tests/package.lisp

(defpackage :mnas-string/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-string/tests)

(defun run-tests () (run! 'all))

(def-suite all
  :description "Мастер-набор всех тестов проекта mnas-package.")

(in-suite all)

;;;;(run-tests)
