(in-package :cl-user)

(require :sb-sprof)
(require :str)

(declaim (optimize speed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; static profiler

(sb-sprof:reset)

(defparameter *GPL* (uiop:read-file-string "~/quicklisp/local-projects/mnas/mnas-string/LICENSE"))

(defun test-split (char-bag)
  (format t "~&(test-split ~S)" char-bag)
  (format t "~&====================================================================================================")
  (dotimes (i 5000)
    (str:split             "[\\.]" *GPL*  :omit-nulls t)
    (mnas-string:split     char-bag *GPL*)
    (mnas-string::split-01 "[\\.]" *GPL*)))

(progn
  (sb-profile:profile test-split
                      mnas-string:split
                      mnas-string::split-01
                      str:split
                      )
  (sb-profile:reset)
  (test-split ".")
  (sb-profile:report)
  (sb-profile:unprofile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; modern profiler

(sb-sprof:reset)

(defun str-split (char-bag &key (times 500))
  (dotimes (i times)
    (str:split char-bag *GPL*  :omit-nulls t)
    ))

(defun mnas-split (char-bag &key (times 500))
  (dotimes (i times)
    (mnas-string:split char-bag *GPL*)
    ))

(defun mnas-split-01 (char-bag &key (times 500))
  (dotimes (i times)
    (mnas-string::split-01 char-bag *GPL*)
    ))

(let ((char-bag " "))
  (sb-sprof:reset)
  (format t "~&(str-split ~S)" char-bag)
  (format t "~&====================================================================================================")
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :flat
                            :loop nil)
    (str-split char-bag)))

(let ((char-bag " "))
  (sb-sprof:reset)
  (format t "~&(mnas-split ~S)" char-bag)
  (format t "~&====================================================================================================")
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :flat
                            :loop nil)
    (mnas-split char-bag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mnas-string)

(defun split (char-bag string &key (omit-nulls t))
  (check-type string string)
  (check-type char-bag string)
  (let ((char-bag-hash (make-populated-hash-table (map-to-list char-bag)))
        (s-l (length string))
	(rez '(-1))
	(rezult))
    (loop :for i :from 0 :below s-l :do
      (when (nth-value 1 (gethash (char string i) char-bag-hash))
	(push i rez)))
    (loop :for i :in rez
          :for j :in (push s-l rez)
          :do (push (subseq string (1+ i) j) rezult))
    (if omit-nulls 
	(mapcan  #'(lambda (x)
                     (check-type x string)
		     (if (= (length x) 0) nil (list x)))
		 rezult)
	rezult)))

(defun split-01 (char-bag string &key (omit-nulls t))
  (check-type string string)
  (check-type char-bag string)
  (let ((rezult))
    (setf rezult
          (cl-ppcre:split
           (format nil "[~{\\~C~}]"
                   (loop :for c :across char-bag :collect c))
           string))
    (if omit-nulls 
	(mapcan  #'(lambda (x)
                     (check-type x string)
		     (if (= (length x) 0) nil (list x)))
		 rezult)
	rezult)))

(defun split-01 (regex string &key (omit-nulls t))
  (check-type string string)
  (check-type regex string)
  (let ((rezult))
    (setf rezult
          (cl-ppcre:split regex string))
    (if omit-nulls 
	(mapcan  #'(lambda (x)
                     (check-type x string)
		     (if (= (length x) 0) nil (list x)))
		 rezult)
	rezult)))

(defparameter *test-string* "")

(let ((test-string "123           234  345"))
  (setf *test-string* test-string)
  (split-01 " " test-string))
