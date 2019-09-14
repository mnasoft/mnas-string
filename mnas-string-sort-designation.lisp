;;;; mnas-string-sort-designation.lisp

(in-package #:mnas-string)

;;; "mnas-string" goes here. Hacks and glory await!

(defun zm-ob-split (str) (split "-" str ))

(defun zm-ob-main (str) (first (zm-ob-split str)))

(defun zm-is-type(str)
  (let* ((str-main (zm-ob-main str))
	 (str-len (length str-main)))
    (cond
      ((<= str-len 4) "AC")
      ((= str-len 9) "SERIYA")
      ((= str-len 11) "OPYT")
      (T "UNKNOWN"))))

(defun zm-group-serija (str) (subseq str 3 5))

(defun zm-group-opyt (str) (subseq str 5 8))

(defun zm-group-ac (str)
  (let ((gr (subseq str 2)))
    (cond
      ((= (length gr) 0) "00")
      ((= (length gr) 1) (concatenate 'string "0" gr))
      ((> (length gr) 1) gr))))

(defun zm-group (str)
  (let* ((str-main (zm-ob-main str))
;;;;	 (str-len (length str-main))
	 )
    (cond
      ((string= (zm-is-type str) "SERIYA")
       (zm-group-serija str-main))
      ((string= (zm-is-type str) "OPYT")
       (zm-group-opyt str-main))
      ((string= (zm-is-type str) "AC")
       (zm-group-ac str-main)))))


(defun zm-engine-serija (str) (list (subseq str 1 3) (subseq str 0 1)))

(defun zm-engine-opyt (str) (list (subseq str 3 5) (subseq str 2 3 )))

(defun zm-engine-ac () (list "АС" "I"))

(defun zm-engine (str)
    (let ((str-main (zm-ob-main str)))
    (cond
      ((string= (zm-is-type str) "SERIYA")
       (zm-engine-serija str-main))
      ((string= (zm-is-type str) "OPYT")
       (zm-engine-opyt str-main))
      ((string= (zm-is-type str) "AC")
       (zm-engine-ac)))))

(defun zm-master-serija () "M00")

(defun zm-master-opyt (str) (concatenate 'string "M0" (subseq str 1 2)))

(defun zm-master-ac () "M50")

(defun zm-master (str)
  (let ((str-main (zm-ob-main str)))
    (cond
      ((string= (zm-is-type str) "SERIYA")
       (zm-master-serija))
      ((string= (zm-is-type str) "OPYT")
       (zm-master-opyt str-main))
      ((string= (zm-is-type str) "AC")
       (zm-master-ac)))))

(defun zm-unit-serija (str)
  (let ((unt (subseq str 5 6)))
    (if (or (string= unt "8") (string= unt "9") )
	t
	nil)))

(defun zm-unit-opyt (str)
    (let ((unt (subseq str 8 9)))
    (if (or (string= unt "8") (string= unt "9") )
	t
	nil)))

(defun zm-unit-ac () nil)

(defun zm-unit-p (str)
  (let ((str-main (zm-ob-main str)))
    (cond
      ((string= (zm-is-type str) "SERIYA")
       (zm-unit-serija str-main))
      ((string= (zm-is-type str) "OPYT")
       (zm-unit-opyt str-main))
      ((string= (zm-is-type str) "AC")
       (zm-unit-ac )))))

(defun zm-part-p (str) (null (zm-unit-p str)))

(defun zm-unit-p-key (str)
  (cond
    ((zm-unit-p str) "2")
    ((zm-part-p str) "3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zm-number-serija (str)
  (subseq str 5))

(defun zm-number-opyt (str)
  (subseq str 8))

(defun zm-prepand-n (str prep-str n)
  (if (>= (length str) n)
      str
      (zm-prepand-n (concatenate 'string prep-str str) prep-str n)))

(defun zm-number-ac (str-spl)
  (cond
    ((= (length str-spl) 2)
     (zm-prepand-n (second str-spl) "0" 4))
    ((= (length str-spl) 3)
     (concatenate 'string
		  (zm-prepand-n (second str-spl) "0" 4)
		  "-"
		  (third str-spl)))))



(defun zm-number (str)
  (let ((str-spl  (zm-ob-split str))
;;;;	(str-main (zm-ob-main str))
	)
    (cond
      ((string= (zm-is-type str) "SERIYA")
       (zm-number-serija str))
      ((string= (zm-is-type str) "OPYT")
       (zm-number-opyt str))
      ((string= (zm-is-type str) "AC")
       (zm-number-ac str-spl)))))

(defun zm-key (str)
  (concatenate 'string
	       (zm-unit-p-key str) "-"
	       (zm-master str) "-"
	       (first (zm-engine str)) "-"
	       (second (zm-engine str)) "-"
	       (zm-group str) "-"
	       (zm-number str)))


(export 'string-prepare-to-query)

(defun sort-designation-zm (seq)
  "Выполняет сортировку обозначений деталей и сборочных единиц для формирования спецификации.
Пример использования:
  (defparameter *ob*
  '(\"090038045-04\"    \"090038045\"    \"090038044-01\"    \"090038045-02\"    \"Г90006545\"    \"В7110011856\"    
    \"В7110011856-01\"  \"В7110011856-02\"    \"В8Г90003855\"    \"В7110011956\"    \"В7110011756\"    \"АС1-58\"    
    \"АС10-14\"    \"АС4-136\"    \"АС04-0580\"    \"АС01-52\"))
(sort-designation-zm *ob*)
"
  (sort seq #'string< :key #'zm-key))
