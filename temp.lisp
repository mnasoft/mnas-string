;;;; temp.lisp

(in-package #:mnas-string)

(defparameter *trd-files*
  (uiop:directory-files
   (concatenate 'string (getenv "MSYS2_HOME" "") "/home/namatv/org/knil-2/trd/DG80/") #P"*.trd"))

(export 'getenv)
(defun getenv (x &optional (default ""))
  "Пример использования:
 (getenv \"SBCL_HOME\")
 (getenv \"PATH\")
"
  (cond
    ((uiop:getenv x))
    (t default)))

(file-namestring (first *trd-files*))
(directory-namestring (first *trd-files*))

(host-namestring (first *trd-files*))
(native-namestring (first *trd-files*))
(parse-namestring (first *trd-files*))
(enough-namestring (first *trd-files*))

(pathname-device (first *trd-files*))
(pathname-directory (first *trd-files*))
(pathname-name (first *trd-files*))
(pathname-type (first *trd-files*))

(rename-file #P"D:/PRG/msys32/home/namatv/org/knil-2/trd/2019-09-20_084109.trd"
	     #P"D:/PRG/msys32/home/namatv/org/knil-2/trd/200919_084109.trd"
	     )

(map
 'nil
 #'(lambda (el)
     )
 (uiop:directory-files #P"D:/PRG/msys32/home/namatv/org/knil-2/trd/" #P"*.trd")
 )


