;;;; temp.lisp

(in-package :mnas-string)

(require :mnas-graph)
(require :mnas-package)

(defparameter *docs-path* "~/quicklisp/local-projects/mnas/mnas-string/docs/")
(defparameter *pkg*       :mnas-string)

(progn
  (mnas-package:package-system-graph *pkg* :fname "package-system-graph" :fpath *docs-path* :out-type "png" :viewer nil) 
  (mnas-package:package-call-graph   *pkg* :fname "package-call-graph"   :fpath *docs-path* :out-type "png" :viewer nil)
  (mnas-package:package-class-graph  *pkg* :fname "package-class-graph"  :fpath *docs-path* :out-type "png" :viewer nil)
  (mnas-package:package-symbol-graph *pkg* :fname "package-symbol-graph" :fpath *docs-path* :out-type "png" :viewer nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-files*
  (uiop:directory-files
   (concatenate 'string (getenv "MSYS2_HOME" "") "/home/namatv/org/knil-2/trd/DG80/") #P"*.trd"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((lst ()))                                                     
  (do-all-symbols (s lst)
    (when (eq (find-package *package*) (symbol-package s)) (push (string-downcase (symbol-name s)) lst)))
  (sort lst #'string> ))

(mnas-package:doc-template)
