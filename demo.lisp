;;;; mnas-string-sort-designation.lisp

(in-package #:mnas-string)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (demo-translit)
@end(code)
"
(defun demo-translit ()
  (translit "Съешь же ещё этих мягких французских булочек да выпей чаю!"))

@export
@annot.doc:doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (demo-zm-sort) 
@end(code)
"
(defun demo-zm-sort ()
  (let ((ob   '("090038045-04"    "090038045"    "090038044-01"    "090038045-02"    "Г90006545"    "В7110011856"    
		"В7110011856-01"  "В7110011856-02"    "В8Г90003855"    "В7110011956"    "В7110011756"    "АС1-58"    
		"АС10-14"    "АС4-136"    "АС04-0580"    "АС01-52")))
    (sort-designation-zm ob)))
