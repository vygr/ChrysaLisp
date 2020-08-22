;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-data - ChrysaLisp YAML Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; imports
(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/scanner.lisp)
(import 'lib/yaml-data/parser.lisp)

(defun-bind yaml-read (fname &rest options)
  ; (print "yaml-read " fname)
  (if (zero? (age fname))
    (throw (str fname " not found") t)
    (parse (scan (load fname)) options)))

(defun-bind yaml-write (yaml stream &rest options))
