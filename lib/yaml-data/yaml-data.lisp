;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-data - ChrysaLisp YAML Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; imports
(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/scanner.lisp)

(defun-bind yaml-read (fname &rest options)
  ; (print "yaml-read " fname)
  (if (zero? (age fname))
    (throw (str fname " not found") t)
    (progn
      (defq res (scan (load fname)))
      (print "Done")))
  )

(defun-bind yaml-write (yaml stream &rest options))
