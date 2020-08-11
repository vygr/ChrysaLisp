;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-data - ChrysaLisp YAML Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; imports
(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/scanner.inc)

(defun yaml-read (fname &rest options)
  (if (zero? (age fname))
    (throw (str fname " not found") t)
    (scan (load fname))))

(defun yaml-write (yaml stream &rest options))
