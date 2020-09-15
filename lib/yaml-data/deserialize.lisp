;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deserialize - ChrysaLisp Mini YAML Object De-Serializer
; Converts string-stream to ChyrsaLisp object form
;
; Primary usage is intended to the CPU interchange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/tokens.lisp)
(import 'lib/yaml-data/reader.lisp)

(defun-bind deserialize (sstrm)
  ; (deserialize stream) -> object
  )