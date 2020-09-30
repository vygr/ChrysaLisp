;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-xchange - ChrysaLisp Mini YAML Data Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/yaml-data/serialize.lisp")
(import "lib/yaml-data/deserialize.lisp")


(defun-bind yaml-xser (obj &optional sstrm)
  ; (yaml-xser object [stream]) -> stream | exception
  (setd sstrm (string-stream (cat "")))
  (serialize obj sstrm))

(defun-bind yaml-xdeser (sstrm)
  ; (yaml-xdeser stream) -> object | exception
  (deserialize sstrm))
