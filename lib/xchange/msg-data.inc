;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yaml-xchange - ChrysaLisp Mini YAML Data Processor
; DEPRECATING - Favoring using lib/ast/data_ast.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xchange/serialize.lisp")
(import "lib/xchange/deserialize.lisp")


(defun data-xser (obj &optional sstrm)
  ; (data-xser object [stream]) -> stream | exception
  (setd sstrm (string-stream (cat "")))
  (serialize obj sstrm))

(defun data-xdeser (sstrm)
  ; (data-xdeser stream) -> object | exception
  (deserialize sstrm))
