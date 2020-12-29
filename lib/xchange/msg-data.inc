;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; msg-data - ChrysaLisp Data Serialize/Deserialize
; DEPRECATING - serialize/deserialize in favor of
; ast conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/ast/data_ast.inc")
(import "lib/xchange/serialize.lisp")
(import "lib/xchange/deserialize.lisp")

(defun data-xser (obj &optional sstrm)
  ; (data-xser object [stream]) -> stream | exception
  (setd sstrm (string-stream (cat "")))
  (serialize obj sstrm))

(defun data-xdeser (sstrm)
  ; (data-xdeser stream) -> object | exception
  (deserialize sstrm))

(defun data-xlist (data)
  ; (data-xlist data) -> list
  ; Converts Lisp data/classes to
  ; encoded list for transmission
  (. (data-to-ast data) :tolist))

(defun data-xliststr (data)
  ; (data-xliststr data) -> string
  ; Converts Lisp data/classes to
  ; string of encoded list for transmission
  (str (data-xlist data)))

(defun xlist-data (xlist)
  ; (xlist-data xlist) -> list
  ; Converts encoded transmission list
  ; to list Lisp data/classes
  (. (list-to-ast xlist) :to_object))

(defun xliststr-data (xliststr)
  ; (xliststr-data xliststring) -> list
  ; Converts string encoded transmission list
  ; to list Lisp data/classes
  (bind '(form _nc) (read (string-stream xliststr) (code " ")))
  (xlist-data form))
