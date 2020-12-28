;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; serialize - ChrysaLisp Mini YAML Object Serializer
; Converts ChrysaLisp object tree to a compact
; string-stream form.
;
; Primary usage is intended to the CPU interchange
; DEPRECATING - Favoring using lib/ast/data_ast.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defq ser_boolean
      (properties
        t   "*true"
        nil "*false"))

(defun node-to-xchange-stream (sstrm ast)
  (defq recur (curry node-to-xchange-stream sstrm))
  (case (gets ast :type)
    ((:map)
     (write sstrm "{")
     (each recur (gets ast :children))
     (write sstrm "}"))
    ((:set)
     (write sstrm "<")
     (each recur (gets ast :children))
     (write sstrm ">"))
    ((:seq)
     (write sstrm "[")
     (each recur (gets ast :children))
     (write sstrm "]"))
    ((:map_entry)
     (each recur (gets ast :children)))
    ((:me_key)
     (each recur (gets ast :children)))
    ((:me_value)
     (each recur (gets ast :children)))
    ((:scalar)
     (if (eql (gets ast :stype) :boolean)
         (write sstrm (str (gets ser_boolean (gets ast :value)) " "))
         (write sstrm (str (gets ast :value) " "))))
    (t (throw "Unknown " (entries ast)))))

(defun serialize (obj sstrm)
  ; (serialize object stream) -> stream
  (defq cntxt (Context))
  (obj-to-node cntxt obj)
  (node-to-xchange-stream sstrm (gets cntxt :current))
  sstrm)