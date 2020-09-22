;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; serialize - ChrysaLisp Mini YAML Object Serializer
; Converts ChrysaLisp object tree to a compact
; string-stream form.
;
; Primary usage is intended to the CPU interchange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/yaml-data/commons.lisp)

(defq ser_boolean
      (properties
        t   "*true"
        nil "*false"))

(defun node-to-xchange-stream (sstrm ast)
  (defq recur (curry node-to-xchange-stream sstrm))
  (case (getp ast :type)
    ((:map)
     (write sstrm "{")
     (each recur (getp ast :children))
     (write sstrm "}"))
    ((:seq)
     (write sstrm "[")
     (each recur (getp ast :children))
     (write sstrm "]"))
    ((:map_entry)
     (each recur (getp ast :children)))
    ((:key)
     (each recur (getp ast :children)))
    ((:value)
     (each recur (getp ast :children)))
    ((:scalar)
     (if (eql (getp ast :stype) :boolean)
         (write sstrm (str (getp ser_boolean (getp ast :value)) " "))
         (write sstrm (str (getp ast :value) " "))))
    (t (throw "Unknown " ast))))

(defun-bind serialize (obj sstrm)
  ; (serialize object stream) -> stream
  (defq cntxt (Context))
  (obj-to-node cntxt obj)
  (node-to-xchange-stream sstrm (getp cntxt :current))
  sstrm)