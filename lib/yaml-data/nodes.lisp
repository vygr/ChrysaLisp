;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nodes - ChrysaLisp Object YAML Emiter Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defun Node (ttype &optional value)
  (properties
      :type         ttype
      :value        value
      :children     (list)))

(defun DocStartNode ()
  (Node :docstart "---"))

(defun DocEndNode ()
  (Node :docend "..."))

(defun MapNode ()
  (Node :map))

(defun MapEntryNode ()
  (Node :map_entry))

(defun KeyNode ()
  (Node :key))

(defun ValueNode ()
  (Node :value))

(defun SequenceNode ()
  (Node :seq))

(defun ScalarNode (stype value)
  (setp! (Node :scalar value) :stype stype t))

(defun add-child-node! (p c)
  (when (not (lst? (defq chs (getp p :children))))
    (throw "Children of node is not a list" p))
  (push chs c)
  c)
