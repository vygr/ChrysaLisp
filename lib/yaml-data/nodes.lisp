;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nodes - ChrysaLisp Object YAML Emiter Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defun Node (ttype &optional value)
  (properties
      :type         ttype
      :value        value
      :indent       0
      :parent       nil
      :representer  nil
      :children     (list)))

(defun cr_present (&optional i n)
  (setd i 0 n "")
  (str (pad "" (* i 2)) n (char 0x0a)))

(defun cr_block_present (i n)
  )

(defun cr_flow_present (i n)
  )

(defun DocStartNode ()
  (setp! (Node :docstart "---") :representer cr_present))

(defun DocEndNode ()
  (seto! (Node :docstart "...") :representer cr_present))

(defun ScalarNode (stype value)
  (setp! (Node :scalar value) :stype stype))

(defun KeyNode (stype value)
  (setp! (Node :key value) :stype stype))

(defun ValueNode (stype value)
  (setp! (Node :value value) :stype stype))

(defun SequenceNode ())
(defun BlockSequenceNode ())
(defun FlowSequenceNode ())
(defun MapNode ())
(defun BlockMapNode ())
(defun FlowMapNode ())

(defun node-children (_)
  (getp _ :children))

(defun add-child (p c)
  (push (node-children p) (setp! c :parent p)))
