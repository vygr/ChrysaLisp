;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tokens - ChrysaLisp YAML Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/reader.inc)

; STREAM-START
; STREAM-END
; DOCUMENT-START
; DOCUMENT-END
; BLOCK-SEQUENCE-START
; BLOCK-MAPPING-START
; BLOCK-END
; FLOW-SEQUENCE-START
; FLOW-MAPPING-START
; FLOW-SEQUENCE-END
; FLOW-MAPPING-END
; BLOCK-ENTRY
; FLOW-ENTRY
; KEY
; VALUE
; SCALAR(value, plain, style)
; DIRECTIVE(name, value)    ----- NOT SUPPORTED
; ALIAS(value)              ----- NOT SUPPORTED
; ANCHOR(value)             ----- NOT SUPPORTED
; TAG(value)                ----- NOT SUPPORTED


(defun-bind Token (ttype sm em)
  (properties
      :type       ttype
      :start_mark sm
      :end_mark   sm))

(defun-bind StreamStart ()
  (Token :stream_start nil nil))

(defun-bind StreamEnd ()
  (Token :stream_end nil nil))

(defun-bind DocumentStart ()
  (Token :document_start nil nil))

(defun-bind DocumentEnd ()
  (Token :document_end nil nil))

(defun-bind BlockSequenceStart ()
  (Token :blockseq_start nil nil))

(defun-bind BlockMappingStart ()
  (Token :blockmap_start nil nil))

(defun-bind BlockEntry ()
  (Token :block_entry nil nil))

(defun-bind BlockEnd ()
  (Token :block_end nil nil))

(defun-bind FlowSequenceStart ()
  (Token :flowseq_start nil nil))

(defun-bind FlowSequenceEnd ()
  (Token :flowseq_end nil nil))

(defun-bind FlowMappingStart ()
  (Token :flowmap_start nil nil))

(defun-bind FlowMappingEnd ()
  (Token :flowmap_end nil nil))

(defun-bind FlowEntry ()
  (Token :flow_entry nil nil))

(defun-bind Key ()
  (Token :key nil nil))

(defun-bind Value ()
  (Token :value nil nil))

(defun-bind Scalar ()
  (Token :scalar nil nil))
