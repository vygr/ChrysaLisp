;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scanner - ChrysaLisp YAML Scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Scanner produces tokens of the following types:
; STREAM-START
; STREAM-END
; DIRECTIVE(name, value)
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
; ALIAS(value)
; ANCHOR(value)
; TAG(value)
; SCALAR(value, plain, style)
;

(private pscan)

(defunp-bind pscan 'gen-base-token ()
  (print "Generating token base"))
(defunp-bind pscan 'gen-stream-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-stream-end-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-directive-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-document-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-document-end-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-block-sequence-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-block-mapping-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-block-end-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-flow-sequence-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-flow-mapping-start-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-flow-sequence-end-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-flow-mapping-end-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-block-entry-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-flow-entry-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-key-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-value-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-alias-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-anchor-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-tag-token ()
  (callp pscan 'gen-base-token))
(defunp-bind pscan 'gen-scalar-token ()
  (callp pscan 'gen-base-token))

(defun scan-stream (istrm)
  ; (scan stream) -> list of tokens
  ; Scans through the istrm and tokenizes
  ; into YAML standard token ids
  (callp pscan 'gen-scalar-token))