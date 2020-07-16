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

(progn
  (def pscan :reader (dictionary))
  (def (dict-get pscan :reader)
    :buffer     nil
    :buff_len   0
    :buff_pos   0
    :line       1
    :column     1))

; Reader forms

(defunp-bind pscan 'reader ()
  ; (reader) -> :reader dictionary
  (dict-get pscan :reader))

(defunp-bind pscan 'setup-reader (buffer)
  ; (setup-reader buffer) -> :reader dictionary
  (defq cntrl (callp pscan 'reader))
  (dict-puts! cntrl
    :buffer buffer
    :buff_len (length buffer))
  cntrl)

(defunp-bind pscan 'rpeek (&optional pndx)
  ; (rpeek [i]) -> nil | char
  (defq cntrl (callp pscan 'reader))
  (elem (+ (dict-get cntrl :buff_pos) (opt pndx 0)) (dict-get cntrl :buffer)))

(defunp-bind pscan 'rnext ()
  ; (rnext) -> nil | char
  (defq
    cntrl (callp pscan 'reader)
    pos   (dict-get cntrl :buff_pos)
    clin  (dict-get cntrl :line)
    ccol  (dict-get cntrl :column)
    npos  (inc pos)
    ch    nil)
  (cond
    ((< pos (dict-get cntrl :buff_len))
      (dict-put! cntrl :buff_pos npos)
      (dict-put! cntrl :column (inc ccol))
      (setq ch (elem pos (dict-get cntrl :buffer))))
    (t nil))
  (when (and ch (eql ch (char 0x0a)))
    (dict-puts! cntrl :line (inc clin) :column 1))
  ch)

(defunp-bind pscan 'skip-line ()
  ; (skip-line) -> next line
  (defq ch (callp pscan 'rnext))
  (until (or (eql ch nil) (eql ch (char 0x0a)))
    (setq ch (callp pscan 'rnext))))

(defunp-bind pscan 'mark ()
  (defq cntrl (callp pscan 'reader))
  (list
    (dict-get cntrl :line)
    (dict-get cntrl :column)
    (dict-get cntrl :buff_pos)))

; Tokens

(defunp-bind pscan 'gen-base-token ()
  (defq bt (dictionary))
  (dict-puts! bt :mark_start nil :mark_end nil)
  bt)

(defunp-bind pscan 'gen-stream-start-token ()
  (defq ss (callp pscan 'gen-base-token))
  (dict-puts! ss :type :stream-start)
  ss)

(defunp-bind pscan 'gen-stream-end-token ()
  (defq se (callp  pscan 'gen-base-token))
  (dict-puts! se :type :stream-end)
  se)

(defunp-bind pscan 'gen-document-start-token ()
  (defq
    ds (callp  pscan 'gen-base-token)
    ms (callp pscan 'mark)
    me (progn (times 2 (callp pscan 'rnext)) (callp pscan 'mark)))
  (dict-puts! ds
    :type :document-start
    :mark_start ms
    :mark_end me)
  ds)

(defunp-bind pscan 'gen-document-end-token ()
  (defq
    de (callp  pscan 'gen-base-token)
    ms (callp pscan 'mark)
    me (progn (times 2 (callp pscan 'rnext)) (callp pscan 'mark)))
  (dict-puts! de
    :type :document-end
    :mark_start ms
    :mark_end me)
  de)

(defunp-bind pscan 'gen-directive-token ()
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

(defun consume (&optional context)
  (defq
    ch  t
    res (list))
  (push res (callp pscan 'gen-stream-start-token))
  (while ch
    (setq ch (callp pscan 'rnext))
    (cond
      ((eql ch "#") (callp pscan 'skip-line))
      ((eql ch (char 0x0a)))
      ((eql ch " "))
      ((eql ch "-") (push res (callp pscan 'gen-document-start-token)))
      ((eql ch ".") (push res (callp pscan 'gen-document-end-token)))
      ((eql ch "&") (print "anchor"))
      ((eql ch ":") (print "key declared"))
      ((eql ch "[") (print "flow seq start"))
      ((eql ch "]") (print "flow seq end"))
      ((eql ch "{") (print "flow dict start"))
      ((eql ch "}") (print "flow dict end"))
      ; Need to handle the arbitraries
      (t
        ; (when ch (push res ch))
        nil)))

  (push res (callp pscan 'gen-stream-end-token)))

(defun scan (buffer)
  ; (scan stream) -> list of tokens
  ; Scans through the istrm and tokenizes
  ; into YAML standard token ids
  (callp pscan 'setup-reader buffer)
  (each (#(print
    (dict-get %0 :type)
    " "
    (dict-get %0 :mark_start)
    " "
    (dict-get %0 :mark_end))) (consume))
  ; (callp pscan 'reader)
  )
