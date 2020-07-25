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

; Reader forms

(defunp-bind pscan 'setup-reader (buffer)
  ; (setup-reader buffer) -> :reader dictionary
  (def pscan :reader (properties))
  (defq tmp (dict-get pscan :reader))
  (setsp! tmp
    :buffer buffer
     :buff_len (length buffer)
     :pre_mark (properties :line 1 :column 1 :buff_pos 0)
     :buff_pos 0
     :line 1
     :column 1
     :sstart_produced nil
     :ssend_produced nil
     :simple_key_allowed t
     :simple_keys (list)
     :indent -1
     :indents (list)
     :flow_level 0
     :is_error nil
     :tokens (list)))

(defunp-bind pscan 'reader ()
  ; (reader) -> :reader dictionary
  (dict-get pscan :reader))

(defunp-bind pscan 'allow-simple-key ()
  (setp (callp pscan 'reader) :simple_key_allowed t))

(defunp-bind pscan 'disallow-simple-key ()
  (setp (callp pscan 'reader) :simple_key_allowed nil))

(defunp-bind pscan 'mark ()
  (defq cntrl (callp pscan 'reader))
  (properties
    :line     (getp cntrl :line)
    :column   (getp cntrl :column)
    :pos      (getp cntrl :buff_pos)))


(defunp-bind pscan 'rpeek (&optional pndx)
  ; (rpeek [i]) -> nil | char
  (defq cntrl (callp pscan 'reader))
  (elem (+ (getp cntrl :buff_pos) (opt pndx 0)) (getp cntrl :buffer)))

(defunp-bind pscan 'rnext ()
  ; (rnext) -> nil | char
  (defq
    cntrl (callp pscan 'reader)
    pos   (getp cntrl :buff_pos)
    clin  (getp cntrl :line)
    ccol  (getp cntrl :column)
    npos  (inc pos)
    ch    nil)
  (cond
    ((< pos (getp cntrl :buff_len))
      (setsp! cntrl
        :pre_mark (properties :line clin :column ccol :pos pos)
        :buff_pos npos
        :column (inc ccol))
      (setq ch (elem pos (getp cntrl :buffer))))
    (t nil))
  (when (and ch (eql ch (char 0x0a)))
    (setsp! cntrl :line (inc clin) :column 1))
  ch)

(defunp-bind pscan 'rback ()
  ; (rback) -> nil
  (defq
    cntrl (callp pscan 'reader)
    pmark (getp cntrl :pre_mark))
  (when pmark
    (setsp! cntrl
      :line (first pmark)
      :column (second pmark)
      :buff_pos (elem 2 pmark))))

(defunp-bind pscan 'skip-line ()
  ; (skip-line) -> next line
  (defq ch (callp pscan 'rnext))
  (until (or (eql ch nil) (eql ch (char 0x0a)))
    (setq ch (callp pscan 'rnext))))

; Reader predicates
(defq ascii-set (const "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
(defq ascii-flow (const ",[]{}"))

(defunp-bind pscan 'is_z (ch)
  (eql ch (ascii-char 0)))

(defunp-bind pscan 'is_break (ch)
  (defq cc (code ch))
  (or (= cc 0x0a) (= cc 0x0d)))

(defunp-bind pscan 'is_breakz (ch)
  (or (callp pscan 'is_z ch)(callp pscan 'is_break ch)))

(defunp-bind pscan 'is_blank (ch)
  (defq cc (code ch))
  (or (= cc 0x20) (= cc 0x09)))

(defunp-bind pscan 'is_blankz (ch)
  (or (callp pscan 'is_blank ch) (callp pscan 'is_z ch)))

(defunp-bind pscan 'is_digit (ch)
  (<= 0x30 (code ch) 0x39))

(defunp-bind pscan 'is_alpha (ch)
  (find ch ascii-set))

(defunp-bind pscan 'is_flow (ch)
  (find ch ascii-flow))

(defunp-bind pscan 'char_is (chi cht)
  (= (code chi) (chode cht)))

; Tokens

(defunp-bind pscan 'base-token ()
  (properties :mark_start nil :mark_end nil))

(defunp-bind pscan 'stream-start-token ()
  (defq ss (callp pscan 'base-token))
  (setp! ss :type :stream-start t))

(defunp-bind pscan 'stream-end-token ()
  (defq se (callp  pscan 'base-token))
  (setp! se :type :stream-end t))

(defunp-bind pscan 'document-start-token ()
  (defq
    ds (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 3 (callp pscan 'rnext)) (callp pscan 'mark)))
  (setsp! ds
    :type :document-start
    :mark_start ms
    :mark_end me))

(defunp-bind pscan 'document-end-token ()
  (defq
    de (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 3 (callp pscan 'rnext)) (callp pscan 'mark)))
  (setsp! de
    :type :document-end
    :mark_start ms
    :mark_end me))

(defunp-bind pscan 'directive-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-sequence-start-token ()
  (defq
    bss (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (callp pscan 'mark))
  (setsp! bss
    :type :block-sequence-start
    :mark_start ms
    :mark_end me))

(defunp-bind pscan 'block-mapping-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-end-token ()
  (defq
    be (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (callp pscan 'mark))
  (setsp!  bss
    :type :block-end
    :mark_start ms
    :mark_end me))

(defunp-bind pscan 'flow-sequence-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-mapping-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-sequence-end-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-mapping-end-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-entry-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-entry-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'key-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'value-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'alias-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'anchor-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'tag-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'scalar-token ()
  (callp pscan 'base-token))

(defun fetch-next-token (rstrm)
  (defq tlist (getp rstrm :tokens))
  (if (getp rstrm :sstart_produced)
    (let (
      (mark (getp rstrn :pre_mark))
      (ch (callp pscan 'rpeek))
      (nc (callp pscan 'rpeek 1)))
      (cond
        ((callp pscan 'is_z ch)
          (push tlist (callp pscan 'stream-end-token))
          :eof)
        ((and
          (= (getp mark :column) 1)
          (callp pscan 'char_is ch "-")
          (callp pscan 'char_is nc "-")
          (callp pscan 'char_is (callp pscan 'rpeek 2) "-"))
          (push tlist (callp pscan 'document-start-token))
          :ok)
        ((and
          (= (getp mark :column) 1)
          (callp pscan 'char_is ch ".")
          (callp pscan 'char_is nc ".")
          (callp pscan 'char_is (callp pscan 'rpeek 2) "."))
          (push tlist (callp pscan 'document-end-token))
          :ok)
        ))
    (progn
      (push tlist (callp pscan 'stream-start-token))
      :ok)))

(defun consume ()
  (defq
    rdr (callp pscan 'reader)
    res (fetch-next-token rdr))
  (while (eql res :ok)
    (setq res (fetch-next-token rdr)))
  (getp rdr :tokens))

(defun scan (buffer)
  ; (scan stream) -> list of tokens
  ; Scans through the istrm and tokenizes
  ; into YAML standard token ids
  (callp pscan 'setup-reader buffer)
  (each (#(print
    (getp %0 :type)
    " "
    (getp %0 :mark_start)
    " "
    (getp %0 :mark_end))) (consume))
  ; (callp pscan 'reader)
  )
