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
     :column 0
     :sstart_produced nil
     :ssend_produced nil
     :simple_key_allowed t
     :simple_keys (list)
     :indent -1
     :indents (list)
     :flow_level 0
     :is_error nil
     :tokens_parsed 0
     :tokens (list)))

(defunp-bind pscan 'reader ()
  ; (reader) -> :reader dictionary
  (dict-get pscan :reader))

(defunp-bind pscan 'mark ()
  (defq cntrl (callp pscan 'reader))
  (properties
    :line     (getp cntrl :line)
    :column   (getp cntrl :column)
    :pos      (getp cntrl :buff_pos)))

(defunp-bind pscan 'token-parsed (rdr)
  (setp! rdr :tokens_parsed (inc (getp rdr :tokens_parsed))))

(defunp-bind pscan 'insert-token (pos token)
  (defq
    rdr  (callp pscan 'reader)
    toks (getp rdr :tokens))
  (when (> pos (length toks))
    (throw "Invalid index in 'insert-token' " pos))
  (callp pscan 'token-parsed rdr)
  (insert toks pos (list token)))

(defunp-bind pscan 'push-token (token)
  (defq rdr  (callp pscan 'reader))
  (callp pscan 'token-parsed rdr)
  (push  (getp rdr :tokens) token))

(defunp-bind pscan 'allow-simple-key ()
  (setp! (callp pscan 'reader) :simple_key_allowed t))

(defunp-bind pscan 'disallow-simple-key ()
  (setp! (callp pscan 'reader) :simple_key_allowed nil))

(defunp-bind pscan 'rpeek (&optional pndx)
  ; (rpeek [i]) -> nil | char
  (setd pndx 0)
  (defq
    rdr (callp pscan 'reader)
    buf (getp rdr :buffer)
    bp  (getp rdr :buff_pos))
  (if (< (+ bp pndx) (getp rdr :buff_len))
    (elem (+ bp pndx) (getp rdr :buffer))))

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

(defunp-bind pscan 'skip-line ()
  ; (skip-line) -> next line
  (defq ch (callp pscan 'rnext))
  (until (or (eql ch nil) (eql ch (char 0x0a)))
    (setq ch (callp pscan 'rnext))))

(defunp-bind pscan 'skip-to-next ()
  (defq
    loop t
    fl   (getp (callp pscan 'reader) :flow_level)
    ska  (getp (callp pscan 'reader) :simple_key_allowed))
  (while loop
    (defq ch (callp pscan 'rpeek))
    (prin ch)
    (cond
      ((callp pscan 'is_blank ch)
        (callp pscan 'rnext))
      ((and (callp pscan 'is_tab ch) (or (> fl 0) (not ska)))
        (callp pscan 'rnext))
      ((callp pscan 'char_is ch "#")
        (callp pscan 'skip-line))
      ((callp pscan 'is_break ch)
        (callp pscan 'rnext)
        (when (= fl 0) (callp pscan 'allow-simple-key)))
      (t (setq loop nil))))
  (print)
  t)

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

(defunp-bind pscan 'is_tab (ch)
  (= (code ch) 0x09))

(defunp-bind pscan 'is_blank (ch)
  (= (code ch) 0x20))

(defunp-bind pscan 'is_blankz (ch)
  (or
    (callp pscan 'is_blank ch)
    (callp pscan 'is_breakz ch)))

(defunp-bind pscan 'is_digit (ch)
  (<= 0x30 (code ch) 0x39))

(defunp-bind pscan 'is_alpha (ch)
  (find ch ascii-set))

(defunp-bind pscan 'is_flow (ch)
  (find ch ascii-flow))

(defunp-bind pscan 'char_is (chi cht)
  (= (code chi) (code cht)))

; SimpleKey

(defunp-bind pscan 'simple-key ()
  (properties :possible nil :required nil :token_number -1 :mark nil))

(defunp-bind pscan 'mark-simple-key ()
  (setp! (callp pscan 'simple-key)
    :mark (callp pscan 'mark)))

(defunp-bind pscan 'remove-simple-key ()
  (defq rdr (callp pscan 'reader))
  (when (> (length (getp rdr :simple_keys)) 0)
    (defq lm (last (getp rdr :simple_keys)))
    (when (and (getp lm :possible) (getp lm :required))
      (throw "Simple key expected " (callp pscan 'mark)))
    (setp! lm :possible nil)))

(defunp-bind pscan 'stale-simple-keys (rdr)
  (reduced-reduce (lambda (acc el)
    (when (and
      (getp el :possible) (getp el :mark)
      (or
        (< (getp (getp el :mark) :line) (getp rdr :line))
        (< (+ (getp (getp el :mark) :buff_pos) 1024) (getp rdr :buff_pos))))
      (when (getp el :required)
        (throw "simple key expect :" t)))
    acc)
  (getp rdr :simple_keys) t))

; Token Management
(defunp-bind pscan 'roll-indent (rdr col numb token)
  (print "roll col " col " indent " (getp rdr :indent))
  (if (<= (getp rdr :flow_level) 0)
    (if (< (getp rdr :indent) col)
      (progn
        (push (getp rdr :indents) (getp rdr :indent))
        (setp! rdr :indent col)
        (cond
          ((eql numb nil)
            (callp pscan 'push-token token))
          (t (callp pscan 'insert-token (- (getp rdr :tokens_parsed) numb) token)))))))

(defunp-bind pscan 'unroll-indent (rdr col)
  (print "unroll col " col " indent " (getp rdr :indent))
  (if (<= (getp rdr :flow_level) 0)
    (while (> (getp rdr :indent) col)
      (print "  unroll condition met")
      (callp pscan 'push-token (callp pscan 'block-end-token))
      (setp! rdr :indent (pop (getp rdr :indents))))))

; Tokens

(defunp-bind pscan 'base-token ()
  (properties :mark_start nil :mark_end nil))

(defunp-bind pscan 'stream-start-token ()
  (defq ss (callp pscan 'base-token))
  (callp pscan 'push-token (setp! ss :type :stream-start t)))

(defunp-bind pscan 'stream-end-token ()
  (defq se (callp  pscan 'base-token))
  (callp pscan 'push-token (setp! se :type :stream-end t)))

(defunp-bind pscan 'document-start-token ()
  (defq
    ds (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 3 (callp pscan 'rnext)) (callp pscan 'mark)))
  (callp pscan 'skip-line)
  (callp pscan 'push-token (setsp! ds
      :type :document-start
      :mark_start ms
      :mark_end me)))

(defunp-bind pscan 'document-end-token ()
  (defq
    de (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 3 (callp pscan 'rnext)) (callp pscan 'mark)))
  (callp pscan 'skip-line)
  (callp pscan 'push-token (setsp! de
    :type :document-end
    :mark_start ms
    :mark_end me)))

(defunp-bind pscan 'directive-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-sequence-start-token ()
  (defq
    bss (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark))
  (callp pscan 'rnext)
  (setsp! bss
    :type :block-sequence-start
    :mark_start ms
    :mark_end (callp pscan 'mark)))

(defunp-bind pscan 'block-mapping-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-end-token ()
  (defq
    be (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (callp pscan 'mark))
  (setsp!  be
    :type :block-end
    :mark_start ms
    :mark_end me))

(defunp-bind pscan 'block-entry-token ()
  (defq
    rdr (callp pscan 'reader)
    bet (callp pscan 'base-token)
    pm  (getp rdr :pre_mark))
  (when (= (getp rdr :flow_level) 0)
    (when (not (getp rdr :simple_key_allowed))
      (throw "Block sequence not allowed here" ms))
    (print "Block Entry Sequence flow Level " (getp rdr :flow_level))
    (callp pscan 'roll-indent
      rdr
      (getp pm :column)
      nil
      (callp pscan 'block-sequence-start-token)))
  (callp pscan 'remove-simple-key)
  (callp pscan 'allow-simple-key)
  (defq sm (getp rdr :pre_mark))
  (callp pscan 'rnext)
  (callp pscan 'push-token
    (setsp! bet
      :type :block-entry
      :mark_start sm
      :mark_end (callp pscan 'mark))))

(defunp-bind pscan 'flow-sequence-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-mapping-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-sequence-end-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'flow-mapping-end-token ()
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

(defun fetch-plain-scalar (rdr))

(defun fetch-next-token (rdr)
  (defq tlist (getp rdr :tokens))
  (if (getp rdr :sstart_produced)
    (let (
      (ign  (callp pscan 'skip-to-next))
      (ign0 (callp pscan 'stale-simple-keys rdr))
      (mark (getp rdr :pre_mark))
      (ch (callp pscan 'rpeek))
      (nc (callp pscan 'rpeek 1)))
      (callp pscan 'unroll-indent rdr (getp mark :column))
      ; (print "In fetch next inner let")
      (cond
        ; End of stream
        ((callp pscan 'is_z ch)
          (push tlist (callp pscan 'stream-end-token))
          (setp! rdr :ssend_produced t)
          :eof)
        ; EOL
        ((callp pscan 'is_break ch) (callp pscan 'rnext) :ok)
        ; Document start
        ((and
          (= (getp mark :column) 1)
          (callp pscan 'char_is ch "-")
          (callp pscan 'char_is nc "-")
          (callp pscan 'char_is (callp pscan 'rpeek 2) "-")
          (callp pscan 'is_blankz (callp pscan 'rpeek 3)))
          (print "Document Start")
          (callp pscan 'document-start-token)
          :ok)
        ; Document end
        ((and
          (= (getp mark :column) 1)
          (callp pscan 'char_is ch ".")
          (callp pscan 'char_is nc ".")
          (callp pscan 'char_is (callp pscan 'rpeek 2) ".")
          (callp pscan 'is_blankz (callp pscan 'rpeek 3)))
          (print "Document End")
          (callp pscan 'document-end-token)
          :ok)
        ; Currently unsupported
        ((or
          (callp pscan 'char_is ch "?")
          (callp pscan 'char_is ch "%")
          (callp pscan 'char_is ch "*")
          (callp pscan 'char_is ch "&")
          (callp pscan 'char_is ch "!")
          (callp pscan 'char_is ch (ascii-char 92)))
          (throw "Control not supported " ch))
        ; Block entry
        ((and
          (callp pscan 'char_is ch "-")
          (callp pscan 'is_blankz nc))
          ; (callp pscan 'is_breakz nc))
          (print "Block entry")
          (callp pscan 'block-entry-token)
          :ok)
        ; ((and
        ;   (callp pscan 'char_is ch "-")
        ;   (not (callp pscan 'is_blankz nc)))
        ;   (print "Plain scalar")
        ;   (callp pscan 'skip-line)
        ;   :ok)
        ; Sequence flows
        ; Mapping flows
        (t
          (print "Plain scalar skipping line")
          (callp pscan 'skip-line)
          (print "  next " (callp pscan 'rpeek))
          :eof)))
    (progn
      (callp pscan 'stream-start-token)
      (setp! rdr :sstart_produced t)
      :ok)))

(defun consume ()
  (defq
    rdr (callp pscan 'reader)
    res (fetch-next-token rdr))
  (while (eql res :ok)
    (setq res (fetch-next-token rdr))
    (print "FN res = " res))
  (print "all done")
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
