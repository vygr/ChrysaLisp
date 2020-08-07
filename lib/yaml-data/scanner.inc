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
     :pre_mark (properties :line 1 :column 0 :buff_pos 0)
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
  (cond
    ((<= pos (length toks))
      (callp pscan 'token-parsed rdr)
      (insert toks pos (list token)))
    (t
      (throw "Invalid index in 'insert-token' " pos))))

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
    (setsp! cntrl :line (inc clin) :column 0))
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
    ; (prin ch)
    (cond
      ((callp pscan 'is_blank ch)
        (callp pscan 'rnext))
      ((and (callp pscan 'is_tab ch) (or (> fl 0) (not ska)))
        (callp pscan 'rnext))
      ((callp pscan 'char_is ch "#")
        (callp pscan 'skip-line))
      ((callp pscan 'is_break ch)
        (callp pscan 'rnext)
        (when (= fl 0)
          (callp pscan 'allow-simple-key)))
      (t (setq loop nil))))
  ; (print)
  t)

; Reader predicates and well know char sequences

(defq ascii-set (const "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
(defq ascii-flow (const ",[]{}"))
(defq doc-start-set (const "---"))
(defq doc-end-set (const "..."))
(defq illegal-set (const (cat "?%*&!" (char 92))))

(defunp-bind pscan 'in-buff-len (n)
  (< (+ (getp rdr :buff_pos) n) (getp rdr :buff_len)))

(defunp-bind pscan 'char-seq-is (rdr chs)
  (defq
    bp  (getp rdr :buff_pos)
    bf  (getp rdr :buffer)
    res t)
  (each (lambda (ch)
    (when (not (eql ch (elem (+ _ bp) bf)))
      (setq res nil))) chs)
  res)

(defunp-bind pscan 'doc-start? (rdr)
  (when (and (callp pscan 'in-buff-len 3)
    (callp pscan 'char-seq-is rdr doc-start-set))
    t))

(defunp-bind pscan 'doc-end? (rdr)
  (when (and (callp pscan 'in-buff-len 3)
    (callp pscan 'char-seq-is rdr doc-end-set))
    t))

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
  (properties
    :possible nil
    :required nil
    :token_number nil
    :mark nil))

(defunp-bind pscan 'mark-simple-key ()
  (setp!
    (callp pscan 'simple-key)
    :mark
    (callp pscan 'mark)))

(defunp-bind pscan 'remove-simple-key (&optional rdr)
  (setd rdr (callp pscan 'reader))
  (when (> (length (getp rdr :simple_keys)) 0)
    (defq lm (last (getp rdr :simple_keys)))
    (when (and (getp lm :possible) (getp lm :required))
      (throw "Simple key expected " (callp pscan 'mark)))
    (setp! lm :possible nil)))

(defunp-bind pscan 'stale-simple-keys (&optional rdr)
  (setd rdr (callp pscan 'reader))
  (defq mk (getp rdr :pre_mark))
  (print "Stale simple key " (getp mk :line))
  (each (lambda (el)
    (print
      "SK possible " (getp el :possible)
      " el line " (getp (getp el :mark) :line))
    (when (and (getp el :possible)
      (or (< (getp (getp el :mark) :line) (getp mk :line))
        (< (+ (getp (getp el :mark) :pos) 1024) (getp mk :pos))))
      (if (getp el :required)
        (throw "simple key expect :" t)
        (setp! el :possible nil)))) (getp rdr :simple_keys)))

(defunp-bind pscan 'save-simple-key (&optional rdr)
  (setd rdr (callp pscan 'reader))
  (defq
    pm  (getp rdr :pre_mark)
    req (and
      (> (getp rdr :flow_level) 0)
      (= (getp rdr :indent) (getp pm :column))))
  (when (getp rdr :simple_key_allowed)
    (defq sk (callp pscan 'mark-simple-key))
    (setsp! sk
      :possible t
      :required req
      :token_number (+ (getp rdr :tokens_parsed) (length (getp rdr :tokens))))
    (callp pscan 'remove-simple-key rdr)
    (pop (getp rdr :simple_keys))
    (push (getp rdr :simple_keys) sk)))


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
          (t
            (callp pscan 'insert-token (- (getp rdr :tokens_parsed) numb) token)))))))

(defunp-bind pscan 'unroll-indent (rdr col)
  (print "unroll col " col " indent " (getp rdr :indent))
  (if (<= (getp rdr :flow_level) 0)
    (while (> (getp rdr :indent) col)
      (print "  unroll condition met " (getp rdr :indents))
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
    ms (callp pscan 'mark)
    me (progn (times 3 (callp pscan 'rnext)) (callp pscan 'mark)))
  (callp pscan 'skip-line)
  (callp pscan 'push-token (setsp! ds
      :type :document-start
      :mark_start ms
      :mark_end me)))

(defunp-bind pscan 'document-end-token ()
  (defq
    de (callp  pscan 'base-token)
    ms (callp pscan 'mark)
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
    ms (callp pscan 'mark))
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
    pm  (callp pscan 'mark))
  (when (= (getp rdr :flow_level) 0)
    (when (not (getp rdr :simple_key_allowed))
      (throw "Block sequence not allowed here" ms))
    (print
      "Block Entry Sequence flow Level " (getp rdr :flow_level)
      " mark " pm)
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

(defunp-bind pscan 'pps1 (props)
  (if (empty? (getp props :trailing_breaks))
    (setp! props :strng (cat (getp props :strng) " "))
    ; Else
    (setsp! props
      :strng (cat
        (getp props :strng)
        (getp props :trailing_breaks))
      :trailing_breaks ""))
  ; Complete
  (setp! props :leading_break ""))

(defunp-bind pscan 'pps0 (props)
  (if (or (getp props :leading_blanks)
    (not (empty? (getp props :white_spaces))))
    (if (getp props :leading_blanks)
      (if (empty? (getp props :leading_break))
        (setsp! props
          :strng (cat
            (getp props :strng)
            (getp props :leading_break)
            (getp props :trailing_breaks))
          :trailing_breaks ""
          :leading_break "")
        ; Else
        (callp pscan 'pps1 props))
      ; Else
      (setsp! props
        :strng (cat (getp props :strng) (getp props :white_spaces))
        :white_spaces ""))))

(defunp-bind pscan 'pre-plain-scalar (rdr props)
  ; (pre-plain-scalar rdr props)
  (defq
    ch          (getp props :ch)
    nc          (getp props :nc)
    inner_loop  t
    lcount      0)
  ; Loop
  (while inner_loop
    (print "lcount = " lcount)
    (setq lcount (inc lcount))
    (cond
      ; Break on blank
      ((callp pscan 'is_blankz ch)
        (print "pps Exit loop, found blank on " props)
        (setq inner_loop nil))
      ; Break on simple key indicator
      ((and (eql ch ":") (callp pscan 'is_blankz nc)
        (or (and (> (getp rdr :flow_level) 0)
          (callp pscan 'is_flow nc))))
        (print "pps Exit loop, found break on :" props)
        (setq inner_loop nil))
      ; Break if flow control
      ((callp pscan 'is_flow ch)
        (when (> (getp rdr :flow_level) 0)
          (print "pps Exit loop, found break on flow" props)
          (setq inner_loop nil)))
      ; Otherwise
      (t
        (print "Calling pps0 with " props)
        (callp pscan 'pps0 props)
        (print "  Returns pps0 with " props) ))
    (when inner_loop
      (setp! props :strng (cat (getp props :strng) ch))
      (callp pscan 'rnext)
      (setq
        ch (callp pscan 'rpeek)
        nc (callp pscan 'rpeek 1))))
    (setsp! props
      :ch ch
      :nc nc))

(defunp-bind pscan 'post-plain-scalar (rdr props)
  ; (post-plain-scalar rdr props)
  (defq
    ch          (getp props :ch)
    nc          (getp props :nc)
    inner_loop  t)

  (while (or (callp pscan 'is_blank ch)(callp pscan 'is_break ch))
    (if (callp pscan 'is_blank ch)
      (cond
        ((and
          (getp props :leading_blanks)
          (< (getp (callp pscan 'mark) :column) (getp props :indent))
          (= (code ch) 0x09))
          (throw "Found tab in plain scaler" props))
        ((getp props :leading_blanks)
          (callp pscan 'rnext))
        (t
          (setp! props :white_spaces (cat (getp props :white_spaces) ch)))
          (callp pscan 'rnext))
        (cond
          ((getp props :leading_blanks)
            (print "Leading blanks break")
            (setp! props :trailing_breaks
              (cat (getp props :trailing_breaks) ch))
            (callp pscan 'rnext))
          (t
            (setsp! props
              :white_spaces ""
              :leading_break (cat (getp props :leading_break) ch)
              :leading_blanks t)
            (callp pscan 'rnext))))
    (setq
      ch (callp pscan 'rpeek)
      nc (callp pscan 'rpeek 1))
    (setsp! props :ch ch :nc nc))
  )

(defunp-bind pscan 'scan-plain-scalar (rdr)
  (defq
    index (inc (getp rdr :indent))
    mark  (callp pscan 'mark)
    props (properties
      :ch   (callp pscan 'rpeek)
      :nc   (callp pscan 'rpeek 1)
      :indent (inc (getp rdr :indent))
      :strng ""
      :leading_break ""
      :trailing_breaks ""
      :white_spaces ""
      :leading_blanks nil)
    ch    (getp props :ch)
    nc    (getp props :nc)
    loop t)

  ; Outer loop
  (while loop
    ; Check for start or end doc if we are at col 1
    (cond
      ; Break on doc start or end
      ((and (= (getp mark :column) 0)
        (or (callp pscan 'doc-start? rdr) (callp pscan 'doc-end? rdr)))
        (print "Break on start/end doc")
        (setq loop nil))
      ; Break on comment
      ((eql ch "#")
        (print "Break on comment")
        (setq loop nil))
      ; Otherwise
      (t
        (print "Calling pre plain scalar with " props)
        (callp pscan 'pre-plain-scalar rdr props)
        (setq
          mark (callp pscan 'mark)
          ch (getp props :ch)
          nc (getp props :nc))))
    ; Back in main loop
    (when loop
      (if (not (or (callp pscan 'is_blank ch) (callp pscan 'is_break ch)))
        (progn
          (print "Killing loop")
          (setq loop nil))
        (progn
          (print "Calling post plain scalar with " props)
          (callp pscan 'post-plain-scalar rdr props)
          (print "  Return from post plan " props)
          (setq
            mark (callp pscan 'mark)
            ch (getp props :ch)
            nc (getp props :nc))
          ))))

  (print "Exiting scan plain with " props)
  (when (getp props :leading_blanks)
    (callp pscan 'allow-simple-key)))

(defun fetch-plain-scalar (rdr)
  (callp pscan 'save-simple-key rdr)
  (callp pscan 'disallow-simple-key)
  (callp pscan 'push-token
    (callp pscan 'scan-plain-scalar rdr)))

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
          (callp pscan 'doc-start? rdr)
          (callp pscan 'is_blankz (callp pscan 'rpeek 3)))
          (print "Document Start")
          (callp pscan 'document-start-token)
          :ok)
        ; Document end
        ((and
          (callp pscan 'doc-end? rdr)
          (callp pscan 'is_blankz (callp pscan 'rpeek 3)))
          (print "Document End")
          (callp pscan 'document-end-token)
          :ok)
        ; Currently unsupported
        ((find ch illegal-set)
          (throw "Control not supported " ch))
        ; Block entry
        ((and
          (callp pscan 'char_is ch "-")
          (callp pscan 'is_blankz nc))
          (print "Block entry")
          (callp pscan 'block-entry-token)
          :ok)
        ((and
          (callp pscan 'char_is ch "-")
          (not (callp pscan 'is_blankz nc)))
          (print "Plain scalar")
          ;(callp pscan 'skip-line)
          (throw "In '-' not blankz" (callp pscan 'mark))
          :eok)
        ; Sequence flows
        ; Mapping flows
        (t
          (print "Plain scalar skipping line")
          (print " ch '" ch "'' nc '" nc "'")
          (callp pscan 'scan-plain-scalar rdr)
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
