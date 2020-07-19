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

(defunp-bind pscan 'reader ()
  ; (reader) -> :reader dictionary
  (dict-get pscan :reader))

(defunp-bind pscan 'setup-reader (buffer)
  ; (setup-reader buffer) -> :reader dictionary
  (def pscan :reader (properties))
  (defq tmp (dict-get pscan :reader))
  (setp! tmp :buffer buffer t)
  (setp! tmp :buff_len (length buffer) t)
  (setp! tmp :pre_mark nil t)
  (setp! tmp :buff_pos 0 t)
  (setp! tmp :line 1 t)
  (setp! tmp :column 1 t)
  (setp! tmp :tokens (list) t)
  tmp)

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

(defunp-bind pscan 'premark-col-first? ()
  (= (getp (getp (callp pscan 'reader) :pre_mark) :column) 1))

(defunp-bind pscan 'skip-line ()
  ; (skip-line) -> next line
  (defq ch (callp pscan 'rnext))
  (until (or (eql ch nil) (eql ch (char 0x0a)))
    (setq ch (callp pscan 'rnext))))

; Tokens

(defunp-bind pscan 'base-token ()
  (defq bt (dictionary))
  (dict-puts! bt :mark_start nil :mark_end nil)
  bt)

(defunp-bind pscan 'stream-start-token ()
  (defq ss (callp pscan 'base-token))
  (dict-puts! ss :type :stream-start)
  ss)

(defunp-bind pscan 'stream-end-token ()
  (defq se (callp  pscan 'base-token))
  (dict-puts! se :type :stream-end)
  se)

(defunp-bind pscan 'document-start-token ()
  (defq
    ds (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 2 (callp pscan 'rnext)) (callp pscan 'mark)))
  (dict-puts! ds
    :type :document-start
    :mark_start ms
    :mark_end me)
  ds)

(defunp-bind pscan 'document-end-token ()
  (defq
    de (callp  pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (progn (times 2 (callp pscan 'rnext)) (callp pscan 'mark)))
  (dict-puts! de
    :type :document-end
    :mark_start ms
    :mark_end me)
  de)

(defunp-bind pscan 'directive-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-sequence-start-token ()
  (defq
    bss (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (callp pscan 'mark))
  (dict-puts! bss
    :type :block-sequence-start
    :mark_start ms
    :mark_end me)
  bss)

(defunp-bind pscan 'block-mapping-start-token ()
  (callp pscan 'base-token))

(defunp-bind pscan 'block-end-token ()
  (defq
    be (callp pscan 'base-token)
    ms (getp (callp pscan 'reader) :pre_mark)
    me (callp pscan 'mark))
  (dict-puts! bss
    :type :block-end
    :mark_start ms
    :mark_end me)
  be)

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

(defun consume (&optional context)
  (defq
    ch    t
    res   (list)
    indoc nil
    instr nil
    ind   -1
    ctx   (list))
  (push res (callp pscan 'stream-start-token))
  (while ch
    (setq ch (callp pscan 'rnext))
    (cond
      ((eql ch "#") (callp pscan 'skip-line))
      ((eql ch (char 0x0a)))
      ((eql ch " ")
        (print "is space first col? " (callp pscan 'premark-col-first?)))
      ((eql ch "-") ; Either a document start or a sequence item
        (defq pk (callp pscan 'rpeek))
        (cond
          ((and (eql pk "-") (eql (callp pscan 'rpeek 1) "-") (not indoc))
            (setq indoc t)
            (push res (callp pscan 'document-start-token)))
          ((or (eql pk  " ") (eql pk (char 0x0a)))
            (push res (callp pscan 'block-sequence-start-token)))
          (t (throw "Problem at " (callp pscan 'mark)))))
      ((eql ch ".") ; Either a document end or a key/value char
        (defq pk (callp pscan 'rpeek))
        (cond
          ((and (eql pk ".") (eql (callp pscan 'rpeek 1) "."))
            (push res (callp pscan 'document-end-token)))
          (t t)))
      ((eql ch "&") (throw "Anchors not supported " (callp pscan 'mark)))
      ((eql ch "%") (throw "Directives not supported " (callp pscan 'mark)))
      ((eql ch ":") (print "key declared"))
      ((eql ch "[") (print "flow seq start"))
      ((eql ch "]") (print "flow seq end"))
      ((eql ch "{") (print "flow dict start"))
      ((eql ch "}") (print "flow dict end"))
      ; Need to handle the arbitraries
      (t nil)))
  (push res (callp pscan 'stream-end-token)))

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
