;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scanner - ChrysaLisp YAML Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/tokens.lisp)
(import 'lib/yaml-data/reader.lisp)

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

(defun SimpleKey (tnum req indx line col mark)
  (properties
    :clz            :clz_simple_key
    :token_number   tnum
    :required       req
    :index          indx
    :line           line
    :column         col
    :mark           mark))

(defun Scanner (strm)
  (properties
    :clz                  :clz_scanner
    :rdr                  (Reader strm)
    :done                 nil
    :flow_level           0
    :tokens_taken         0
    :tokens               (list)
    :indent               -1
    :indents              (list)
    :allow_simple_key     t
    :possible_simple_keys (properties)))

(defun push-token (scn token)
  (push (getp scn :tokens) token))
(defun last-token (scn token)
  (last (getp scn :tokens)))
(defun first-token (scn token)
  (last (getp scn :tokens)))

; Simple key functions

(defun remove-possible-simple-key (scn)
  (defq
    p (getp scn :possible_simple_keys)
    k (getp scn :flow_level)
    v (getp p k))
  (when v
    (if (getp v :required)
        (throw "Key required" v))
    (pdrop! p k)))

(defun save-possible-simple-key (scn rdr)
  (defq fl (getp scn :flow_level))
  (when (getp scn :allow_simple_key)
    (remove-possible-simple-key scn)
    (defq
      sk  (SimpleKey
            (+ (getp scn :tokens_taken) (length (getp scn :tokens)))
            (and
              (= fl 0)
              (= (getp scn :indent) (getp rdr :column)))
            (getp rdr :index)
            (getp rdr :line)
            (getp rdr :column)
            (rdr-get-mark rdr)))
    (setp! (getp scn :possible_simple_keys) fl sk t)))

(defun next-possible-simple-key (scn)
  (throw "Need impl next-possible-simple-key"))

(defun stale-possible-simple-keys (scn rdr)
  (defq
    p  (getp scn :possible_simple_keys)
    ke (prop-entries p)
    ln (getp rdr :line))
  (when ke
    (each
      (lambda (_)
        (defq k (first _) v (second _))
        (if (or
            (/= (getp v :line) ln)
              (> (- (getp rdr :index) (getp v :index)) 1024))
          (if (getp v :required)
              (throw "Key required" v))
          (pdrop! p k)))
      ke)))

; Indent functions

(defun unwind-indent (scn rdr col)
  (cond
    ((> (getp scn :flow_level) 0)
     nil)
    (t
      (print "unwind-indent indent " (getp scn :indent) " and col " col)
      (while (> (getp scn :indent) col)
        (defq mark (rdr-get-mark rdr))
        (setp! scn :indent (pop (getp scn :indents)))
        (print "  pushing blockend " mark)
        (push-token scn (BlockEnd mark))))))

(defun add-indent (scn rdr)
  (defq
    cl (getp rdr :column)
    id (getp scn :indent))
  (print "add-indent  indent " id " column = " cl)
  (if (< id cl)
      (progn
        (push (getp scn :indents) id)
        (setp! scn :indent cl)
        t)
      nil))

; Scanner helpers

; Character sets
(defq
  unsupported "%*&!|>"
  docstart "---"
  docend "..."
  tmperr "{}[],:"
  keyind "?:"
  cbreakz (const (cat "" blank cr lf))
  ebreakz (const (cat "" cr lf tab blank eof)))
(defq
  notplain (const (cat "" "-?:,[]{}#&*!|>%@`" squote dquote ebreakz)))

(defun scan-line-break (rdr ch)
  ; (scan-line-break reader ch) -> lf | nil
  ; Determines if we are at line break
  (defq res nil)
  (when (find ch crlf)
    (if (eql (rdr-prefix rdr 2) crlf)
        (rdr-forward rdr 2)
        (rdr-forward rdr))
    (setq res lf))
  res)

(defun scan-next-token (scn rdr)
  ; (scan-next-token scanner reader) -> nil
  ; Eats whitespaces, comments and line breaks
  (defq found nil)
  (while (not found)
    (defq nc (rdr-peek rdr))
    (cond
      ((eql nc blank)
        (rdr-forward rdr))
      ((eql nc comment)
       (while (not (find (rdr-peek rdr) breakz))
        (rdr-forward rdr)))
      ((scan-line-break rdr nc)
       (when (= (getp scn :flow_level) 0)
         (setp! scn :allow_simple_key t)))
      (t (setq found t)))))

(defun scan-plain-dsorde? (rdr prfx)
  ; (scan-plain-dsorde? reader prefix) -> t | nil
  ; Predicate for docstart, end or extended breaks
  (and
    (or
      (eql prfx docstart)
      (eql prfx docend))
    (find (rdr-peek rdr 3) ebreakz)))

(defun scan-spaces (rdr)
  ; (scan-spaces reader) -> str
  ; Batches up repeating spaces
  (defq
    plen 0
    wsp  nil)
  (while (eql (rdr-peek rdr plen) blank)
    (setq plen (inc plen)))
  (setq wsp (rdr-prefix rdr plen))
  (rdr-forward rdr plen)
  wsp)

(defun scan-plain-spaces (scn rdr)
  ; (scan-plain-spaces scanner reader) -> str
  (defq
    chunks  (list)
    ch      (rdr-peek rdr)
    lb      nil
    prfx    nil
    res     nil
    wsp     (scan-spaces rdr))
  (cond
    ((find ch ebreakz)
     (setq
       lb   (scan-line-break rdr ch)
       prfx (rdr-prefix rdr 3))
     (setp! scn :allow_simple_key t)
     (if (scan-plain-dsorde? rdr prfx)
         nil
         (progn
           (setq ch (rdr-peek rdr))
           (defq
             breaks (list)
             iloop  (find ch ebreakz))
           (while iloop
              ; (print "sps char " ch)
              (cond
                ((eql ch blank)
                 (rdr-forward rdr)
                 (setq ch (rdr-peek rdr)))
                (t
                  (push breaks (scan-line-break rdr ch))
                  (setq prfx (rdr-prefix rdr 3))
                  (if (scan-plain-dsorde? rdr prfx)
                      (setq iloop nil))))
              (when iloop
                ; (print "  in iloop " lb " " chunks " " breaks)
                (if lb
                    (push chunks lb)
                    (if (empty? breaks)
                        (push chunks blank)))
                (setq chunks (cat chunks breaks))
                (setq iloop nil))))))
      ((not (empty? wsp))
       (push chunks wsp)))
  chunks)

(defun scan-plain-inner? (scn ch nc)
  (defq
    ifl     (> (getp scn :flow_level) 0)
    sbreakz (cat "" ebreakz (if ifl ",[]{}" "")))
  (or
    (find ch ebreakz)
    (and (eql ch ":") (find nc sbreakz))
    (and ifl (find ch ",?[]{}"))))

(defun scan-plain-sres? (scn rdr spcs)
  (or
    (not (empty? spcs))
    (eql (rdr-peek rdr) comment)
    (and
      (> (getp scn :flow_level) 0)
      (< (getp rdr :column) (getp scn :indent)))))

(defun scan-plain (scn rdr)
  (defq
    chunks  (list)
    sm      (rdr-get-mark rdr)
    em      sm
    ind     (inc (getp scn :indent))
    spaces  (list)
    oloop   t
    iloop   t)
  (while oloop
    (defq plen 0)
    ; If '#'
    (if (eql (rdr-peek rdr) comment)
        (setq oloop nil)
        (while iloop
          (defq
            ch (rdr-peek rdr plen)
            nc (rdr-peek rdr (inc plen)))
          (if (scan-plain-inner? scn ch nc)
              (setq iloop nil)
              (setq plen (inc plen)))))
    (when oloop
      (cond
        ((= plen 0)
         (setq oloop nil))
        (t
          (setp! scn :allow_simple_key nil)
          (setq chunks (cat chunks spaces))
          (push chunks (rdr-prefix rdr plen))
          (rdr-forward rdr plen)
          (setq
            em (rdr-get-mark rdr)
            spaces (scan-plain-spaces scn rdr))
          (if (scan-plain-sres? scn rdr spaces)
            (setq oloop nil))))))
  (Scalar (join chunks "") t sm em))

; Token type verifications
(defun check-document-indicator (rdr dset)
  (defq p3 (rdr-peek rdr 3))
  (cond
    ((= (getp rdr :column) 0)
     (cond
       ((and
          (eql (rdr-prefix rdr 3) dset)
          (find p3 ebreakz))
        t)
       (t nil)))
    (t nil)))

(defun check-document-start (rdr)
  (check-document-indicator rdr docstart))
(defun check-document-end (rdr)
  (check-document-indicator rdr docend))
(defun check-block-entry (rdr)
  (find (rdr-peek rdr 1) ebreakz))
(defun check-plain (scn rdr)
  (defq
    ch (rdr-peek rdr)
    nc (rdr-peek rdr 1)
    res (or (not (find ch notplain))
      (and (not (find nc ebreakz))
           (or (eql ch "-")
               (and
                 (= (getp scn :flow_level) 0)
                 (find ch keyind))))))
  ; (print "check-plain " ch " followed by " nc " res " res)
  res)

; Token generators

(defun fetch-document-indicator (scn rdr token)
  (unwind-indent scn rdr -1)
  (remove-possible-simple-key scn)
  (setp! scn :allow_simple_key nil)
  (defq sm (rdr-get-mark rdr))
  (rdr-forward rdr 3)
  (setsp! token
      :start_mark sm
      :end_mark (rdr-get-mark rdr))
  (push-token scn token)
  :ok)

(defun fetch-document-start (scn rdr)
  (fetch-document-indicator scn rdr (DocumentStart)))

(defun fetch-document-end (scn rdr)
  (fetch-document-indicator scn rdr (DocumentEnd)))

(defun fetch-block-entry (scn rdr)
  (when (= (getp scn :flow_level) 0)
    (when (not (getp scn :allow_simple_key))
        (throw "Sequence entries not allowed here " (rdr-get-mark rdr)))
    (when (add-indent scn rdr)
        (push-token scn (BlockSequenceStart (rdr-get-mark rdr)))))
  (setp! scn :allow_simple_key t)
  (remove-possible-simple-key scn)
  (defq sm (rdr-get-mark rdr))
  (rdr-forward rdr 1)
  (push-token scn (BlockEntry sm (rdr-get-mark rdr)))
  :ok)

 (defun fetch-plain (scn rdr)
  (save-possible-simple-key scn rdr)
  (setp! scn :allow_simple_key nil)
  (defq res (scan-plain scn rdr))
  (push-token scn res)
  :ok)

; Main dispatch

(defun fetch-next (scn)
  (defq rdr (getp scn :rdr))
  ; Eat up the white spaces
  (scan-next-token scn rdr)
  ; Drop obsoleted simple keys
  (stale-possible-simple-keys scn rdr)
  ; Compare indentation and current column
  (unwind-indent scn rdr (getp rdr :column))
  ; Peek next char
  (defq ch (rdr-peek rdr))
  (cond
    ; End of file
    ((eql ch (ascii-char 0)) :eof)
    ; Unsupported controls at the moment
    ((find ch unsupported)
     (list :exception (str ch "Unsupported char ") (rdr-get-mark rdr)))
    ; Common likely
    ((and (eql ch "-") (check-block-entry rdr))
     (fetch-block-entry scn rdr))
    ; Flow controls
    ((find ch tmperr)
     (list :exception (str ch " pending implementation ") (rdr-get-mark rdr)))
    ; Document start
    ((and (eql ch "-") (check-document-start rdr))
     (print "Docstart")
     (fetch-document-start scn rdr))
    ; Document end
    ((and (eql ch ".") (check-document-end rdr))
     (print "Docend")
     (fetch-document-end scn rdr))
    ((check-plain scn rdr)
     (print "Have plain")
     (fetch-plain scn rdr))
    (t
      (list :exception "Not implemented " (rdr-get-mark rdr)))))

(defun consume-tokens (scn)
  (print "Consume-tokens")
  ; Start stream
  (push-token scn (StreamStart))
  (defq res (fetch-next scn))
  (while (eql res :ok)
    (setq res (fetch-next scn)))
  (cond
    ; Exception
    ((lst? res)
     (print "I-> " (getp scn :indent) " " (getp scn :indents))
     (throw (second res) (last res)))
    ; End stream
    ((eql res :eof)
     (print "Last result " res)
     (push-token scn (StreamEnd))))
     (each (lambda (p)
        (print "T-> " (getp p :type)))(getp scn :tokens))
  (getp scn :tokens))

(defun-bind scan (strm)
  (consume-tokens (Scanner strm)))

