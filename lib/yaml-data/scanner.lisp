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

(defun check-plain (scn))

(defun check-value (scn))

(defun check-key (scn))

; Simple key functions

(defun save-possible-simple-key (scn rdr)
  (when (getp scn :allow_simple_key)
    (remove-possible-simple-key scn)
    (setp! (getp scn :possible_simple_keys)
           (getp scn :flow_level)
           (SimpleKey
             (+ (getp scn :tokens_taken) (length (getp scn :tokens)))
             (and (= (getp scn :flow_level)) (= (getp scn :indent) (getp rdr :column)))
             (getp rdr :index)
             (getp rdr :line)
             (getp rdr :column)
             (rdr-get-mark rdr)))))

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
        (print "spsk key " k " value " v)
        (if (or
            (/= (getp v :line) ln)
              (> (- (getp rdr :index) (getp v :index)) 1024))
          (if (getp v :required)
              (throw "Key required" v))
          (pdrop! p k)))
      ke)))

(defun remove-possible-simple-key (scn)
  (defq
    p (getp scn :possible_simple_keys)
    k (getp scn :flow_level)
    v (getp p k))
  (when v
    (if (getp v :required)
        (throw "Key required" v))
    (pdrop! p k)))

; Indent functions

(defun unwind-indent (scn col rdr)
  (when (> (getp scn :flow_level) 0)
    (while (> (getp scn :indent) col)
      (defq mark (rdr-get-mark rdr))
      (setp! scn :indent (pop (getp scn :indents)))
      (push-token scn (BlockEnd mark)))))

(defun add-indent (scn rdr)
  (defq
    cl (getp rdr :column)
    id (getp scn :indent))
  ; (print "add-indent column = " cl " indent " id)
  (if (< id cl)
      (progn
        (push (getp scn :indents) id)
        (setp! scn :indent cl)
        t)
      nil))

; Scanner helpers

(defq
  unsupported "%*&!|>"
  docstart "---"
  docend "..."
  tmperr "{}[],:"
  keyind "?:"
  ebreakz (const (cat "" cr lf tab blank eof))
  notplain (const (cat "" "-?:,[]{}#&*!|>%@`" squote dquote ebreakz)))

(defun scan-peek (scn))

(defun scan-line-break (rdr ch)
  (defq res nil)
  (when (find ch crlf)
    (if (eql (rdr-prefix rdr 2) crlf)
        (rdr-forward rdr 2)
        (rdr-forward rdr))
    (setq res lf))
  res)

(defun scan-next-token (scn rdr)
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

(defun scan-plain-inner (scn ch nc)
  (defq
    ifl     (> (getp scn :flow_level) 0)
    sbreakz (cat "" ebreakz (if ifl ",[]{}" "")))
  (or
    (find ch ebreakz)
    (and (eql ch ":") (find nc sbreakz))
    (and ifl (find ch ",?[]{}"))))

(defun scan-plain-dsorde (rdr prfx)
  (and
    (or
      (eql prfx docstart)
      (eql prfx docend))
    (find (rdr-peek rdr 3) ebreakz)))

; def scan_plain_spaces(self, indent, start_mark):
;         # See the specification for details.
;         # The specification is really confusing about tabs in plain scalars.
;         # We just forbid them completely. Do not use tabs in YAML!
;         chunks = []
;         length = 0
;         while self.peek(length) in ' ':
;             length += 1
;         whitespaces = self.prefix(length)
;         self.forward(length)
;         ch = self.peek()
;         if ch in '\r\n\x85\u2028\u2029':
;             line_break = self.scan_line_break()
;             self.allow_simple_key = True
;             prefix = self.prefix(3)
;             if (prefix == '---' or prefix == '...')   \
;                     and self.peek(3) in '\0 \t\r\n\x85\u2028\u2029':
;                 return
;             breaks = []
;             while self.peek() in ' \r\n\x85\u2028\u2029':
;                 if self.peek() == ' ':
;                     self.forward()
;                 else:
;                     breaks.append(self.scan_line_break())
;                     prefix = self.prefix(3)
;                     if (prefix == '---' or prefix == '...')   \
;                             and self.peek(3) in '\0 \t\r\n\x85\u2028\u2029':
;                         return
;             if line_break != '\n':
;                 chunks.append(line_break)
;             elif not breaks:
;                 chunks.append(' ')
;             chunks.extend(breaks)
;         elif whitespaces:
;             chunks.append(whitespaces)
;         return chunks

(defun scan-plain-spaces (scn rdr)
  (defq
    chunks  (list)
    plen    0
    ch      nil
    lb      nil
    prfx    nil
    res     nil)

  (while (eql (rdr-peek rdr plen) blank)
    (setq plen (inc plen)))

  (defq wsp (rdr-prefix rdr plen))
  (rdr-forward rdr plen)
  (setq ch (rdr-peek rdr))
  (cond
    ((find ch ebreakz)
     (setp! scn :allow_simple_key t)
     (setq
       lb   (scan-line-break rdr ch)
       prfx (rdr-prefix rdr 3))
     (if (scan-plain-dsorde rdr prfx)
         nil
         (progn
           (setq ch (rdr-peek rdr))
           (defq
             breaks (list)
             iloop  (find ch ebreakz))
           (while iloop
              (cond
                ((eql ch blank)
                 (rdr-forward rdr))
                (t
                  (push breaks (scan-line-break rdr ch))
                  (setq prfx (rdr-prefix rdr 3))
                  (if (scan-plain-dsorde rdr prfx)
                      (setq iloop nil))))
              (when iloop
                (if lb
                    (push chunks lb)
                    (if (empty? breaks)
                        (push chunks blank)))
                (setq chunks (cat chunks breaks)))))))
      ((not (empty? wsp))
       (push chunks wsp)))
  )


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
    (if (eql (rdr-peek rdr) comment)
        (setq oloop nil)
        (while iloop
          (defq
            ch (rdr-peek rdr plen)
            nc (rdr-peek rdr (inc plen)))
          (if (scan-plain-inner scn ch nc)
              (setq iloop nil)
              (setq plen (inc plen)))))
    (if (= plen 0)
        (setq oloop nil)
        (progn
          (setp! scn :allow_simple_key nil)
          (setq chunks (cat chunks spaces))
          (push chunks (rdr-prefix rdr plen))
          (rdr-forward rdr plen)
          (setq em (rdr-get-mark rdr))

          ))

    )
  )

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
  (print "check-plain " ch " followed by " nc " res " res)
  res)

; Token generators

(defun fetch-document-indicator (scn rdr token)
  (unwind-indent scn -1 rdr)
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
  (push-token scn (scan-plain scn rdr))
  :ok)

; Main dispatch

(defun fetch-next (scn)
  (defq rdr (getp scn :rdr))
  ; Eat up the white spaces
  (scan-next-token scn rdr)
  ; Drop obsoleted simple keys
  (stale-possible-simple-keys scn rdr)
  ; Compare indentation and current column
  (unwind-indent scn (getp rdr :column) rdr)
  ; Peek next char
  (defq ch (rdr-peek rdr))
  (print "peek-char " ch)
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
     )
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
     (print "-> tokens ")
     (each (lambda (p)
        (print "T-> " (getp p :type)))(getp scn :tokens))
     (print "I-> " (getp scn :indent) " " (getp scn :indents))
     (throw (second res) (last res)))
    ; End stream
    ((eql res :eof)
     (print "Last result " res)
     (push-token scn (StreamEnd))))
  (getp scn :tokens))

(defun-bind scan (strm)
  (print "scan")
  (consume-tokens (Scanner strm)))

