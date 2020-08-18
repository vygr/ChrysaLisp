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
(defun next-possible-simple-key (scn))

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
              (throw "Key required"))
          (pdrop! p k)))
      ke)))

(defun save-possible-simple-key (scn))
(defun remove-possible-simple-key (scn))


(defun unwind-indent (scn col rdr)
  (when (> (getp scn :flow_level) 0)
    (while (> (getp scn :indent) col)
      (defq mark (rdr-get-mark rdr))
      (setp! scn :indent (pop (getp scn :indents)))
      (push-token scn (BlockEnd mark)))))

(defun add-indent (scn col))

(defun scan-peek (scn))

(defun scan-line-break (rdr ch)
  (defq res nil)
  (when (find ch crlf)
    (if (eql (rdr-prefix rdr 2) crlf)
        (rdr-forward rdr 2)
        (rdr-forward rdr))
    (setq res t))
  res)

(defun scan-next-token (scn rdr)
  (defq found nil)
  (while (not found)
    (defq nc (rdr-peek rdr))
    (cond
      ((eql nc blank)
        (print "  Blank")
        (rdr-forward rdr))
      ((eql nc comment)
       (while (not (find (rdr-peek rdr) breakz))
        (rdr-forward rdr)))
      ((scan-line-break rdr nc)
       (when (= (getp scn :flow_level) 0)
         (setp! scn :allow_simple_key t)))
      (t (setq found t)))))

(defq
  unsupported "%*&!|>"
  docstart "---"
  docend "..."
  ebreakz (const (cat "" cr lf tab blank eof)))

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
    ((find ch unsupported) (list :exception "Unsupported char " ch))
    ; Common likely
    ; Document start
    ((and (eql ch "-") (check-document-start rdr))
     (print "Docstart")
     (fetch-document-start scn rdr))
    ; Document end
    ((and (eql ch ".") (check-document-end rdr))
     (print "Docend")
     (fetch-document-end scn rdr))
    ; ()
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
     (print "Exception -> tokens " (getp scn :tokens))
     (throw (second res) (last res)))
    ; End stream
    ((eql res :eof)
     (print "Last result " res)
     (push-token scn (StreamEnd))))
  (getp scn :tokens))

(defun-bind scan (strm)
  (print "scan")
  (consume-tokens (Scanner strm)))

