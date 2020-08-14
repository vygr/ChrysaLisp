;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scanner - ChrysaLisp YAML Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)
(import 'lib/yaml-data/tokens.inc)
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

(SimpleKey (tnum, req, indx, line, col, mark)
  (properties
    :token_number   tnum
    :required       req
    :index          indx
    :line           line
    :column         col
    :mark           mark))

(Scanner (strm)
  (properties
    :rdr                  (Reader strm)
    :done                 nil
    :flow_level           0
    :tokens_taken         0
    :tokens               (list)
    :indent               -1
    :indents              (list)
    :allow_simple_key     t
    :possible_simple_keys (properties)))

(scan-to-next-token (scn))
(scan-peek (scn))
(check-plain (scn))
(check-value (scn))
(check-key (scn))
(next-possible-simple-key (scn))
(stale-possible-simple-keys (scn))
(save-possible-simple-key (scn))
(remove-possible-simple-key (scn))
(unwind-indent (scn col))
(add-indent (scn col))

(defun-bind scn-dump (fname mfunc)
  (defq rdr (Scanner (load fname)))
  ; (print "Dumping " fname)
  ; (catch (until (eql (defq ch (rdr-peek rdr)) (ascii-char 0))
  ;   (mfunc ch)
  ;   (rdr-forward rdr)
  ;   (setq ch (rdr-peek rdr)))
  ;   (print "Houston.... problem " _))
  (print))

