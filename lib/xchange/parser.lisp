;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parser - ChrysaLisp YAML Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xtras/xtras.inc")

(defq indent 0)

(defun shr ()
  (setq indent (+ indent 2)))
(defun shl ()
  (setq indent (- indent 2)))

(defun parse-quite (cmd token)
  ; (parse-quite command token) -> cmd
  cmd)

(defun parse-exception-type (cmd token)
  (throw "No idea what this is " token))

(defun parse-no-impl (cmd token)
  (print "No implementation for " (gets token :type))
  cmd)

(defmacro peek-last (col)
  ; (peek-last stack) -> el | nil
  `(if (not (empty? ,col))
    (last ,col)
    nil))

(defun dispatch (cmd)
  (defq lst (pop (gets cmd :rtoks)))
  ; ; (print "dispatching "(gets lst :type))
  ((gets jmp (gets lst :type) parse-exception-type) cmd lst)
  lst)

(defun dispatch-until (cmd &rest kws)
  (while (not (find (gets (dispatch cmd) :type) kws))))

(defun push-container (cmd ctype)
  ; (push-container cmd ctype) -> list
  (shr)
  (defq
    curr      (gets cmd :current)
    container (list ctype (list)))
  (push (gets cmd :parents) container)
  (push curr container)
  (sets! cmd :current (second container))
  container)

(defun pop-container (cmd)
  ; (pop-container cmd) -> list
  (shl)
  (pop (gets cmd :parents))
  (defq curr (peek-last (gets cmd :parents)))
  (sets! cmd :current (second curr))
  curr)

(defun parse-document-start (cmd token)
  (push-container cmd :document)
  (dispatch-until cmd :document_end)
  (pop-container cmd))

(defun parse-document-end (cmd token)
  cmd)

(defq yaml_boolean
      (properties
        t       :true
        nil     :false
        "TRUE"  t
        "FALSE" nil
        "YES"   t
        "NO"    nil))

(defun parse-boolean (val)
  (defq res (gets yaml_boolean val :nomatch))
  (when (eql res :nomatch)
    (setq res (gets yaml_boolean (to-upper val) :nomatch)))
  res)

(defun parse-scalar (cmd token)
  (defq
    v (gets token :value))
  (cond
    ; Test for key
    ((and
       (eql (first (last (gets cmd :parents))) :key)
       (gets (gets cmd :in-args) :keys-to-kw))
      (setq v (sym (str ":" (join (split v " ") "_")))))
    ; Test for numeric
    ((and
       (gets (gets cmd :in-args) :vals-to-num)
       (eql (str-is-ints? v) :true))
     (setq v (str-as-num v)))
    ((and
       (eql (first (last (gets cmd :parents))) :value)
       (gets (gets cmd :in-args) :vals-to-kw)
       (eql (first v) +kw_ind))
     (setq v (kw v)))
    (t
      (if (neql? (defq ch (parse-boolean v)) :nomatch)
        (setq v ch))))
  (push (gets cmd :current) (list :scalar v)))

(defun parse-key (cmd token)
  (defq key (push-container cmd :key))
  (dispatch cmd)
  (pop-container cmd)
  cmd)

(defun parse-value (cmd token)
  (defq key (push-container cmd :value))
  (dispatch cmd)
  (pop-container cmd)
  cmd)

(defun parse-pair (cmd token)
  (push-container cmd :entry)
  (dispatch-until cmd :value_entry)
  (pop-container cmd)
  cmd)

(defun parse-block-entry (cmd token)
  (defq key (push-container cmd :entry))
  (dispatch cmd)
  (pop-container cmd)
  cmd)

(defun parse-flow-entry (cmd token)
  (defq lt (gets (peek-last (gets cmd :rtoks)) :type))
  (cond
    ((eql lt :key_entry)
     (push-container cmd :entry)
     (dispatch-until cmd :value_entry)
     (pop-container cmd))
    ((eql lt :pair)
     (dispatch cmd))
    (t
      (push-container cmd :entry)
      (dispatch cmd)
      (pop-container cmd)))
  cmd)

(defun parse-blockseq-start (cmd token)
  (defq key (push-container cmd :list))
  (dispatch-until cmd :block_end)
  (pop-container cmd)
  cmd)

(defun parse-blockmap-start (cmd token)
  (defq
    key (push-container cmd :properties))
  (dispatch-until cmd :block_end)
  (pop-container cmd)
  cmd)

(defun parse-block-end (cmd token)
  cmd)

(defun parse-flowseq-start (cmd token)
  (defq
    key (push-container cmd :list)
    fe  (eql (gets (peek-last (gets cmd :rtoks)) :type) :scalar))
  (when fe
    (push-container cmd :entry)
    (dispatch cmd)
    (pop-container cmd))
  (dispatch-until cmd :flowseq_end)
  (pop-container cmd)
  cmd)

(defun parse-flowseq-end (cmd token)
  cmd)

(defun parse-flowmap-start (cmd token)
  ; (print (pad "" indent) "flowmap-start ")
  (defq
    key (push-container cmd :properties))
  (dispatch-until cmd :flowmap_end)
  (pop-container cmd)
  cmd)

(defun parse-flowmap-end (cmd token)
  cmd)

(defq jmp
      (properties
        :stream_start   parse-quite
        :stream_end     parse-quite
        :scalar         parse-scalar
        :document_start parse-document-start
        :document_end   parse-document-end
        :blockseq_start parse-blockseq-start
        :blockmap_start parse-blockmap-start
        :block_entry    parse-block-entry
        :block_end      parse-block-end
        :flowseq_start  parse-flowseq-start
        :flowseq_end    parse-flowseq-end
        :flowmap_start  parse-flowmap-start
        :flowmap_end    parse-flowmap-end
        :flow_entry     parse-flow-entry
        :pair           parse-pair
        :key_entry      parse-key
        :value_entry    parse-value))

(defun parse (tokens in-args)
    ; (print "Parsing")
    (shr)
    (defq cmd (properties
                :key_to_kw          t
                :scalar_to_native   nil
                :rtoks              (reverse tokens)
                :tree               (list)
                :parents            (list)
                :in-args            in-args
                :current            nil))
    (sets! cmd :current (gets cmd :tree))
    (when (not (eql (gets (last (gets cmd :rtoks)) :type) :stream_start))
      (throw "Missing :stream_start, found "(peek-last (gets cmd :rtoks))))
    (catch (until (empty? (gets cmd :rtoks))
            (dispatch cmd))
        (print _))
    ; ; (print (gets cmd :tree))
    ; (list)
    (gets cmd :tree)
    )
