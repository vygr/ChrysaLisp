;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parser - ChrysaLisp YAML Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'lib/xtras/xtras.inc)

(defun parse-quite (cmd token)
  ; (parse-quite command token) -> cmd
  cmd)

(defun parse-exception-type (cmd token)
  (throw "No idea what this is " token))

(defun parse-no-impl (cmd token)
  (print "No implementation for " (getp token :type))
  cmd)

(defmacro peek-last (col)
  ; (peek-last stack) -> el|nil
  `(if (not (empty? ,col))
    (last ,col)
    nil))

(defun dispatch (cmd)
  (defq lst (pop (getp cmd :rtoks)))
  ((getp jmp (getp lst :type) parse-exception-type) cmd lst)
  lst)

(defun dispatch-until (cmd &rest kws)
  (while (not (find (getp (dispatch cmd) :type) kws))))

(defun push-container (cmd ctype)
  ; (push-container cmd ctype) -> list
  (defq
    curr      (getp cmd :current)
    container (list ctype (list)))
  (push (getp cmd :parents) container)
  (push curr container)
  (setp! cmd :current (second container))
  container)

(defun pop-container (cmd)
  ; (pop-container cmd) -> list
  (pop (getp cmd :parents))
  (defq curr (peek-last (getp cmd :parents)))
  (setp! cmd :current (second curr))
  curr)

(defun parse-document-start (cmd token)
  (push-container cmd :document)
  (dispatch-until cmd :document_end)
  (pop-container cmd))

(defun parse-document-end (cmd token)
  cmd)

(defun parse-scalar (cmd token)
  (push
    (getp cmd :current)
    (list :scalar (getp token :value))))

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

(defun parse-block-entry (cmd token)
  (defq key (push-container cmd :entry))
  (dispatch cmd)
  (pop-container cmd)
  cmd)

(defun parse-flow-entry (cmd token)
  (defq
    key (push-container cmd :entry)
    fe  (eql (getp (peek-last (getp cmd :rtoks)) :type) :key_entry))
  (if fe
      (dispatch-until cmd :value_entry)
      (dispatch cmd))
  (pop-container cmd)
  cmd)

(defun parse-blockseq-start (cmd token)
  (defq key (push-container cmd :list))
  (dispatch-until cmd :block_end)
  (pop-container cmd)
  cmd)

(defun parse-blockmap-start (cmd token)
  (defq key (push-container cmd :properties ))
  (dispatch-until cmd :block_end)
  (pop-container cmd)
  cmd)

(defun parse-block-end (cmd token)
  cmd)

(defun flow-start (cmd ctype)
  (defq
    key (push-container cmd ctype)
    fe  (eql (getp (peek-last (getp cmd :rtoks)) :type) :flow_entry))
  (when (not fe)
    (push-container cmd :entry))
  (dispatch-until cmd :flowseq_end)
  (when (not fe)
    (pop-container cmd))
  (pop-container cmd)
  cmd)

(defun parse-flowseq-start (cmd token)
  (defq
    key (push-container cmd :list)
    fe  (eql (getp (peek-last (getp cmd :rtoks)) :type) :scalar))
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
  (defq
    key (push-container cmd :properties)
    fe  (eql (getp (peek-last (getp cmd :rtoks)) :type) :key_entry))
  (when fe
    (push-container cmd :entry)
    (dispatch-until cmd :value_entry)
    (pop-container cmd))
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
        :key_entry      parse-key
        :value_entry    parse-value))

(defun-bind parse (tokens options)
    (defq cmd (properties
                :key_to_kw          t
                :scalar_to_native   nil
                :rtoks              (reverse tokens)
                :tree               (list)
                :parents            (list)
                :current            nil))
    (setp! cmd :current (getp cmd :tree))
    (when (not (eql (getp (last (getp cmd :rtoks)) :type) :stream_start))
      (throw "Missing :stream_start, found "(peek-last (getp cmd :rtoks))))
    (catch (until (empty? (getp cmd :rtoks))
            (dispatch cmd))
        (print _))
    (getp cmd :tree))
