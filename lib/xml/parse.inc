;;;;;;;;;;;;
; xml parser
;;;;;;;;;;;;

;module
(env-push)

(defq +xml_entities (scatter (Fmap 11) "lt" "<" "gt" ">" "amp" "&" "quot" "\q" "apos" "'"))

(defun xml-entities (text)
	; (xml-entities str) -> str
	;the text with each entity what it stands for: &lt; &gt; &amp; &quot;
	;&apos; and &#n; &#xh; as UTF-8. One it does not know is left as it is
	(defq at (find "&" text) out (if at (list)) from 0)
	(while at
		(push out (slice text from at))
		(defq end (find ";" text at) name (if end (slice text (inc at) end)) got :nil)
		(cond
			((not name))
			((setq got (. +xml_entities :find name)))
			((and (> (length name) 2) (eql (first name) "#") (or (eql (second name) "x") (eql (second name) "X")))
				(if (defq n (str-to-num (cat "0x" (slice name 2 -1)))) (setq got (num-to-utf8 n))))
			((and (> (length name) 1) (eql (first name) "#"))
				(if (defq n (str-to-num (slice name 1 -1))) (setq got (num-to-utf8 n)))))
		(cond
			(got (push out got) (setq from (inc end)))
			(:t (push out "&") (setq from (inc at))))
		(setq at (find "&" text from)))
	(if out (apply (const cat) (push out (slice text from -1))) text))

(defun XML-parse (stream fnc_in fnc_out fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)
	;each element to (fnc_in name keys vals) as it opens, its attributes,
	;and to (fnc_out name) as it closes, and each run of text to (fnc_text
	;text), in the order they are in the stream. keys and vals are the
	;parser's own, cleared for the next element, a callback that keeps
	;them keeps a copy. Values and text with their entities made what they
	;stand for, (xml-entities); the tokens are of lib/xml/lisp.vp. An element
	;still open at the end of the stream, a file cut short, is closed there,
	;the last opened first, as near as it can be taken
	(bind '(kinds tokens) ((const (ffi (path-to-absolute "./parse"))) stream))
	(defq e (penv) name "" keys (list) vals (list) open (list))
	(each (lambda (kind token)
			(case kind
				(:in (setq name token) (clear keys vals))
				(:key (push keys token))
				(:val (push vals (xml-entities token)))
				(:open (task-slice) (push open name) (callback fnc_in e name keys vals))
				(:empty (task-slice) (callback fnc_in e name keys vals) (callback fnc_out e name))
				(:out (pop open) (callback fnc_out e token))
				(:text (callback fnc_text e (xml-entities token)))))
		kinds tokens)
	(while (defq left (pop open)) (callback fnc_out e left)))

;module
(export-symbols '(XML-parse xml-entities))
(env-pop)
