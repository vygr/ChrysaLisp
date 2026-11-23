(import "lib/xml/parse.inc")

(report-header "XML: a document parsed into its elements, and the native given what is not a stream")

;what each callback is given, in the order they are called, of a
;document as an SVG file is written. The keys and vals given are the
;parser's own and cleared at the next element, so copies are kept
(defq xm_said (list))
(XML-parse (string-stream "<a x=\q1\q y=\q2\q>\n<b />\nhi\n</a>\n")
	(lambda (name keys vals) (push xm_said (list :in name (cat keys) (cat vals))))
	(lambda (name) (push xm_said (list :out name)))
	(lambda (text) (push xm_said (list :text text))))
(assert-eq "the elements in with their attributes, their text, and out, in order"
	"((:in \qa\q (\qx\q \qy\q) (\q1\q \q2\q)) (:in \qb\q () ()) (:out \qb\q) (:text \qhi\q) (:out \qa\q))"
	(str xm_said))

;the native that breaks the stream into tokens, lib/xml/lisp.vp, given a
;number: an error, and the node goes on. Its error path was code after the
;names of its states, 3 past a place of 4, and an arm64 faulted on it,
;Illegal instruction, 2026-11-23
(ffi "lib/xml/parse" xm-tokens)
(assert-true "the tokenizer given a number is an error, and this node goes on"
	(catch (progn (xm-tokens 42) :nil) :t))

(undef (env) 'xm_said)
