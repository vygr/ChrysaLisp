(import "lib/xml/parse.inc")

(report-header "XML: a document parsed into its elements, and the native given what is not a stream")

(defun xm-said (doc)
	; (xm-said doc) -> str
	;what each callback is given, in the order they are called. The keys
	;and vals given are the parser's own and cleared at the next element,
	;so copies are kept
	(defq said (list))
	(XML-parse (string-stream doc)
		(lambda (name keys vals) (push said (list :in name (cat keys) (cat vals))))
		(lambda (name) (push said (list :out name)))
		(lambda (text) (push said (list :text text))))
	(str said))

(assert-eq "the elements in with their attributes, their text, and out, in order, as an SVG file is written"
	"((:in \qa\q (\qx\q \qy\q) (\q1\q \q2\q)) (:in \qb\q () ()) (:out \qb\q) (:text \q\\nhi\\n\q) (:out \qa\q))"
	(xm-said "<a x=\q1\q y=\q2\q>\n<b />\nhi\n</a>\n"))
(assert-eq "either quote, and an element closed by /> with no space before it"
	"((:in \qa\q (\qx\q \qy\q) (\q1\q \q2\q)) (:in \qb\q () ()) (:out \qb\q) (:text \qhi\q) (:out \qa\q))"
	(xm-said "<a x=\q1\q y='2'><b/>hi</a>"))
(assert-eq "an element still open at the end, a file cut short, is closed there"
	"((:in \qa\q () ()) (:text \qtext\q) (:out \qa\q))"
	(xm-said "<a>text"))
(assert-eq "those still open closed the last opened first, one closed in the file once"
	"((:in \qa\q () ()) (:in \qb\q () ()) (:text \qx\q) (:out \qb\q) (:in \qc\q () ()) (:out \qc\q) (:out \qa\q))"
	(xm-said "<a><b>x</b><c>"))
(assert-eq "text whole, what is markup elsewhere only text in it"
	"((:in \qp\q () ()) (:text \qa-b 1/2 x = 'y' don't?\q) (:out \qp\q))"
	(xm-said "<p>a-b 1/2 x = 'y' don't?</p>"))
(assert-eq "a comment with a quote and a > in it passed over"
	"((:in \qa\q () ()) (:out \qa\q))"
	(xm-said "<!-- don't > stop --><a/>"))
(assert-eq "CDATA is text, whatever is in it"
	"((:text \q <not> a ]] tag \q))"
	(xm-said "<![CDATA[ <not> a ]] tag ]]>"))
(assert-eq "a DOCTYPE with a > in its [ ] passed over, and names with a :"
	"((:in \qsvg\q (\qxmlns:xlink\q \qxlink:href\q) (\qw\q \q#u\q)) (:out \qsvg\q))"
	(xm-said "<!DOCTYPE svg [ <!ENTITY e \q>\q> ]><svg xmlns:xlink='w' xlink:href=\q#u\q/>"))
(assert-eq "entities in values and text, by name and by number, one not known left"
	(cat "((:in \qt\q (\qa\q) (\q<&A" (num-to-utf8 0x263a) "&bogus;\q)) (:text \q\\qcaf" (num-to-utf8 233) "\\q\q) (:out \qt\q))")
	(xm-said "<?xml version=\q1.0\q?><t a=\q&lt;&amp;&#65;&#x263a;&bogus;\q>&quot;caf&#233;&quot;</t>"))
(assert-eq "an element not closed in the document is closed at its end, after the last that is"
	"((:in \qa\q () ()) (:in \qb\q () ()) (:out \qb\q) (:out \qa\q))"
	(xm-said "<a><b></b>"))
(assert-eq "not well formed taken as near as can be: a key of no value, a value not quoted"
	"((:in \qa\q (\qk\q) (\q\q)) (:text \qv\q) (:out \qa\q) (:in \qb\q (\qk\q) (\qbare\q)) (:out \qb\q))"
	(xm-said "<a k>v</a><b k=bare/>"))
(assert-eq "text all white space between elements is not given"
	"((:in \qa\q () ()) (:in \qb\q () ()) (:out \qb\q) (:out \qa\q))"
	(xm-said "<a>\n  <b/>\n\t</a>"))

;the native that breaks the stream into tokens, lib/xml/lisp.vp, given a
;number: an error, and the node goes on. Its error path was code after the
;names of its states, 3 past a place of 4, and an arm64 faulted on it,
;Illegal instruction, 2026-11-23
(ffi "lib/xml/parse" xm-tokens)
(assert-error "the tokenizer given a number is an error, and this node goes on" (xm-tokens 42) {(xml-parse stream) wrong_types})

(undef (env) 'xm-said)
