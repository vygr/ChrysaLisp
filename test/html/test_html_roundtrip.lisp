
;; HTML Round-trip Serialization Tests
;; Inspired by XStream's TreeMapAndTreeSetTest patterns
;; Tests that HTML can be parsed to DOM and serialized back

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/dom.inc")

(deftest-suite "HTML Round-trip Serialization Tests")

(defun serialize-element (element)
	; Serialize an element back to HTML string
	(defq tag (. element :get-tag-name))
	(defq html (cat "<" tag))

	; Add attributes
	(defq attrs (. element 'attributes))
	(when attrs
		(each-pair (lambda (k v)
			(setq html (cat html " " k "=\"" v "\"")))
			attrs))

	; Close opening tag
	(setq html (cat html ">"))

	; Add children
	(each! 0 -1 (lambda (child)
		(cond
			((= (. child 'node_type) 1)  ; NODE_ELEMENT
				(setq html (cat html (serialize-element child))))
			((= (. child 'node_type) 3)  ; NODE_TEXT
				(setq html (cat html (. child :get-text))))
			((= (. child 'node_type) 8)  ; NODE_COMMENT
				(setq html (cat html "<!--" (. child :get-text) "-->")))))
		(. element 'child_nodes))

	; Closing tag (skip void elements)
	(unless (or (= tag "br") (= tag "img") (= tag "input") (= tag "hr"))
		(setq html (cat html "</" tag ">")))

	html)

(defun serialize-document (doc)
	; Serialize entire document to HTML
	(defq html "")
	(each! 0 -1 (lambda (child)
		(when (= (. child 'node_type) 1)  ; NODE_ELEMENT
			(setq html (cat html (serialize-element child)))))
		(. doc 'child_nodes))
	html)

(defun normalize-html (html)
	; Normalize HTML for comparison (remove extra whitespace)
	(defq normalized (cat html))
	; Remove newlines and extra spaces
	(setq normalized (reduce (lambda (s ch)
		(if (or (= ch "\n") (= ch "\r") (= ch "\t"))
			s
			(cat s ch)))
		"" (split normalized "")))
	normalized)

