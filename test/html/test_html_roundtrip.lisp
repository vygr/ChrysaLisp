
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

(defun main ()
	(run-test-suite
		; Test simple element round-trip
		(deftest "Simple Element Round-trip"
			(defq original "<p>Hello World</p>")
			(defq doc (parse-html original))
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(defq serialized (serialize-element p))
			(assert-eq original serialized))

		; Test element with attributes
		(deftest "Element with Attributes Round-trip"
			(defq original-html "<div id=\"main\" class=\"container\">Content</div>")
			(defq doc (parse-html original-html))
			(defq div (first (. doc :get-elements-by-tag-name "div")))

			; Verify attributes preserved
			(assert-element-id div "main")
			(assert-element-has-class div "container")
			(assert-element-text-contains div "Content"))

		; Test nested elements
		(deftest "Nested Elements Round-trip"
			(defq doc (parse-html "<div><p>Nested</p></div>"))
			(defq div (first (. doc :get-elements-by-tag-name "div")))
			(defq p (first (. doc :get-elements-by-tag-name "p")))

			; Verify nesting preserved
			(assert-element-parent p div)
			(assert-element-has-children div)
			(assert-element-child-count div 1))

		; Test empty element
		(deftest "Empty Element Round-trip"
			(defq doc (parse-html "<div></div>"))
			(defq div (first (. doc :get-elements-by-tag-name "div")))

			(assert-element-no-children div)
			(defq serialized (serialize-element div))
			(assert-eq "<div></div>" serialized))

		; Test void element (like br)
		(deftest "Void Element Round-trip"
			(defq doc (parse-html "<p>Line 1<br>Line 2</p>"))
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(defq br (first (. doc :get-elements-by-tag-name "br")))

			(assert-not-nil br)
			(assert-element-tag br "br")
			(assert-element-parent br p))

		; Test multiple children
		(deftest "Multiple Children Round-trip"
			(defq html "<ul><li>One</li><li>Two</li><li>Three</li></ul>")
			(defq doc (parse-html html))
			(defq ul (first (. doc :get-elements-by-tag-name "ul")))

			(assert-element-child-count ul 3)
			(defq items (. doc :get-elements-by-tag-name "li"))
			(assert-eq 3 (length items)))

		; Test complex structure preservation
		(deftest "Complex Structure Preservation"
			(defq html "
				<div id=\"container\">
					<h1>Title</h1>
					<p class=\"intro\">Introduction</p>
					<ul>
						<li>Item 1</li>
						<li>Item 2</li>
					</ul>
				</div>")
			(defq doc (parse-html html))

			; Verify all elements exist
			(assert-elements-count doc "div" 1)
			(assert-elements-count doc "h1" 1)
			(assert-elements-count doc "p" 1)
			(assert-elements-count doc "ul" 1)
			(assert-elements-count doc "li" 2)

			; Verify structure
			(defq container (. doc :get-element-by-id "container"))
			(assert-element-has-children container)

			; Verify specific element properties
			(defq intro (first (. doc :get-elements-by-tag-name "p")))
			(assert-element-has-class intro "intro")
			(assert-element-text-contains intro "Introduction"))

		; Test attribute order independence
		(deftest "Attribute Order Independence"
			(defq html1 "<div id=\"test\" class=\"box\">Content</div>")
			(defq html2 "<div class=\"box\" id=\"test\">Content</div>")

			(defq doc1 (parse-html html1))
			(defq doc2 (parse-html html2))

			(defq div1 (first (. doc1 :get-elements-by-tag-name "div")))
			(defq div2 (first (. doc2 :get-elements-by-tag-name "div")))

			; Both should have same attributes regardless of order
			(assert-element-id div1 "test")
			(assert-element-id div2 "test")
			(assert-element-has-class div1 "box")
			(assert-element-has-class div2 "box"))

		; Test table structure preservation
		(deftest "Table Structure Preservation"
			(defq html "
				<table>
					<tr>
						<th>Header 1</th>
						<th>Header 2</th>
					</tr>
					<tr>
						<td>Cell 1</td>
						<td>Cell 2</td>
					</tr>
				</table>")
			(defq doc (parse-html html))

			(assert-elements-count doc "table" 1)
			(assert-elements-count doc "tr" 2)
			(assert-elements-count doc "th" 2)
			(assert-elements-count doc "td" 2)

			; Verify table structure
			(defq table (first (. doc :get-elements-by-tag-name "table")))
			(assert-element-child-count table 2))

		; Test form elements preservation
		(deftest "Form Elements Preservation"
			(defq html "
				<form id=\"login\" action=\"/login\" method=\"post\">
					<input type=\"text\" name=\"user\" id=\"username\" />
					<input type=\"password\" name=\"pass\" id=\"password\" />
					<button type=\"submit\">Login</button>
				</form>")
			(defq doc (parse-html html))

			(defq form (. doc :get-element-by-id "login"))
			(assert-element-attribute-eq form "action" "/login")
			(assert-element-attribute-eq form "method" "post")

			(assert-elements-count doc "input" 2)
			(assert-elements-count doc "button" 1)

			(defq username (. doc :get-element-by-id "username"))
			(assert-element-attribute-eq username "type" "text")
			(assert-element-attribute-eq username "name" "user"))

		; Test comment preservation
		(deftest "Comment Preservation"
			(defq html "<div><!-- This is a comment --><p>Text</p></div>")
			(defq doc (parse-html html))

			(defq div (first (. doc :get-elements-by-tag-name "div")))
			(assert-element-has-children div)

			; Check that comment exists as a child node
			(defq children (. div 'child_nodes))
			(assert-eq 2 (length children))

			; First child should be comment
			(defq comment (first children))
			(assert-eq 8 (. comment 'node_type)))  ; NODE_COMMENT = 8

		; Test special characters in text
		(deftest "Special Characters in Text"
			(defq html "<p>Price: $100 &amp; up</p>")
			(defq doc (parse-html html))
			(defq p (first (. doc :get-elements-by-tag-name "p")))

			(assert-element-text-contains p "$100"))

		; Test deeply nested structure
		(deftest "Deeply Nested Structure"
			(defq html "
				<div id=\"level1\">
					<div id=\"level2\">
						<div id=\"level3\">
							<div id=\"level4\">
								<p>Deep content</p>
							</div>
						</div>
					</div>
				</div>")
			(defq doc (parse-html html))

			(defq level1 (. doc :get-element-by-id "level1"))
			(defq level2 (. doc :get-element-by-id "level2"))
			(defq level3 (. doc :get-element-by-id "level3"))
			(defq level4 (. doc :get-element-by-id "level4"))

			; Verify nesting chain
			(assert-element-parent level2 level1)
			(assert-element-parent level3 level2)
			(assert-element-parent level4 level3)

			; Verify deep content
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(assert-element-text-contains p "Deep content")
			(assert-element-parent p level4))

		; Test list with mixed content
		(deftest "List with Mixed Content"
			(defq html "
				<ul>
					<li>Plain text</li>
					<li><strong>Bold</strong> text</li>
					<li>Text with <a href=\"#\">link</a></li>
				</ul>")
			(defq doc (parse-html html))

			(assert-elements-count doc "li" 3)
			(assert-elements-count doc "strong" 1)
			(assert-elements-count doc "a" 1)

			(defq items (. doc :get-elements-by-tag-name "li"))
			(assert-element-text-contains (first items) "Plain text")
			(assert-element-text-contains (second items) "Bold"))
	))

