#!/usr/bin/env lsp

;; Unit tests for HTML Parser and Renderer

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/renderer.inc")

(deftest-suite "HTML Parser and Renderer Tests")

(deftest "Parse Simple HTML"
	(defq html "<html><body><p>Hello World</p></body></html>")
	(defq doc (parse-html html))
	(assert-not-nil doc)
	(assert-eq NODE_DOCUMENT (. doc 'node_type)))

(deftest "Parse HTML with Attributes"
	(defq html "<div id=\"test\" class=\"container\">Content</div>")
	(defq doc (parse-html html))
	(assert-not-nil doc)
	(defq divs (. doc :get-elements-by-tag-name "div"))
	(assert-eq 1 (length divs))
	(defq div (first divs))
	(assert-eq "test" (. div :get-attribute "id"))
	(assert-eq "container" (. div :get-attribute "class")))

(deftest "Parse Nested HTML"
	(defq html "<div><p><span>Nested</span></p></div>")
	(defq doc (parse-html html))
	(defq divs (. doc :get-elements-by-tag-name "div"))
	(assert-eq 1 (length divs))
	(defq ps (. doc :get-elements-by-tag-name "p"))
	(assert-eq 1 (length ps))
	(defq spans (. doc :get-elements-by-tag-name "span"))
	(assert-eq 1 (length spans)))

(deftest "Parse Self-Closing Tags"
	(defq html "<p>Line 1<br>Line 2</p>")
	(defq doc (parse-html html))
	(defq ps (. doc :get-elements-by-tag-name "p"))
	(assert-eq 1 (length ps))
	(defq brs (. doc :get-elements-by-tag-name "br"))
	(assert-eq 1 (length brs)))

(deftest "Parse HTML Comments"
	(defq html "<div><!-- This is a comment --><p>Text</p></div>")
	(defq doc (parse-html html))
	(defq divs (. doc :get-elements-by-tag-name "div"))
	(assert-eq 1 (length divs))
	(defq div (first divs))
	; Should have 2 children: comment and p
	(assert-eq 2 (length (. div 'child_nodes))))

(deftest "Get Element By ID"
	(defq html "<div><p id=\"para1\">First</p><p id=\"para2\">Second</p></div>")
	(defq doc (parse-html html))
	(defq elem (. doc :get-element-by-id "para1"))
	(assert-not-nil elem)
	(assert-eq "p" (. elem :get-tag-name)))

(deftest "Get Elements By Tag Name"
	(defq html "<div><p>First</p><p>Second</p><span>Third</span></div>")
	(defq doc (parse-html html))
	(defq ps (. doc :get-elements-by-tag-name "p"))
	(assert-eq 2 (length ps)))

(deftest "Render Simple Text"
	(defq html "<p>Hello World</p>")
	(defq doc (parse-html html))
	(defq text (render-html-to-text doc))
	(assert-not-nil text)
	(assert-neq "" text))

(deftest "Render Headings"
	(defq html "<h1>Title</h1><p>Content</p>")
	(defq doc (parse-html html))
	(defq text (render-html-to-text doc))
	(assert-not-nil text))

(deftest "Render Links"
	(defq html "<p>Visit <a href=\"http://example.com\">Example</a></p>")
	(defq doc (parse-html html))
	(defq text (render-html-to-text doc))
	(assert-not-nil text))

(deftest "Render Lists"
	(defq html "<ul><li>Item 1</li><li>Item 2</li></ul>")
	(defq doc (parse-html html))
	(defq text (render-html-to-text doc))
	(assert-not-nil text))

(defun main ()
	(run-test-suite
		(deftest "Parse Simple HTML"
			(defq html "<html><body><p>Hello World</p></body></html>")
			(defq doc (parse-html html))
			(assert-not-nil doc)
			(assert-eq NODE_DOCUMENT (. doc 'node_type)))

		(deftest "Parse HTML with Attributes"
			(defq html "<div id=\"test\" class=\"container\">Content</div>")
			(defq doc (parse-html html))
			(assert-not-nil doc)
			(defq divs (. doc :get-elements-by-tag-name "div"))
			(assert-eq 1 (length divs))
			(defq div (first divs))
			(assert-eq "test" (. div :get-attribute "id"))
			(assert-eq "container" (. div :get-attribute "class")))

		(deftest "Parse Nested HTML"
			(defq html "<div><p><span>Nested</span></p></div>")
			(defq doc (parse-html html))
			(defq divs (. doc :get-elements-by-tag-name "div"))
			(assert-eq 1 (length divs))
			(defq ps (. doc :get-elements-by-tag-name "p"))
			(assert-eq 1 (length ps))
			(defq spans (. doc :get-elements-by-tag-name "span"))
			(assert-eq 1 (length spans)))

		(deftest "Parse Self-Closing Tags"
			(defq html "<p>Line 1<br>Line 2</p>")
			(defq doc (parse-html html))
			(defq ps (. doc :get-elements-by-tag-name "p"))
			(assert-eq 1 (length ps))
			(defq brs (. doc :get-elements-by-tag-name "br"))
			(assert-eq 1 (length brs)))

		(deftest "Parse HTML Comments"
			(defq html "<div><!-- This is a comment --><p>Text</p></div>")
			(defq doc (parse-html html))
			(defq divs (. doc :get-elements-by-tag-name "div"))
			(assert-eq 1 (length divs))
			(defq div (first divs))
			(assert-eq 2 (length (. div 'child_nodes))))

		(deftest "Get Element By ID"
			(defq html "<div><p id=\"para1\">First</p><p id=\"para2\">Second</p></div>")
			(defq doc (parse-html html))
			(defq elem (. doc :get-element-by-id "para1"))
			(assert-not-nil elem)
			(assert-eq "p" (. elem :get-tag-name)))

		(deftest "Get Elements By Tag Name"
			(defq html "<div><p>First</p><p>Second</p><span>Third</span></div>")
			(defq doc (parse-html html))
			(defq ps (. doc :get-elements-by-tag-name "p"))
			(assert-eq 2 (length ps)))

		(deftest "Render Simple Text"
			(defq html "<p>Hello World</p>")
			(defq doc (parse-html html))
			(defq text (render-html-to-text doc))
			(assert-not-nil text)
			(assert-neq "" text))

		(deftest "Render Headings"
			(defq html "<h1>Title</h1><p>Content</p>")
			(defq doc (parse-html html))
			(defq text (render-html-to-text doc))
			(assert-not-nil text))

		(deftest "Render Links"
			(defq html "<p>Visit <a href=\"http://example.com\">Example</a></p>")
			(defq doc (parse-html html))
			(defq text (render-html-to-text doc))
			(assert-not-nil text))

		(deftest "Render Lists"
			(defq html "<ul><li>Item 1</li><li>Item 2</li></ul>")
			(defq doc (parse-html html))
			(defq text (render-html-to-text doc))
			(assert-not-nil text))))
