#!/usr/bin/env lsp

;; HTML Parser and Renderer Demo
;; Demonstrates the ChrysaLisp HTML parsing and rendering capabilities

(import "lib/html/parser.inc")
(import "lib/html/renderer.inc")
(import "lib/html/part.inc")

(defun print-separator ()
	(print "========================================"))

(defun demo-simple-html ()
	(print-separator)
	(print "Demo 1: Simple HTML Parsing")
	(print-separator)

	(defq html "<html><body><h1>Welcome to ChrysaLisp HTML</h1><p>This is a simple HTML document.</p></body></html>")

	(print "Input HTML:")
	(print html)
	(print "")

	(defq doc (parse-html html))
	(print "Parsed DOM:")
	(print (. doc :to-string))
	(print "")

	(print "Rendered Text:")
	(print (render-html-to-text doc))
	(print ""))

(defun demo-complex-html ()
	(print-separator)
	(print "Demo 2: Complex HTML with Attributes")
	(print-separator)

	(defq html "
<html>
<head><title>Example Page</title></head>
<body>
	<div id=\"header\" class=\"main-header\">
		<h1>ChrysaLisp HTML Parser</h1>
		<p>A modern HTML parser written in Lisp</p>
	</div>
	<div id=\"content\">
		<h2>Features</h2>
		<ul>
			<li>Tokenization</li>
			<li>DOM construction</li>
			<li>Text rendering</li>
		</ul>
		<p>For more information, visit <a href=\"https://github.com/vygr/ChrysaLisp\">the repository</a>.</p>
	</div>
</body>
</html>")

	(print "Input HTML:")
	(print html)
	(print "")

	(defq doc (parse-html html))

	(print "Finding element by ID:")
	(defq header (. doc :get-element-by-id "header"))
	(if header
		(print "Found header: " (. header :to-string))
		(print "Header not found"))
	(print "")

	(print "Finding elements by tag name:")
	(defq lists (. doc :get-elements-by-tag-name "ul"))
	(print "Found " (length lists) " list(s)")
	(print "")

	(print "Rendered Text:")
	(print (render-html-to-text doc 60))
	(print ""))

(defun demo-html-part ()
	(print-separator)
	(print "Demo 3: HTML Part (like KHTML)")
	(print-separator)

	(defq part (html-part :init))
	(. part :begin)
	(. part :write "<html><body>")
	(. part :write "<h1>HTML Part Demo</h1>")
	(. part :write "<p>This demonstrates the HTML Part API, ")
	(. part :write "similar to KHTML's KHTMLPart.</p>")
	(. part :write "</body></html>")
	(. part :end)

	(print "HTML content written to part:")
	(print (. part :get-html))
	(print "")

	; Parse and render
	(defq doc (parse-html (. part :get-html)))
	(print "Rendered output:")
	(print (render-html-to-text doc))
	(print ""))

(defun demo-text-formatting ()
	(print-separator)
	(print "Demo 4: Text Formatting")
	(print-separator)

	(defq html "
<article>
	<h1>The Philosophy of ChrysaLisp</h1>
	<p>ChrysaLisp represents a <strong>radical rethinking</strong> of how a Lisp system can be built for <em>maximum performance</em> and resilience.</p>
	<h2>Core Principles</h2>
	<ul>
		<li>Cooperative design</li>
		<li>Iteration over recursion</li>
		<li>O(1) caching</li>
	</ul>
	<p>For more details, visit the <a href=\"https://github.com/vygr/ChrysaLisp\">GitHub repository</a>.</p>
</article>")

	(defq doc (parse-html html))
	(print "Rendered with 60-character line width:")
	(print (render-html-to-text doc 60))
	(print ""))

(defun main ()
	(print "")
	(print "ChrysaLisp HTML Parser and Renderer")
	(print "Ported from KDE KHTML")
	(print "")

	(demo-simple-html)
	(demo-complex-html)
	(demo-html-part)
	(demo-text-formatting)

	(print-separator)
	(print "All demos complete!")
	(print-separator))
