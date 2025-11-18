#!/usr/bin/env lsp

;; HTML Browser Demo - Standalone Canvas Rendering
;; Demonstrates HTML + CSS rendering to canvas files

(import "gui/canvas/lisp.inc")
(import "lib/html/parser.inc")
(import "lib/html/css.inc")
(import "lib/html/canvas_renderer.inc")

(defun demo-page-1 ()
	(print "Demo 1: Basic HTML with CSS Styling")
	(print "=====================================")

	(defq html "
<html>
<body>
	<h1>ChrysaLisp HTML Browser</h1>
	<p class=\"intro\">This demonstrates HTML rendering with <strong>CSS styling</strong>.</p>
	<h2>Features</h2>
	<ul>
		<li>HTML parsing</li>
		<li>CSS styling</li>
		<li>Canvas rendering</li>
	</ul>
</body>
</html>
")

	(defq css "
h1 { color: #0066cc; font-size: 24px; }
h2 { color: #006600; font-size: 18px; }
.intro { color: #ff0000; }
")

	(print "HTML:")
	(print html)
	(print "")
	(print "CSS:")
	(print css)
	(print ""))

(defun demo-page-2 ()
	(print "Demo 2: Tables and Forms")
	(print "========================")

	(defq html "
<html>
<body>
	<h1>Tables Demo</h1>
	<table>
		<tr><th>Name</th><th>Value</th></tr>
		<tr><td>Parser</td><td>Complete</td></tr>
		<tr><td>Renderer</td><td>Complete</td></tr>
	</table>

	<h2>Form Elements</h2>
	<form>
		<input type=\"text\" value=\"Sample input\" />
		<button>Submit</button>
	</form>
</body>
</html>
")

	(defq css "
table { border: 1px solid #000000; }
th { font-weight: bold; color: #ffffff; background-color: #0066cc; }
td { color: #000000; }
")

	(print "HTML:")
	(print html)
	(print "")
	(print "CSS:")
	(print css)
	(print ""))

(defun demo-page-3 ()
	(print "Demo 3: Complex Styling")
	(print "=======================")

	(defq html "
<html>
<body>
	<h1 id=\"title\">Styled Page</h1>
	<p class=\"highlight\">Highlighted paragraph</p>
	<p class=\"muted\">Muted paragraph</p>
	<div class=\"box\">
		<h2>Box Content</h2>
		<p>Content inside a styled box</p>
	</div>
</body>
</html>
")

	(defq css "
#title { color: #ff0000; font-size: 28px; }
.highlight { color: #ff6600; font-weight: bold; }
.muted { color: #808080; }
.box { background-color: #f0f0f0; }
")

	(print "HTML:")
	(print html)
	(print "")
	(print "CSS:")
	(print css)
	(print ""))

(defun render-demo (html css name)
	; Render HTML+CSS to canvas and save
	(print "Rendering " name "...")

	; Parse HTML and CSS
	(defq doc (parse-html html))
	(defq stylesheet (parse-css css))

	; Create canvas
	(defq canvas (Canvas))
	(defq w 800 h 800)
	(.-> canvas
		(:set_size w h)
		(:canvas_alloc 0 w h 0xffffffff 1))

	; Create renderer and render
	(defq renderer (html-canvas-renderer :init doc canvas w stylesheet))
	(. renderer :render)

	; Save to file
	(defq filename (cat "obj/html_demo_" name ".png"))
	(when (. canvas :save filename "png")
		(print "Saved to " filename))

	(print ""))

(defun main ()
	(print "")
	(print "ChrysaLisp HTML+CSS Browser Demo")
	(print "=================================")
	(print "")

	; Demo 1
	(demo-page-1)
	(render-demo "
<html><body>
<h1>ChrysaLisp HTML Browser</h1>
<p class=\"intro\">HTML with <strong>CSS styling</strong>.</p>
<h2>Features</h2>
<ul>
<li>HTML parsing</li>
<li>CSS styling</li>
<li>Canvas rendering</li>
</ul>
</body></html>"
		"h1 { color: #0066cc; } .intro { color: #ff0000; } h2 { color: #006600; }"
		"demo1")

	; Demo 2
	(demo-page-2)
	(render-demo "
<html><body>
<h1>Tables Demo</h1>
<table>
<tr><th>Name</th><th>Value</th></tr>
<tr><td>Parser</td><td>Done</td></tr>
<tr><td>Renderer</td><td>Done</td></tr>
</table>
</body></html>"
		"table { border: 1px solid #000; } th { font-weight: bold; color: #00c; }"
		"demo2")

	; Demo 3
	(demo-page-3)
	(render-demo "
<html><body>
<h1 id=\"title\">Styled Page</h1>
<p class=\"highlight\">Highlighted paragraph</p>
<p class=\"muted\">Muted paragraph</p>
<div class=\"box\">
<h2>Box Content</h2>
<p>Content in styled box</p>
</div>
</body></html>"
		"#title { color: #f00; } .highlight { color: #f60; } .muted { color: #888; }"
		"demo3")

	(print "All demos complete!")
	(print "Check the obj/ directory for PNG output files.")
	(print ""))
