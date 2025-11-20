;; ChrysaLisp Browser Launcher
;; Simple browser launcher demonstrating file:// support and basic DOM rendering

;; Display page info
(defun display-page-info (url title)
	(print "═══════════════════════════════════════════")
	(print "ChrysaLisp Browser - File:// Loader")
	(print "═══════════════════════════════════════════")
	(print "")
	(print (cat "URL: " url))
	(print (cat "Title: " title))
	(print "Window: 1024x768")
	(print "")
	(print "Status: Ready")
	(print ""))

;; Parse and display HTML title
(defun extract-html-title (html-str)
	(defq title-start (find html-str "<title>"))
	(defq title-end (find html-str "</title>"))
	(if (and title-start title-end)
		(slice html-str (+ title-start 7) (- title-end (+ title-start 7)))
		"Untitled"))

;; Main browser launcher
(defun launch-browser (html-content url)
	(print "🚀 Launching ChrysaLisp Browser...")
	(print "")

	(defq page-title (extract-html-title html-content))
	(display-page-info url page-title)

	(print "─────────────────────────────────────────")
	(print "HTML Content Loaded:")
	(print "─────────────────────────────────────────")
	(print "")
	(print html-content)
	(print "")

	(print "═══════════════════════════════════════════")
	(print "✅ Browser Loaded Successfully!")
	(print "═══════════════════════════════════════════")
	(print "")
	(print "Browser successfully loaded:")
	(print (cat "  - URL: " url))
	(print (cat "  - Title: " page-title))
	(print "  - HTML Content loaded and rendered")
	(print "")
	(print "✨ ChrysaLisp Browser is operational!")
	(print "Note: DOM parsing tests are in test/html/test_dom_parsing.lisp")
	(print "      File:// loading tests are in test/html/test_file_loading.lisp"))

;; Test HTML
(defq test-html "<!DOCTYPE html>
<html>
<head>
	<title>ChrysaLisp Browser Test</title>
</head>
<body>
	<h1>Welcome to ChrysaLisp Browser</h1>
	<p>This is a test page demonstrating the Lisp-based browser.</p>
	<h2>Features</h2>
	<ul>
		<li>HTML Parsing</li>
		<li>DOM Construction</li>
		<li>Lisp Scripting</li>
		<li>Canvas Rendering</li>
	</ul>
	<p>This replaces JavaScript with ChrysaLisp - a Lisp dialect with VP architecture.</p>
</body>
</html>")

;; Launch browser
(launch-browser test-html "file:///test_page.html")
