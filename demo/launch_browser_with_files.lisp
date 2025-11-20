;; ChrysaLisp Browser Launcher - File:// Support
;; Demonstrates loading HTML from filesystem via file:// URLs

(import "lib/html/file_utils.inc")

;; Display browser info
(defun show-browser-header ()
	(print "═══════════════════════════════════════════")
	(print "ChrysaLisp Browser - File:// Support Demo")
	(print "═══════════════════════════════════════════")
	(print ""))

;; Load and display HTML file
(defun load-and-display (filepath)
	(defq content (read-file-to-string filepath))
	(if content
		(progn
			(print "✅ File loaded successfully")
			(print (cat "File size: " (str (file-size filepath)) " bytes"))
			(print "")
			(print "File content:")
			(print "─────────────────────────────────────────")
			(print content)
			(print "─────────────────────────────────────────"))
		(progn
			(print "❌ Failed to load file:")
			(print filepath))))

;; Main demo
(defun demo-file-loading ()
	(show-browser-header)

	; Demo 1: Load from filesystem
	(print "Demo 1: Loading HTML from filesystem")
	(print "URL: file:///home/paul/scm/ChrysaLisp_AI_made_apps_experiment/demo/sample_page.html")
	(print "")

	(defq filepath "/home/paul/scm/ChrysaLisp_AI_made_apps_experiment/demo/sample_page.html")
	(if (file-exists? filepath)
		(progn
			(print "✅ File exists and is readable")
			(print "")
			(load-and-display filepath))
		(print "⚠️  File not found or not readable"))

	(print "")
	(print "═══════════════════════════════════════════")
	(print "File loading demonstration complete!")
	(print "═══════════════════════════════════════════"))

;; Run demo
(demo-file-loading)
