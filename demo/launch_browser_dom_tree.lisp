;; ChrysaLisp Browser with Actual DOM Tree Parsing and Display
;; Loads HTML files and displays the parsed DOM tree structure

(import "lib/html/file_utils.inc")

;; Display browser header
(defun show-header ()
	(print "═════════════════════════════════════════════════════════════")
	(print "ChrysaLisp Browser - Full DOM Tree Parser and Display Engine")
	(print "═════════════════════════════════════════════════════════════")
	(print ""))

;; Recursively traverse DOM tree and display structure
(defun display-dom-node (node depth)
	; Indent based on depth
	(defq indent (slice "                                                  " 0 (* depth 2)))

	; Display node info
	(if node
		(progn
			; Get node type info
			(defq node-type (class-name node))
			(defq node-desc (cat indent "• " node-type))

			; Add specific info based on node type
			(if (eql node-type "html-element")
				(progn
					(defq tag (. node :tag))
					(if tag (setq node-desc (cat node-desc " <" tag ">"))))
				:nil)

			(if (eql node-type "text-node")
				(progn
					(defq text (. node :text))
					(if text
						(progn
							(if (> (length text) 40)
								(setq text (slice text 0 40)))
							(setq node-desc (cat node-desc " \"" text "\"")))))
				:nil)

			(print node-desc)

			; Try to get and display children
			(defq children (. node :children))
			(if children
				(progn
					(defq idx 0)
					(while (< idx (length children))
						(display-dom-node (elem-get children idx) (+ depth 1))
						(setq idx (+ idx 1))))
				:nil))))

;; Parse HTML and display DOM tree
(defun load-and-parse-html (filepath)
	(defq html-content (read-file-to-string filepath))

	(if html-content
		(progn
			(print "Step 1: HTML File Loaded")
			(print "─────────────────────────────────────────────────────────")
			(print (cat "File: " filepath))
			(print (cat "Size: " (str (length html-content)) " bytes"))
			(print "")

			(print "Step 2: Parsing HTML into DOM Tree")
			(print "─────────────────────────────────────────────────────────")

			; Parse HTML
			(defq document nil)
			(if (catch (setq document (parse-html html-content)))
				(progn
					(print "✅ HTML successfully parsed")
					(print "")

					(print "Step 3: DOM Tree Structure")
					(print "─────────────────────────────────────────────────────────")
					(if document
						(display-dom-node document 0)
						(print "⚠️  Document is nil after parsing")))
				(print "❌ Error parsing HTML")))
		(progn
			(print "❌ Failed to load HTML file")
			(print (cat "Path: " filepath)))))

;; Main demo
(defun demo-dom-tree-browser ()
	(show-header)

	(defq filepath "/home/paul/scm/ChrysaLisp_AI_made_apps_experiment/demo/sample_page.html")

	(if (file-exists? filepath)
		(progn
			(print "File Status: ✅ Found and readable")
			(print "")
			(load-and-parse-html filepath))
		(print "File Status: ❌ Not found"))

	(print "")
	(print "═════════════════════════════════════════════════════════════")
	(print "DOM tree parsing complete!")
	(print "═════════════════════════════════════════════════════════════"))

;; Run demo
(demo-dom-tree-browser)
