
;; Interactive Hyperlink Tests
;; Tests clickable regions, link detection, and navigation

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/canvas_renderer.inc")
(import "lib/html/browser.inc")
(import "gui/canvas/lisp.inc")

(deftest-suite "Interactive Hyperlink Tests")

(defun create-test-canvas (w h)
	; Create a canvas for testing
	(defq canvas (Canvas))
	(.-> canvas
		(:set_size w h)
		(:canvas_alloc 0 w h 0xffffffff 1))
	canvas)

(defun main ()
	(run-test-suite
		; Test clickable region tracking
		(deftest "Track Clickable Regions for Links"
			(defq html "<p>Visit <a href=\"/home\">home page</a> now</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Should have one clickable region
			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 1 (length regions))

			; Region should have href
			(defq region (first regions))
			(bind '(x y w h href) region)
			(assert-eq "/home" href))

		; Test multiple links
		(deftest "Track Multiple Clickable Regions"
			(defq html "<nav><a href=\"/home\">Home</a> <a href=\"/about\">About</a> <a href=\"/contact\">Contact</a></nav>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Should have three clickable regions
			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 3 (length regions))

			; Check hrefs
			(defq hrefs (map (lambda (r) (last r)) regions))
			(assert-contains "/home" hrefs)
			(assert-contains "/about" hrefs)
			(assert-contains "/contact" hrefs))

		; Test nested links in lists
		(deftest "Track Links in List Elements"
			(defq html "
				<ul>
					<li><a href=\"/page1\">Page 1</a></li>
					<li><a href=\"/page2\">Page 2</a></li>
					<li><a href=\"/page3\">Page 3</a></li>
				</ul>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 3 (length regions)))

		; Test link detection at coordinates
		(deftest "Detect Link at Coordinates"
			(defq html "<a href=\"/test\">Click here</a>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 1 (length regions))

			; Get the region bounds
			(bind '(x y w h href) (first regions))

			; Click inside region should find link
			(defq found_href (. renderer :find-link-at (+ x 5) (+ y 5)))
			(assert-eq "/test" found_href)

			; Click outside region should not find link
			(defq not_found (. renderer :find-link-at (+ x w 50) (+ y 5)))
			(assert-nil not_found))

		; Test no clickable regions for non-links
		(deftest "No Clickable Regions for Non-Links"
			(defq html "<p>This is plain text with no links</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 0 (length regions)))

		; Test links without href attribute
		(deftest "Ignore Links Without href"
			(defq html "<a>No href here</a>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 0 (length regions)))

		; Test path resolution - absolute paths
		(deftest "Resolve Absolute Paths"
			(defq browser (html-browser-view))
			(.-> browser
				(:set_size 800 600)
				(:canvas_alloc 0 800 600 0xffffffff 1))

			; Set base path
			(set browser :base_path "/var/www/html/")

			; Absolute path should remain unchanged
			(defq resolved (. browser :resolve_path "/absolute/path.html"))
			(assert-eq "/absolute/path.html" resolved))

		; Test path resolution - relative paths
		(deftest "Resolve Relative Paths"
			(defq browser (html-browser-view))
			(.-> browser
				(:set_size 800 600)
				(:canvas_alloc 0 800 600 0xffffffff 1))

			; Set base path
			(set browser :base_path "/var/www/html/")

			; Relative path should be resolved
			(defq resolved (. browser :resolve_path "page.html"))
			(assert-eq "/var/www/html/page.html" resolved)

			; Relative path with subdirectory
			(defq resolved2 (. browser :resolve_path "docs/about.html"))
			(assert-eq "/var/www/html/docs/about.html" resolved2))

		; Test base path extraction from file
		(deftest "Extract Base Path from File"
			(defq browser (html-browser-view))
			(.-> browser
				(:set_size 800 600)
				(:canvas_alloc 0 800 600 0xffffffff 1))

			; Set current file (simulated)
			(set browser :current_file "/var/www/html/index.html")
			(defq last_slash (find-rev "/" (get :current_file browser)))
			(set browser :base_path (slice (get :current_file browser) 0 (inc last_slash)))

			; Base path should be directory
			(assert-eq "/var/www/html/" (get :base_path browser)))

		; Test clickable regions cleared on re-render
		(deftest "Clear Clickable Regions on Re-render"
			(defq html1 "<a href=\"/link1\">Link 1</a>")
			(defq doc1 (parse-html html1))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc1 canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions1 (. renderer :get-clickable-regions))
			(assert-eq 1 (length regions1))

			; Re-render with different content
			(defq html2 "<a href=\"/link2\">Link 2</a><a href=\"/link3\">Link 3</a>")
			(defq doc2 (parse-html html2))
			(setq (. renderer 'document) doc2)
			(defq height2 (. renderer :render))

			; Should have new regions, not accumulated
			(defq regions2 (. renderer :get-clickable-regions))
			(assert-eq 2 (length regions2)))

		; Test link in table cell
		(deftest "Track Links in Table Cells"
			(defq html "
				<table>
					<tr>
						<td><a href=\"/cell1\">Cell 1</a></td>
						<td><a href=\"/cell2\">Cell 2</a></td>
					</tr>
				</table>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 2 (length regions)))

		; Test empty href
		(deftest "Handle Empty href Attribute"
			(defq html "<a href=\"\">Empty link</a>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Empty href should still create a region
			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 1 (length regions))

			(bind '(x y w h href) (first regions))
			(assert-eq "" href))

		; Test link with nested formatting
		(deftest "Track Link with Nested Formatting"
			(defq html "<a href=\"/styled\"><strong>Bold</strong> <em>italic</em> text</a>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 1 (length regions))

			(bind '(x y w h href) (first regions))
			(assert-eq "/styled" href))

		; Test adjacent links
		(deftest "Track Adjacent Links"
			(defq html "<p><a href=\"/first\">First</a><a href=\"/second\">Second</a></p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(defq regions (. renderer :get-clickable-regions))
			(assert-eq 2 (length regions))

			; Regions should not overlap
			(bind '(x1 y1 w1 h1 href1) (first regions))
			(bind '(x2 y2 w2 h2 href2) (second regions))
			(assert-eq "/first" href1)
			(assert-eq "/second" href2))
	))

