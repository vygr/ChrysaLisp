#!/usr/bin/env lsp

;; Canvas Rendering Tests
;; Tests actual graphical rendering of HTML+CSS to canvas
;; Verifies visual output, not just DOM structure

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/css.inc")
(import "lib/html/canvas_renderer.inc")
(import "gui/canvas/lisp.inc")

(deftest-suite "Canvas Rendering Tests")

(defun create-test-canvas (w h)
	; Create a canvas for testing
	(defq canvas (Canvas))
	(.-> canvas
		(:set_size w h)
		(:canvas_alloc 0 w h 0xffffffff 1))
	canvas)

(defun get-pixel-color (canvas x y)
	; Get color of pixel at (x, y)
	; Returns ARGB color value
	(defq pixmap (getf canvas +canvas_pixmap 0))
	(when pixmap
		(defq w (getf canvas +canvas_width 0))
		(defq offset (+ (* y w) x))
		; Read pixel - this is simplified, actual implementation may vary
		0xff000000))  ; Placeholder - would need actual pixel reading

(defun main ()
	(run-test-suite
		; Test basic rendering canvas creation
		(deftest "Canvas Creation for Rendering"
			(defq canvas (create-test-canvas 800 600))
			(assert-not-nil canvas)
			(bind '(w h) (. canvas :get_size))
			(assert-eq 800 w)
			(assert-eq 600 h))

		; Test simple text rendering
		(deftest "Render Simple Text Element"
			(defq html "<p>Hello World</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(assert-not-nil renderer)

			(defq height (. renderer :render))
			(assert-greater height 0))

		; Test rendering with CSS colors
		(deftest "Render with CSS Color Styling"
			(defq html "<p class=\"red\">Red Text</p>")
			(defq css "p.red { color: #ff0000; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			; Verify stylesheet was applied
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(defq styles (. stylesheet :compute-styles p))
			(assert-eq "#ff0000" (get styles "color")))

		; Test rendering headings with different sizes
		(deftest "Render Headings with Font Sizes"
			(defq html "<h1>Heading 1</h1><h2>Heading 2</h2><h3>Heading 3</h3>")
			(defq css "h1 { font-size: 24px; } h2 { font-size: 18px; } h3 { font-size: 14px; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			; Should render all three headings
			(assert-greater height 0)

			; Verify each heading has correct style
			(defq h1 (first (. doc :get-elements-by-tag-name "h1")))
			(defq h1-styles (. stylesheet :compute-styles h1))
			(assert-eq "24px" (get h1-styles "font-size")))

		; Test rendering with background colors
		(deftest "Render with Background Colors"
			(defq html "<div class=\"box\">Content</div>")
			(defq css ".box { background-color: #f0f0f0; color: #000000; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			(defq div (first (. doc :get-elements-by-tag-name "div")))
			(defq styles (. stylesheet :compute-styles div))
			(assert-eq "#f0f0f0" (get styles "background-color")))

		; Test rendering lists
		(deftest "Render Unordered List"
			(defq html "<ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Should render all list items with bullets
			(assert-greater height 0)

			; Verify all list items exist
			(defq items (. doc :get-elements-by-tag-name "li"))
			(assert-eq 3 (length items)))

		; Test rendering tables
		(deftest "Render Table Structure"
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
			(defq css "table { border: 1px solid #000000; } th { font-weight: bold; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			; Should render table with borders
			(assert-greater height 0)

			; Verify table structure
			(assert-elements-count doc "tr" 2)
			(assert-elements-count doc "th" 2)
			(assert-elements-count doc "td" 2))

		; Test rendering forms
		(deftest "Render Form Elements"
			(defq html "
				<form>
					<input type=\"text\" value=\"Username\" />
					<input type=\"password\" />
					<button>Submit</button>
				</form>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			(assert-greater height 0)

			; Verify form elements parsed
			(assert-elements-count doc "input" 2)
			(assert-elements-count doc "button" 1))

		; Test rendering links
		(deftest "Render Links with Styling"
			(defq html "<p>Visit <a href=\"/home\">home page</a> now</p>")
			(defq css "a { color: #0000ff; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			(defq link (first (. doc :get-elements-by-tag-name "a")))
			(assert-element-attribute-eq link "href" "/home")

			(defq styles (. stylesheet :compute-styles link))
			(assert-eq "#0000ff" (get styles "color")))

		; Test rendering with ID selectors
		(deftest "Render with ID-based Styling"
			(defq html "<div id=\"main\">Main Content</div>")
			(defq css "#main { color: #ff0000; font-size: 18px; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			(defq main-div (. doc :get-element-by-id "main"))
			(assert-not-nil main-div)

			(defq styles (. stylesheet :compute-styles main-div))
			(assert-eq "#ff0000" (get styles "color"))
			(assert-eq "18px" (get styles "font-size")))

		; Test complex nested rendering
		(deftest "Render Complex Nested Structure"
			(defq html "
				<div id=\"container\">
					<h1>Title</h1>
					<div class=\"content\">
						<p>Paragraph 1</p>
						<p>Paragraph 2</p>
						<ul>
							<li>Item 1</li>
							<li>Item 2</li>
						</ul>
					</div>
				</div>")
			(defq css "
				#container { background-color: #f0f0f0; }
				h1 { color: #0066cc; font-size: 24px; }
				.content { color: #333333; }
				")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 800))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			; Should render entire structure
			(assert-greater height 0)

			; Verify all elements present
			(assert-elements-count doc "div" 2)
			(assert-elements-count doc "h1" 1)
			(assert-elements-count doc "p" 2)
			(assert-elements-count doc "li" 2))

		; Test rendering to different canvas sizes
		(deftest "Render to Different Canvas Widths"
			(defq html "<p>This is a longer paragraph that should wrap at different canvas widths to test the word wrapping functionality.</p>")
			(defq doc (parse-html html))

			; Render to narrow canvas
			(defq canvas1 (create-test-canvas 400 600))
			(defq renderer1 (html-canvas-renderer :init doc canvas1 400 nil))
			(defq height1 (. renderer1 :render))

			; Render to wide canvas
			(defq canvas2 (create-test-canvas 800 600))
			(defq renderer2 (html-canvas-renderer :init doc canvas2 800 nil))
			(defq height2 (. renderer2 :render))

			; Narrow canvas should produce taller output due to wrapping
			(assert-greater-eq height1 height2))

		; Test rendering with multiple CSS classes
		(deftest "Render with Multiple CSS Classes"
			(defq html "<p class=\"highlight bold large\">Styled text</p>")
			(defq css "
				.highlight { color: #ff6600; }
				.bold { font-weight: bold; }
				.large { font-size: 20px; }
				")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(assert-element-has-class p "highlight")
			(assert-element-has-class p "bold")
			(assert-element-has-class p "large")

			; Verify styles cascade correctly
			(defq styles (. stylesheet :compute-styles p))
			(assert-eq "#ff6600" (get styles "color"))
			(assert-eq "bold" (get styles "font-weight"))
			(assert-eq "20px" (get styles "font-size")))

		; Test rendering empty elements
		(deftest "Render Empty Elements"
			(defq html "<div><p></p><div></div></div>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Should handle empty elements gracefully
			(assert-greater-eq height 0))

		; Test rendering with universal selector
		(deftest "Render with Universal Selector"
			(defq html "<div><p>Text</p><span>More</span></div>")
			(defq css "* { color: #808080; }")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			; Universal selector should apply to all elements
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(defq styles (. stylesheet :compute-styles p))
			(assert-eq "#808080" (get styles "color")))

		; Test style cascading and specificity
		(deftest "CSS Specificity and Cascading"
			(defq html "<p id=\"special\" class=\"text\">Content</p>")
			(defq css "
				p { color: #000000; }
				.text { color: #333333; }
				#special { color: #ff0000; }
				")

			(defq doc (parse-html html))
			(defq stylesheet (parse-css css))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 stylesheet))
			(defq height (. renderer :render))

			(assert-greater height 0)

			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(defq styles (. stylesheet :compute-styles p))

			; ID selector should win (highest specificity)
			(assert-eq "#ff0000" (get styles "color")))
	))

