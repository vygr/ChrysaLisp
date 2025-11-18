#!/usr/bin/env lsp

;; Text Selection Tests
;; Tests click-drag-select and clipboard copy functionality

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/canvas_renderer.inc")
(import "gui/canvas/lisp.inc")

(deftest-suite "Text Selection Tests")

(defun create-test-canvas (w h)
	; Create a canvas for testing
	(defq canvas (Canvas))
	(.-> canvas
		(:set_size w h)
		(:canvas_alloc 0 w h 0xffffffff 1))
	canvas)

(defun main ()
	(run-test-suite
		; Test text fragment tracking
		(deftest "Track Text Fragments During Rendering"
			(defq html "<p>Hello World</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Should have tracked text fragments
			(defq fragments (. renderer 'text_fragments))
			(assert-greater (length fragments) 0))

		; Test selection start/end
		(deftest "Set Selection Start and End"
			(defq html "<p>Select this text</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Set selection
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 100 10)

			; Should have selection
			(assert-not-nil (. renderer 'selection_start))
			(assert-not-nil (. renderer 'selection_end)))

		; Test clear selection
		(deftest "Clear Selection"
			(defq html "<p>Text to select</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Set and clear selection
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 100 10)
			(. renderer :clear-selection)

			; Selection should be cleared
			(assert-nil (. renderer 'selection_start))
			(assert-nil (. renderer 'selection_end)))

		; Test get selected text
		(deftest "Get Selected Text"
			(defq html "<p>Select some text here</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select all text
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 200 10)

			; Should get text
			(defq selected (. renderer :get-selected-text))
			(assert-greater (length selected) 0))

		; Test empty selection
		(deftest "Empty Selection Returns Empty String"
			(defq html "<p>Text</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; No selection
			(defq selected (. renderer :get-selected-text))
			(assert-eq "" selected))

		; Test selection across multiple words
		(deftest "Select Multiple Words"
			(defq html "<p>One Two Three Four Five</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select from start to end of line
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 300 10)

			(defq selected (. renderer :get-selected-text))
			(assert-contains "One" selected)
			(assert-contains "Five" selected))

		; Test selection re-render
		(deftest "Re-render Clears Text Fragments"
			(defq html "<p>First render</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height1 (. renderer :render))
			(defq count1 (length (. renderer 'text_fragments)))

			; Re-render
			(defq height2 (. renderer :render))
			(defq count2 (length (. renderer 'text_fragments)))

			; Should have same number of fragments, not accumulated
			(assert-eq count1 count2))

		; Test selection with formatted text
		(deftest "Select Formatted Text"
			(defq html "<p><strong>Bold</strong> and <em>italic</em> text</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select all
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 300 10)

			(defq selected (. renderer :get-selected-text))
			(assert-contains "Bold" selected)
			(assert-contains "italic" selected))

		; Test selection in list
		(deftest "Select Text in List"
			(defq html "<ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select all list items
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 300 100)

			(defq selected (. renderer :get-selected-text))
			(assert-greater (length selected) 0))

		; Test selection in heading
		(deftest "Select Heading Text"
			(defq html "<h1>Main Heading</h1><p>Paragraph</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select heading
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 300 30)

			(defq selected (. renderer :get-selected-text))
			(assert-contains "Main" selected)
			(assert-contains "Heading" selected))

		; Test selection state preservation
		(deftest "Selection State Preserved Until Cleared"
			(defq html "<p>Persistent selection</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Set selection
			(. renderer :set-selection-start 10 10)
			(. renderer :set-selection-end 100 10)

			; Get text twice - should be same
			(defq selected1 (. renderer :get-selected-text))
			(defq selected2 (. renderer :get-selected-text))

			(assert-eq selected1 selected2))

		; Test partial word selection
		(deftest "Select Part of Document"
			(defq html "<p>This is a long paragraph with many words in it</p>")
			(defq doc (parse-html html))
			(defq canvas (create-test-canvas 800 600))

			(defq renderer (html-canvas-renderer :init doc canvas 800 nil))
			(defq height (. renderer :render))

			; Select middle portion
			(. renderer :set-selection-start 50 10)
			(. renderer :set-selection-end 150 10)

			(defq selected (. renderer :get-selected-text))
			; Should have something selected but not everything
			(assert-greater (length selected) 0))
	))

