;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo: Intelligent Save with Diff Analysis
; Shows how the enhanced save preserves comments, whitespace, and imperative code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")
(import "gui_designer/save_with_diff.inc")

(print "")
(print "═══════════════════════════════════════════════════════════")
(print "  Intelligent Save with Diff Analysis Demo")
(print "═══════════════════════════════════════════════════════════")
(print "")

;Create a test app with comments and mixed imperative code
(defq test-app-source "
(import \"././login/env.inc\")
(import \"gui/lisp.inc\")

;Event IDs
(enums +event 0
	(enum close)
	(enum button_click))

;State variable
(defq *counter* 0)

;Main window with comments
(ui-window *window* ()
	;Title bar - allows user to close
	(ui-title-bar _ \"Test App\" (0xea19) +event_close)

	;Main content area
	(ui-flow main_flow (:flow_flags +flow_down_fill)
		;Display label - shows current counter
		(ui-label counter_label (:text \"Counter: 0\"
			:min_width 200))

		;Button grid - 2x2 layout
		(ui-grid button_grid (:grid_width 2 :grid_height 2)
			;Row 1
			(ui-button inc_btn (:text \"+\"))
			(ui-button dec_btn (:text \"-\"))
			;Row 2
			(ui-button reset_btn (:text \"Reset\"))
			(ui-button quit_btn (:text \"Quit\")))))

;Connect events - imperative code mixed with UI
(. inc_btn :connect +event_button_click)
(. dec_btn :connect +event_button_click)

;Helper function
(defun update-counter (delta)
	(setq *counter* (+ *counter* delta))
	(def counter_label :text (cat \"Counter: \" (str *counter*))))

;Main loop
(defun main ()
	(print \"App started\"))
")

(print "Original app source:")
(print "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(print test-app-source)
(print "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(print "")

;Analyze the original
(defq comments (extract-comments test-app-source)
	imperative (extract-imperative-in-ui test-app-source)
	whitespace (extract-whitespace-pattern test-app-source))

(print "Original source analysis:")
(print "  Total comments: " (str (length comments)))
(print "  Imperative in UI: " (str (length imperative)))
(print "  Indent style: " (if (get :uses_tabs whitespace) "tabs"
	(if (get :uses_spaces whitespace) "spaces" "none")))
(print "")

;Save to temp file and load with designer
(defq temp-file "/tmp/test_app.lisp")
(when (defq stream (file-stream temp-file +file_open_write))
	(write stream test-app-source))

;Load with designer tracking
(print "Loading with designer tracking...")
(defq result (load-app-for-designer temp-file))

(if result
	(progn
		(bind '(tree original-source _) result)
		(print "✓ Loaded successfully")
		(print "")

		;Make some modifications
		(print "═══════════════════════════════════════════════════════════")
		(print "  Making Modifications to UI Tree")
		(print "═══════════════════════════════════════════════════════════")
		(print "")

		;Change counter label text
		(defq counter-label :nil)
		(designer-walk-tree tree (lambda (elem)
			(when (eql (get :name elem) "counter_label")
				(setq counter-label elem))))

		(when counter-label
			(print "Modifying counter label...")
			(designer-set-property counter-label :text "Counter: [MODIFIED]")
			(designer-set-property counter-label :min_width 250)
			(print "✓ Modified")
			(print ""))

		;Add a new button
		(defq button-grid :nil)
		(designer-walk-tree tree (lambda (elem)
			(when (eql (get :name elem) "button_grid")
				(setq button-grid elem))))

		(when button-grid
			(print "Adding new button to grid...")
			(defq new-btn (make-element "ui-button" "help_btn"))
			(designer-set-property new-btn :text "Help")
			(designer-add-child button-grid new-btn)
			(print "✓ Added help button")
			(print ""))

		;Save with diff analysis
		(print "═══════════════════════════════════════════════════════════")
		(print "  Saving with Diff Analysis")
		(print "═══════════════════════════════════════════════════════════")
		(print "")

		(defq output-file "/tmp/test_app_modified.lisp")
		(save-with-diff-analysis tree original-source output-file :t)

		;Read and display the saved file
		(when (defq saved-source (file-read-all output-file))
			(print "")
			(print "═══════════════════════════════════════════════════════════")
			(print "  Modified app source:")
			(print "═══════════════════════════════════════════════════════════")
			(print saved-source)
			(print "")

			;Analyze what was preserved
			(defq new-comments (extract-comments saved-source)
				new-imperative (extract-imperative-in-ui saved-source))

			(print "═══════════════════════════════════════════════════════════")
			(print "  Preservation Results")
			(print "═══════════════════════════════════════════════════════════")
			(print "")
			(print "Comments:")
			(print "  Original: " (str (length comments)))
			(print "  Preserved: " (str (length new-comments)))
			(print "  Status: " (if (>= (length new-comments) (length comments)) "✓ GOOD" "⚠ LOST"))
			(print "")
			(print "Imperative code in UI:")
			(print "  Original: " (str (length imperative)))
			(print "  Preserved: " (str (length new-imperative)))
			(print "  Status: " (if (>= (length new-imperative) (length imperative)) "✓ GOOD" "⚠ LOST"))
			(print "")
			(print "Functions preserved:")
			(print "  update-counter: " (if (find "update-counter" saved-source) "✓" "✗"))
			(print "  main: " (if (find "defun main" saved-source) "✓" "✗"))
			(print "")
			(print "State preserved:")
			(print "  *counter*: " (if (find "*counter*" saved-source) "✓" "✗"))
			(print "  +event enums: " (if (find "enums +event" saved-source) "✓" "✗"))
			(print ""))

		;Show LLM integration possibility
		(print "═══════════════════════════════════════════════════════════")
		(print "  Future: LLM-Assisted Diff Analysis")
		(print "═══════════════════════════════════════════════════════════")
		(print "")
		(defq llm-analysis (llm-analyze-diff original-source saved-source))
		(print "An LLM could analyze the diff and:")
		(each (lambda (item)
			(print "  • " item))
			(get :would_preserve llm-analysis))
		(print "")
		(print "And provide suggestions for:")
		(each (lambda (item)
			(print "  • " item))
			(get :would_suggest llm-analysis))
		(print ""))

	(print "✗ Failed to load app"))

(print "")
(print "═══════════════════════════════════════════════════════════")
(print "  Demo Complete!")
(print "═══════════════════════════════════════════════════════════")
(print "")
(print "Key takeaways:")
(print "  1. Comments are preserved during save")
(print "  2. Imperative code mixed with UI is retained")
(print "  3. Whitespace/formatting patterns maintained")
(print "  4. Diff analysis shows what changed")
(print "  5. LLM could further improve preservation")
(print "")
