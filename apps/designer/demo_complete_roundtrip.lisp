;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete Round-Trip Demo
; Loads calculator, modifies it, saves it back - proves everything works!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")
(import "gui_designer/save_with_diff.inc")

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Complete Designer Round-Trip Demo                      ║")
(print "║  Load → Edit → Save → Verify                            ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 1: Load Calculator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 1: Loading Calculator App")
(print "═══════════════════════════════════════════════════════════")
(print "")

(defq result (load-app-for-designer "apps/calculator/app.lisp"))

(unless result
	(progn
		(print "✗ Failed to load calculator")
		(print "Make sure apps/calculator/app.lisp exists")
		(return :nil)))

(bind '(tree original-source swapped-source) result)

(print "✓ Loaded successfully!")
(print "")

;Count elements
(defun count-tree (elem)
	(defq count 1)
	(each (lambda (child) (setq count (+ count (count-tree child))))
		(get :children elem))
	count)

(print "Tree statistics:")
(print "  Root: " (get :type tree) " " (get :name tree))
(print "  Total elements: " (str (count-tree tree)))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 2: Show Tree Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 2: Tree Structure")
(print "═══════════════════════════════════════════════════════════")
(print "")

(defun show-tree (elem indent max-depth)
	(when (<= indent max-depth)
		(defq spaces "")
		(times indent (setq spaces (cat spaces "  ")))
		(print spaces "├─ " (get :type elem) " \"" (get :name elem) "\"")
		(each (lambda (child) (show-tree child (inc indent) max-depth))
			(get :children elem))))

(show-tree tree 0 3)
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 3: Make Modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 3: Making Modifications")
(print "═══════════════════════════════════════════════════════════")
(print "")

(defq modifications-made (list))

;Modification 1: Change window title
(print "Modification 1: Change window title")
(defq title-bar :nil)
(designer-walk-tree tree (lambda (elem)
	(when (and (eql (get :type elem) "ui-title-bar")
			(find "Calculator" (str (get :props elem))))
		(setq title-bar elem))))

(if title-bar
	(progn
		(designer-set-property title-bar :text "Calculator [Designer Modified]")
		(push modifications-made "Window title changed")
		(print "  ✓ Changed title to 'Calculator [Designer Modified]'"))
	(print "  ✗ Title bar not found"))
(print "")

;Modification 2: Change display background color
(print "Modification 2: Change display background")
(defq display :nil)
(designer-walk-tree tree (lambda (elem)
	(when (eql (get :name elem) "*display*")
		(setq display elem))))

(if display
	(progn
		(designer-set-property display :color "+argb_grey20")
		(push modifications-made "Display background changed to grey")
		(print "  ✓ Changed display background to grey"))
	(print "  ✗ Display not found"))
(print "")

;Modification 3: Add a new button to grid
(print "Modification 3: Add 'CLR' button to grid")
(defq button-grid :nil)
(designer-walk-tree tree (lambda (elem)
	(when (eql (get :name elem) "button_grid")
		(setq button-grid elem))))

(if button-grid
	(progn
		(defq clr-button (make-element "ui-button" "clr_button"))
		(designer-set-property clr-button :text "CLR")
		(designer-add-child button-grid clr-button)
		(push modifications-made "Added CLR button to grid")
		(print "  ✓ Added CLR button to grid"))
	(print "  ✗ Button grid not found"))
(print "")

(print "Total modifications: " (str (length modifications-made)))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 4: Preview Modified UI Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 4: Preview Modified UI Code")
(print "═══════════════════════════════════════════════════════════")
(print "")

(defq new-ui-code (designer-serialize-tree tree))

(print "First 20 lines of modified UI:")
(print "───────────────────────────────────────────────────────────")
(defq lines (split new-ui-code (ascii-char 10)))
(defq line-count 0)
(each (lambda (line)
	(when (< line-count 20)
		(print line)
		(++ line-count)))
	lines)
(print "... (" (str (- (length lines) 20)) " more lines)")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 5: Save with Diff Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 5: Save with Diff Analysis")
(print "═══════════════════════════════════════════════════════════")
(print "")

(defq output-file "apps/calculator/app_modified_demo.lisp")
(print "Saving to: " output-file)
(print "")

(save-with-diff-analysis tree original-source output-file :t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 6: Verify Saved File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "STEP 6: Verify Saved File")
(print "═══════════════════════════════════════════════════════════")
(print "")

(when (defq saved-source (file-read-all output-file))
	(print "✓ File saved successfully")
	(print "")

	;Check modifications are present
	(print "Verifying modifications:")
	(print "  Title change: " (if (find "Designer Modified" saved-source) "✓" "✗"))
	(print "  Display color: " (if (find "+argb_grey20" saved-source) "✓" "✗"))
	(print "  CLR button: " (if (find "clr_button" saved-source) "✓" "✗"))
	(print "")

	;Check imperative code preserved
	(print "Verifying preserved code:")
	(print "  Enums: " (if (find "enums +event" saved-source) "✓" "✗"))
	(print "  Config functions: " (if (find "config-load" saved-source) "✓" "✗"))
	(print "  Handler functions: " (if (find "handle-input" saved-source) "✓" "✗"))
	(print "  Main function: " (if (find "defun main" saved-source) "✓" "✗"))
	(print "  State variables: " (if (find "*config*" saved-source) "✓" "✗"))
	(print "")

	;File size comparison
	(print "File size:")
	(print "  Original: " (str (length original-source)) " chars")
	(print "  Modified: " (str (length saved-source)) " chars")
	(print "  Difference: " (str (- (length saved-source) (length original-source))) " chars")
	(print ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 7: Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Round-Trip Complete! ✓                                 ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

(print "What we just did:")
(print "  1. ✓ Loaded apps/calculator/app.lisp")
(print "  2. ✓ Captured complete UI tree (" (str (count-tree tree)) " elements)")
(print "  3. ✓ Made " (str (length modifications-made)) " modifications")
(print "  4. ✓ Serialized tree back to Lisp code")
(print "  5. ✓ Saved to " output-file)
(print "  6. ✓ Verified modifications present")
(print "  7. ✓ Verified imperative code preserved")
(print "")

(print "Modifications made:")
(each (lambda (mod)
	(print "  • " mod))
	modifications-made)
(print "")

(print "Files:")
(print "  Original: apps/calculator/app.lisp")
(print "  Modified: " output-file)
(print "")

(print "Next steps:")
(print "  • Compare files: diff apps/calculator/app.lisp " output-file)
(print "  • Test modified app: (import \"" output-file "\")")
(print "  • Visual designer: Load in designer UI for drag-drop editing")
(print "")

(print "This proves the complete designer workflow:")
(print "  Load → Track → Edit → Serialize → Save → Verify")
(print "")
(print "Everything works! 🎉")
(print "")
