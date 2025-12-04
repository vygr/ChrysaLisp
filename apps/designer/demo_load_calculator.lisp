;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo: Load Calculator for Editing
; Shows how to load an existing app (calculator) and make it editable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")

(print "")
(print "═══════════════════════════════════════════════════════════")
(print "  ChrysaLisp Designer - Load Calculator Demo")
(print "═══════════════════════════════════════════════════════════")
(print "")

;Load the calculator app with designer tracking
(defq calc-result (load-app-for-designer "apps/calculator/app.lisp"))

(if calc-result
	(progn
		(bind '(tree original-source swapped-source) calc-result)

		(print "")
		(print "═══════════════════════════════════════════════════════════")
		(print "  UI Tree Captured!")
		(print "═══════════════════════════════════════════════════════════")
		(print "")

		;Count all elements
		(defun count-tree (elem)
			(defq count 1)
			(each (lambda (child) (setq count (+ count (count-tree child))))
				(get :children elem))
			count)

		(print "Statistics:")
		(print "  Root type: " (get :type tree))
		(print "  Root name: " (get :name tree))
		(print "  Total elements: " (str (count-tree tree)))
		(print "")

		;Show tree structure
		(defun show-tree (elem indent)
			(defq spaces "")
			(times indent (setq spaces (cat spaces "  ")))
			(print spaces "├─ " (get :type elem) " " (get :name elem))
			(each (lambda (child) (show-tree child (inc indent)))
				(get :children elem)))

		(print "Tree Structure:")
		(show-tree tree 0)
		(print "")

		;Demo: Modify the tree
		(print "═══════════════════════════════════════════════════════════")
		(print "  Demo: Modifying the UI Tree")
		(print "═══════════════════════════════════════════════════════════")
		(print "")

		;Find the display label
		(defq display-elem :nil)
		(designer-walk-tree tree (lambda (elem)
			(when (and (eql (get :name elem) "*display*")
					(eql (get :type elem) "ui-label"))
				(setq display-elem elem))))

		(if display-elem
			(progn
				(print "Found display element: " (get :name display-elem))
				(print "  Current properties: " (str (first (get :props display-elem))))
				(print "")
				(print "Modifying text property...")
				(designer-set-property display-elem :text "Designer Modified!")
				(print "  New properties: " (str (first (get :props display-elem))))
				(print ""))
			(print "Display element not found"))

		;Serialize the modified tree
		(print "═══════════════════════════════════════════════════════════")
		(print "  Serialized UI Code (Modified)")
		(print "═══════════════════════════════════════════════════════════")
		(print "")
		(print (designer-serialize-tree tree))
		(print "")

		;Show how to save
		(print "═══════════════════════════════════════════════════════════")
		(print "  To Save Changes:")
		(print "═══════════════════════════════════════════════════════════")
		(print "")
		(print "  (save-designer-changes tree original-source \"apps/calculator/app_modified.lisp\")")
		(print "")
		(print "This would:")
		(print "  1. Serialize the modified UI tree")
		(print "  2. Replace the old UI section in the original source")
		(print "  3. Restore import to gui/lisp.inc (not gui_designer/lisp.inc)")
		(print "  4. Preserve all imperative code (functions, state, main loop)")
		(print "  5. Write to the new file")
		(print "")

		(print "═══════════════════════════════════════════════════════════")
		(print "  Success! Calculator is now editable in designer mode.")
		(print "═══════════════════════════════════════════════════════════")
		(print ""))

	(progn
		(print "Failed to load calculator")
		(print "Make sure apps/calculator/app.lisp exists")))
