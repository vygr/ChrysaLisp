;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactive Designer Session
; Load this to get an interactive environment for designing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")
(import "gui_designer/save_with_diff.inc")
(import "gui_designer/serialize.inc")

;Global session state
(defq *session-tree* :nil)
(defq *session-original* :nil)
(defq *session-file* :nil)

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ChrysaLisp Designer - Interactive Session              ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Session Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-load (filepath)
	; (designer-load filepath) -> tree
	; Load an app for editing
	(print "Loading: " filepath)
	(defq result (load-app-for-designer filepath))
	(if result
		(progn
			(bind '(tree original swapped) result)
			(setq *session-tree* tree)
			(setq *session-original* original)
			(setq *session-file* filepath)
			(print "✓ Loaded " (str (count-tree tree)) " elements")
			tree)
		(progn
			(print "✗ Failed to load")
			:nil)))

(defun designer-show (&optional max-depth)
	; (designer-show [max-depth]) -> :nil
	; Show the current tree structure
	(setd max-depth 4)
	(if *session-tree*
		(progn
			(print "")
			(print "Current tree: " (get :name *session-tree*))
			(show-tree *session-tree* 0 max-depth)
			(print ""))
		(print "No tree loaded. Use (designer-load \"path/to/app.lisp\")")))

(defun designer-find (name)
	; (designer-find name) -> element | :nil
	; Find an element by name
	(defq result :nil)
	(when *session-tree*
		(designer-walk-tree *session-tree* (lambda (elem)
			(when (eql (get :name elem) name)
				(setq result elem)))))
	(if result
		(progn
			(print "Found: " (get :type result) " \"" (get :name result) "\"")
			(print "Props: " (str (first (get :props result))))
			result)
		(progn
			(print "Not found: " name)
			:nil)))

(defun designer-list-all ()
	; (designer-list-all) -> (element ...)
	; List all elements
	(defq elements (list))
	(when *session-tree*
		(designer-walk-tree *session-tree* (lambda (elem)
			(push elements (list (get :type elem) (get :name elem)))))
		(print "")
		(print "All elements (" (str (length elements)) "):")
		(each (lambda ((type name))
			(print "  " type " \"" name "\""))
			elements)
		(print ""))
	elements)

(defun designer-modify (name prop value)
	; (designer-modify name prop value) -> :t | :nil
	; Modify an element's property
	(defq elem (designer-find name))
	(if elem
		(progn
			(designer-set-property elem prop value)
			(print "✓ Set " prop " = " (str value))
			:t)
		:nil))

(defun designer-add (parent-name type child-name)
	; (designer-add parent-name type child-name) -> element
	; Add a new element
	(defq parent (designer-find parent-name))
	(if parent
		(progn
			(defq child (make-element type child-name))
			(designer-add-child parent child)
			(print "✓ Added " type " \"" child-name "\" to \"" parent-name "\"")
			child)
		:nil))

(defun designer-preview ()
	; (designer-preview) -> :nil
	; Preview the serialized code
	(if *session-tree*
		(progn
			(print "")
			(print "═══════════════════════════════════════════════════════════")
			(print (designer-serialize-tree *session-tree*))
			(print "═══════════════════════════════════════════════════════════")
			(print ""))
		(print "No tree loaded")))

(defun designer-save (&optional filepath)
	; (designer-save [filepath]) -> :t | :nil
	; Save the current tree
	(setd filepath (if *session-file*
		(cat *session-file* ".modified")
		"output.lisp"))
	(if (and *session-tree* *session-original*)
		(progn
			(save-with-diff-analysis *session-tree* *session-original* filepath :t)
			(print "")
			(print "✓ Saved to: " filepath)
			:t)
		(progn
			(print "No tree loaded")
			:nil)))

(defun designer-stats ()
	; (designer-stats) -> :nil
	; Show statistics
	(if *session-tree*
		(progn
			(print "")
			(print "Session Statistics:")
			(print "  File: " (ifn *session-file* "none"))
			(print "  Root: " (get :type *session-tree*) " \"" (get :name *session-tree*) "\"")
			(print "  Total elements: " (str (count-tree *session-tree*)))

			;Count by type
			(defq types (scatter (Lmap)))
			(designer-walk-tree *session-tree* (lambda (elem)
				(defq type (get :type elem))
				(defq count (ifn (. types :find type) 0))
				(. types :insert type (inc count))))

			(print "  Element types:")
			(each (lambda ((type count))
				(print "    " type ": " (str count)))
				(tolist types))
			(print ""))
		(print "No tree loaded")))

(defun designer-help ()
	; (designer-help) -> :nil
	; Show available commands
	(print "")
	(print "╔══════════════════════════════════════════════════════════╗")
	(print "║  Interactive Designer Commands                           ║")
	(print "╚══════════════════════════════════════════════════════════╝")
	(print "")
	(print "Loading:")
	(print "  (designer-load \"apps/calculator/app.lisp\")")
	(print "  (designer-show)        - Show tree structure")
	(print "  (designer-show 2)      - Show tree (max depth 2)")
	(print "")
	(print "Exploring:")
	(print "  (designer-find \"*display*\")  - Find element by name")
	(print "  (designer-list-all)           - List all elements")
	(print "  (designer-stats)              - Show statistics")
	(print "")
	(print "Editing:")
	(print "  (designer-modify \"*display*\" :text \"New Text\")")
	(print "  (designer-modify \"my_btn\" :color \"+argb_blue\")")
	(print "  (designer-add \"button_grid\" \"ui-button\" \"new_btn\")")
	(print "")
	(print "Saving:")
	(print "  (designer-preview)              - Preview serialized code")
	(print "  (designer-save)                 - Save to .modified file")
	(print "  (designer-save \"custom.lisp\")   - Save to custom file")
	(print "")
	(print "Examples:")
	(print "  ; Load calculator")
	(print "  (designer-load \"apps/calculator/app.lisp\")")
	(print "")
	(print "  ; Find and modify display")
	(print "  (defq disp (designer-find \"*display*\"))")
	(print "  (designer-modify \"*display*\" :color \"+argb_grey20\")")
	(print "")
	(print "  ; Add new button")
	(print "  (designer-add \"button_grid\" \"ui-button\" \"clr_btn\")")
	(print "  (designer-modify \"clr_btn\" :text \"CLR\")")
	(print "")
	(print "  ; Preview and save")
	(print "  (designer-preview)")
	(print "  (designer-save)")
	(print "")
	(print "Advanced:")
	(print "  ; Direct tree manipulation")
	(print "  (designer-walk-tree *session-tree* (lambda (elem)")
	(print "    (print (get :name elem))))")
	(print "")
	(print "  ; Manual serialization")
	(print "  (designer-serialize-tree *session-tree*)")
	(print ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper: count tree nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-tree (elem)
	(defq count 1)
	(each (lambda (child) (setq count (+ count (count-tree child))))
		(get :children elem))
	count)

(defun show-tree (elem indent max-depth)
	(when (<= indent max-depth)
		(defq spaces "")
		(times indent (setq spaces (cat spaces "  ")))
		(print spaces "├─ " (get :type elem) " \"" (get :name elem) "\"")
		(each (lambda (child) (show-tree child (inc indent) max-depth))
			(get :children elem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Auto-start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Interactive designer session ready!")
(print "")
(print "Quick start:")
(print "  (designer-help)                          - Show all commands")
(print "  (designer-load \"apps/calculator/app.lisp\") - Load calculator")
(print "")
