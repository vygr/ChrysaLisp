;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enhanced Designer Loader
; Loads existing apps for visual editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/lisp.inc")
(import "gui_designer/serialize.inc")
(import "lib/streams/string.inc")

(defun file-read-all (filepath)
	; (file-read-all filepath) -> str | :nil
	(when (defq stream (file-stream filepath))
		(defq contents "")
		(while (defq line (read-line stream))
			(setq contents (cat contents line (ascii-char 10))))
		contents))

(defun swap-gui-import (source)
	; (swap-gui-import source) -> source
	; Swaps gui/lisp.inc with gui_designer/lisp.inc
	(defq result "")
	(each (lambda (line)
		(cond
			((find "import \"gui/lisp.inc\"" line)
				(setq result (cat result "(import \"gui_designer/lisp.inc\")" (ascii-char 10))))
			(:t (setq result (cat result line (ascii-char 10))))))
		(split source (ascii-char 10)))
	result)

(defun restore-gui-import (source)
	; (restore-gui-import source) -> source
	(defq result "")
	(each (lambda (line)
		(cond
			((find "import \"gui_designer/lisp.inc\"" line)
				(setq result (cat result "(import \"gui/lisp.inc\")" (ascii-char 10))))
			(:t (setq result (cat result line (ascii-char 10))))))
		(split source (ascii-char 10)))
	result)

(defun extract-ui-section (source)
	; (extract-ui-section source) -> (ui-source start-line end-line)
	; Extracts the UI definition section
	(defq lines (split source (ascii-char 10))
		ui-start -1
		ui-end -1
		depth 0
		in-ui :nil
		ui-lines (list))

	(each! 0 -1 1 (lambda (line)
		(cond
			;Start of UI definition
			((and (not in-ui) (or (find "(ui-window" line) (find "(ui-root" line)))
				(setq in-ui :t ui-start (!) depth 1)
				(push ui-lines line))
			;In UI section
			(in-ui
				(push ui-lines line)
				;Count parentheses to find end
				(each (lambda (c)
					(cond
						((eql c "(") (++ depth))
						((eql c ")") (-- depth))))
					(str-to-list line))
				;End of UI section?
				(when (<= depth 0)
					(setq ui-end (!) in-ui :nil)))))
		lines)

	(list (apply cat (map (lambda (l) (cat l (ascii-char 10))) ui-lines))
		ui-start ui-end))

(defun load-app-for-designer (filepath)
	; (load-app-for-designer filepath) -> (tree original-source ui-section)
	; Loads an existing app and captures its UI tree

	(print "Loading app for designer: " filepath)

	;Read original source
	(unless (defq original-source (file-read-all filepath))
		(progn (print "Error: Could not read file") (return :nil)))

	;Reset designer state
	(designer-reset)

	;Swap imports
	(defq swapped-source (swap-gui-import original-source))

	;Write to temporary file
	(defq temp-file "/tmp/designer_temp.lisp")
	(when (defq stream (file-stream temp-file +file_open_write))
		(write stream swapped-source))

	;Load the modified file (this executes it with tracking)
	(print "Executing with designer tracking...")
	(import temp-file)

	;Get the captured tree
	(defq tree (designer-get-tree))

	(if tree
		(progn
			(print "✓ UI tree captured successfully!")
			(print "  Root: " (get :type tree))
			(print "  Children: " (str (length (get :children tree))))
			(list tree original-source swapped-source))
		(progn
			(print "✗ No UI tree captured - app may not use ui-* macros")
			:nil)))

(defun save-designer-changes (tree original-source filepath)
	; (save-designer-changes tree original-source filepath) -> :t | :nil
	; Saves the modified UI tree back to the app file

	(print "Saving changes to: " filepath)

	;Serialize the tree
	(defq new-ui (designer-serialize-tree tree))

	;Extract original UI section
	(bind '(old-ui ui-start ui-end) (extract-ui-section original-source))

	;Replace old UI with new UI
	(defq lines (split original-source (ascii-char 10))
		result ""
		line-num 0)

	(each (lambda (line)
		(cond
			;At UI start - insert new UI
			((= line-num ui-start)
				(setq result (cat result new-ui)))
			;Between UI start and end - skip (already replaced)
			((and (> line-num ui-start) (< line-num ui-end))
				:nil)
			;Other lines - keep as-is
			(:t (setq result (cat result line (ascii-char 10)))))
		(++ line-num))
		lines)

	;Ensure gui/lisp.inc (not gui_designer/lisp.inc)
	(setq result (restore-gui-import result))

	;Write to file
	(when (defq stream (file-stream filepath +file_open_write))
		(write stream result)
		(print "✓ Saved successfully!")
		:t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo: Load Calculator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-load-calculator ()
	; (demo-load-calculator) -> tree
	(print "")
	(print "╔════════════════════════════════════════════════════╗")
	(print "║  Loading Calculator App for Designer              ║")
	(print "╚════════════════════════════════════════════════════╝")
	(print "")

	;Load calculator
	(defq result (load-app-for-designer "apps/calculator/app.lisp"))

	(when result
		(bind '(tree original-source swapped-source) result)
		(print "")
		(print "Tree structure:")
		(print "  Type: " (get :type tree))
		(print "  Name: " (get :name tree))
		(print "  Children: " (str (length (get :children tree))))
		(print "")
		(print "You can now:")
		(print "  • Edit the tree")
		(print "  • Modify properties")
		(print "  • Add/remove widgets")
		(print "  • Save back to file")
		(print "")
		tree))
