;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Loader
; Loads app files and swaps imports for designer mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/streams/string.inc")

(defun designer-load-file-contents (filepath)
	; (designer-load-file-contents filepath) -> str | :nil
	(when (defq stream (file-stream filepath))
		(defq contents "")
		(while (defq line (read-line stream))
			(setq contents (cat contents line (ascii-char 10))))
		contents))

(defun designer-swap-imports (source)
	; (designer-swap-imports source) -> source
	; Swaps (import "gui/lisp.inc") with (import "gui_designer/lisp.inc")
	(defq lines (split source (ascii-char 10))
		result "")
	(each (lambda (line)
		(cond
			;swap GUI import
			((find "import \"gui/lisp.inc\"" line)
				(setq result (cat result
					"(import \"gui_designer/lisp.inc\")"
					(ascii-char 10))))
			;keep other lines as-is
			(:t (setq result (cat result line (ascii-char 10))))))
		lines)
	result)

(defun designer-restore-imports (source)
	; (designer-restore-imports source) -> source
	; Restores (import "gui_designer/lisp.inc") back to (import "gui/lisp.inc")
	(defq lines (split source (ascii-char 10))
		result "")
	(each (lambda (line)
		(cond
			;restore GUI import
			((find "import \"gui_designer/lisp.inc\"" line)
				(setq result (cat result
					"(import \"gui/lisp.inc\")"
					(ascii-char 10))))
			;keep other lines as-is
			(:t (setq result (cat result line (ascii-char 10))))))
		lines)
	result)

(defun designer-extract-ui (source)
	; (designer-extract-ui source) -> ui-code
	; Extracts just the UI definition portions
	; This is a simplified version - a full implementation would parse more carefully
	(defq in-ui :nil
		ui-code ""
		lines (split source (ascii-char 10)))
	(each (lambda (line)
		(cond
			;start of UI definition
			((or (find "(ui-window" line)
				(find "(ui-root" line))
				(setq in-ui :t ui-code (cat ui-code line (ascii-char 10))))
			;in UI block
			(in-ui
				(setq ui-code (cat ui-code line (ascii-char 10)))
				;check for end of UI definition (heuristic)
				(when (and (find ")" line) (not (find "ui-" line)))
					;might be end of UI block
					:nil))
			;non-UI code
			(:t :nil)))
		lines)
	ui-code)

(defun designer-merge-ui (original-source new-ui)
	; (designer-merge-ui original-source new-ui) -> source
	; Merges new UI code back into original source
	; Preserves non-UI code (imports, enums, functions, etc.)
	(defq lines (split original-source (ascii-char 10))
		result ""
		in-ui :nil
		ui-replaced :nil)
	(each (lambda (line)
		(cond
			;start of old UI definition - skip it
			((and (not ui-replaced)
				(or (find "(ui-window" line) (find "(ui-root" line)))
				(setq in-ui :t)
				;insert new UI here
				(setq result (cat result new-ui (ascii-char 10)))
				(setq ui-replaced :t))
			;skip lines in old UI block
			(in-ui
				(when (and (find ")" line) (not (find "ui-" line)))
					(setq in-ui :nil)))
			;keep non-UI lines
			(:t (setq result (cat result line (ascii-char 10))))))
		lines)
	result)

(defun designer-load-app-for-editing (filepath)
	; (designer-load-app-for-editing filepath) -> (tree source original-source)
	(when (defq original-source (designer-load-file-contents filepath))
		(defq swapped-source (designer-swap-imports original-source))
		;reset designer state
		(designer-reset)
		;TODO: actually execute the swapped source to build the UI
		;For now, return the transformed source
		(list (designer-get-tree) swapped-source original-source)))

(defun designer-save-app (tree original-source filepath)
	; (designer-save-app tree original-source filepath) -> :t | :nil
	(when tree
		;serialize the UI tree
		(defq new-ui (designer-serialize-tree tree))
		;merge with original source
		(defq merged-source (designer-merge-ui original-source new-ui))
		;restore imports
		(defq final-source (designer-restore-imports merged-source))
		;write to file
		(when (defq stream (file-stream filepath +file_open_write))
			(write stream final-source)
			:t)))
