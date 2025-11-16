;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Loader - List-Based
; Load existing apps for editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-app-for-designer (filepath)
	; Load an existing app and capture its UI tree
	; Returns the tree or :nil if failed

	; Read the source file
	(defq source (file-read-all filepath))

	(when source
		; Swap the GUI import to use designer tracking
		(defq swapped (swap-gui-import source))

		; Write to temp file
		(defq temp-file "/tmp/designer_load.lisp")
		(file-write temp-file swapped)

		; Reset designer state
		(designer-reset)

		; Import the temp file (executes with tracking)
		(import temp-file)

		; Return the captured tree
		(designer-get-tree)))

(defun swap-gui-import (source)
	; Swap gui/lisp.inc with designer version
	(defq needle "(import \"gui/lisp.inc\")")
	(defq replacement "(import \"gui_designer/lisp_enhanced.inc\")")

	; Just do the replacement
	(str-replace source needle replacement))

(print "Designer loader (list-based) loaded")
