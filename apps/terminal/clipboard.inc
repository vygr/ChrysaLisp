;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-copy ()
	(clipboard-put (. *current_buffer* :copy *anchor_x* *anchor_y*))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(when *pipe*
			;paste data to active pipe
			(each (# (defq line (cat %0 (ascii-char +char_lf)))
					(action-insert line)
					(. *pipe* :write line))
				(split data (ascii-char +char_lf))))
		(clear-selection) (refresh)))

(defun action-copy-word ()
	(action-select-word)
	(action-copy))

(defun action-copy-line ()
	(action-select-line)
	(action-copy))

(defun action-copy-paragraph ()
	(action-select-paragraph)
	(action-copy))
