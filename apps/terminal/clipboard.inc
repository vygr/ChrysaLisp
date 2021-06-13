;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-copy ()
	(clipboard-put (. *current_buffer* :copy *anchor_x* *anchor_y*))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(cond
			(*pipe*
				;paste data to active pipe
				(. *pipe* :write data)
				(action-insert data))
			((> (length (defq lines (split data (ascii-char +char_lf)))) 0)
				;paste first line to input
				(action-insert (elem 0 lines))))))

(defun action-copy-word ()
	(action-select-word)
	(action-copy))

(defun action-copy-line ()
	(action-select-line)
	(action-copy))

(defun action-copy-paragraph ()
	(action-select-paragraph)
	(action-copy))
