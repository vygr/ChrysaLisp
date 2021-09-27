;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-copy ()
	(bind '(ax ay) (. *edit* :get_anchor))
	(clipboard-put (. (. *edit* :get_buffer) :copy ax ay))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(when (> (length (defq lines (split data (ascii-char +char_lf)))) 0)
			(each! 0 -2 (# (action-insert %0) (action-break)) (list lines))
			(action-insert (pop lines))
			(if (ends-with (ascii-char +char_lf) data)
				(action-break)))))

(defun action-copy-word ()
	(action-select-word)
	(action-copy))

(defun action-copy-line ()
	(action-select-line)
	(action-copy))

(defun action-copy-paragraph ()
	(action-select-paragraph)
	(action-copy))
