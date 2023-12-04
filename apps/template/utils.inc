;;;;;;;
; utils
;;;;;;;

(defun select-panel (panel)
	(. panel :to_front))

(defun random-color ()
	(defq colors (list +argb_red +argb_green +argb_blue
		+argb_magenta +argb_cyan +argb_yellow))
	(elem-get (random (length colors)) colors))
