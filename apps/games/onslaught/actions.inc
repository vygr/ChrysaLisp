;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught window action handlers & event map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-maximise ()
	; step zoom up to +zoom_max (3x)
	(when (< (defq zoom *zoom*) +zoom_max)
		(def *window* :zoom (setq *zoom* (inc zoom)))
		(window-resize)))

(defun action-minimise ()
	; step zoom down to +zoom_min (1x)
	(when (> (defq zoom *zoom*) +zoom_min)
		(def *window* :zoom (setq *zoom* (dec zoom)))
		(window-resize)))

(defq *event_map* (scatter (Fmap)
	+event_close action-close
	+event_max action-maximise
	+event_min action-minimise)
	*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))
