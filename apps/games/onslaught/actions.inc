;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/actions.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught window action handlers & event map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(when (/= *zoom* +zoom_1x)
		(setq *zoom* +zoom_1x)
		(def *window* :zoom +zoom_1x)
		(window-resize)))

(defun action-maximise ()
	(when (/= *zoom* +zoom_2x)
		(setq *zoom* +zoom_2x)
		(def *window* :zoom +zoom_2x)
		(window-resize)))

(defq *event_map* (scatter (Fmap)
	+event_close action-close
	+event_max action-maximise
	+event_min action-minimise)
	*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))