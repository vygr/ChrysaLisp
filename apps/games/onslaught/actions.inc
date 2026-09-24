;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Window Action Handlers & Event Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-maximise ()
	; 2x zoom: 640x480 viewport
	(setq *zoom_scale* +zoom_2x)
	(def *window* :zoom +zoom_2x)
	(bind '(x y) (. *window* :get_pos))
	(bind '(pref_w pref_h) (. *window* :pref_size))
	(defq title_h (- pref_h +game_height)
		new_w (* +game_width 2)
		new_h (+ (* +game_height 2) title_h))
	(bind '(fit_x fit_y fit_w fit_h) (view-fit x y new_w new_h))
	(. *window* :change_dirty fit_x fit_y fit_w fit_h))

(defun action-minimise ()
	; 1x zoom: 320x240 viewport
	(setq *zoom_scale* +zoom_1x)
	(def *window* :zoom +zoom_1x)
	(bind '(x y) (. *window* :get_pos))
	(bind '(pref_w pref_h) (. *window* :pref_size))
	(bind '(fit_x fit_y fit_w fit_h) (view-fit x y pref_w pref_h))
	(. *window* :change_dirty fit_x fit_y fit_w fit_h))

(defq *event_map* (scatter (Fmap)
	+event_close action-close
	+event_max action-maximise
	+event_min action-minimise))
