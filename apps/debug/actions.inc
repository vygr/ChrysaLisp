;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_close action-close
	+event_hvalue action-hvalue
	+event_clear action-clear
	+event_clear_all action-clear-all
	+event_play action-play
	+event_forward action-forward
	+event_pause action-pause
	+event_step action-step
	+event_play_all action-play-all
	+event_forward_all action-forward-all
	+event_pause_all action-pause-all
	+event_step_all action-step-all)

*key_map* (Fmap-kv
	(ascii-code "p") action-play
	(ascii-code "f") action-forward
	(ascii-code "s") action-step
	(ascii-code "h") action-pause
	(ascii-code "x") action-clear)

*key_map_shift* (Fmap-kv
	(ascii-code "P") action-play-all
	(ascii-code "F") action-forward-all
	(ascii-code "S") action-step-all
	(ascii-code "H") action-pause-all
	(ascii-code "X") action-clear-all)

*key_map_control* (Fmap-kv))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
