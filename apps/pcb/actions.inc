;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_close action-close
	+event_reset action-reset
	+event_prev action-prev
	+event_next action-next
	+event_zoom_in action-scale-up
	+event_zoom_out action-scale-down
	+event_layer action-layer
	+event_mode action-mode)

*key_map* (Fmap-kv
	(ascii-code "a") action-mode-normal
	(ascii-code "g") action-mode-gerber
	(ascii-code "0") action-show-all
	(ascii-code "1") action-show-1
	(ascii-code "2") action-show-2
	(ascii-code "3") action-show-3
	(ascii-code "4") action-show-4
	(ascii-code "r") action-reset
	(ascii-code "n") action-next
	(ascii-code "p") action-prev)

*key_map_shift* (Fmap-kv)

*key_map_control* (Fmap-kv
	(ascii-code "{") action-scale-down
	(ascii-code "}") action-scale-up))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
