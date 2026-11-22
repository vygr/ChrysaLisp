;module
(env-push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the actions of the window's events and keys, ./app_impl.lisp has
; what each does. The space bar plays, or holds what plays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*event_map* (scatter (Fmap)
		+event_close action-close
		+event_prev action-prev
		+event_play action-play
		+event_pause action-pause
		+event_stop action-stop
		+event_next action-next
		+event_pin action-pin
		+event_open action-open
		+event_mode action-mode
		+event_shuffle action-shuffle
		+event_tab action-tab
		+event_up action-up
		+event_filter action-filter
		+event_search action-search
		+event_seek action-seek
		+event_item action-item)

	*key_map* (scatter (Fmap)
		(ascii-code " ") action-toggle)

	*key_map_shift* (scatter (Fmap))

	*key_map_control* (scatter (Fmap)))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
