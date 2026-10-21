;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_up action-up
	+event_path action-path
	+event_ok action-ok
	+event_kinds action-kinds
	+event_filter action-filter))

;module
(export-symbols '(*event_map* action-item))
(env-pop)
