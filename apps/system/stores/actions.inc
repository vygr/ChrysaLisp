;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_to_right action-to-right
	+event_to_left action-to-left
	+event_delete action-delete
	+event_pin action-pin
	+event_unpin action-unpin
	+event_refresh action-refresh
	+event_skip action-skip
	+event_replace action-replace
	+event_both action-both
	+event_copies action-copies
	+event_cancel action-cancel
	+event_source_0 action-source-0
	+event_source_1 action-source-1
	+event_up_0 action-up-0
	+event_up_1 action-up-1
	+event_path_0 action-path-0
	+event_path_1 action-path-1
	+event_all_0 action-all-0
	+event_all_1 action-all-1))

;module
(export-symbols '(*event_map* action-row work-heard stores-start))
(env-pop)
