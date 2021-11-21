;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
event_map (fmap-kv
	+event_close action-close
	+event_reset action-reset
	+event_prev action-prev
	+event_next action-next
	+event_zoom_in action-zoom-in
	+event_zoom_out action-zoom-out
	+event_res_spinner action-res-spinner
	+event_vias_spinner action-vias-spinner
	+event_quant_spinner action-quant-spinner
	+event_flood_spinner action-flood-spinner
	+event_even_spinner action-even-spinner
	+event_odd_spinner action-odd-spinner
	+event_show_all action-show-all
	+event_show_1 action-show1
	+event_show_2 action-show2
	+event_show_3 action-show3
	+event_show_4 action-show4
	+event_mode_normal action-mode-normal
	+event_mode_gerber action-mode-gerber)

key_map (fmap-kv
	(ascii-code "a") action-mode-normal
	(ascii-code "g") action-mode-gerber
	(ascii-code "0") action-show-all
	(ascii-code "1") action-show1
	(ascii-code "2") action-show2
	(ascii-code "3") action-show3
	(ascii-code "4") action-show4
	(ascii-code "r") action-reset
	(ascii-code "n") action-next
	(ascii-code "p") action-prev)

key_map_shift (fmap-kv)

key_map_control (fmap-kv
	(ascii-code "=") action-zoom-in
	(ascii-code "-") action-zoom-out))

;module
(export-symbols
	event_map key_map key_map_shift key_map_control)
(env-pop)
