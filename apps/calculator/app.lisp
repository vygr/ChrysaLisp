;import ui settings
(run 'apps/ui.lisp)

(defq window (slot create_window nil window_flag_close)
	display (slot create_label nil)
	flow (slot create_flow nil)
	grid (slot create_grid nil))

(slot set_title window "Calculator")
(def-props flow 'flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth))
(def-props grid 'grid_width 4 'grid_height 4 'color 0xffffff00
	'font (slot create_font nil "fonts/OpenSans-Regular.ttf" 42))
(def-props display 'text "0" 'color 0xffffffff 'flow_flags flow_flag_align_hright
	'font (slot create_font nil "fonts/OpenSans-Regular.ttf" 24))
(each (lambda (_)
	(defq button (slot create_button nil))
	(def-props button 'text (if (eql _ "C") "AC" _)
		'flow_flags (bit-or flow_flag_align_hcenter flow_flag_align_vcenter))
	(slot connect_click button 1)
	(slot add_child grid button)) "789/456*123-0=C+")
(slot add_child window flow)
(slot add_child flow display)
(slot add_child flow grid)
(slot connect_close window 0)
(bind '(w h) (slot pref_size window))
(slot change window 920 48 w h)
(slot gui_add window)

(defq id t)
(while id
	(cond
		((ge (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) 1)
			nil)
		((eq id 0)
			(setq id nil))
		(t (slot event window msg))))
