;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_button))

(defq id t keys (list) vals (list) vdu_height 40)

(defun populate-help ()
	(defq state t vdu_width 1 k (list) v (list))
	(each-line (lambda (_)
		(defq s (split _ " ") f (elem 0 s))
		(cond
			(state (cond
				((eql f "###") (push k (sym (elem 1 s))) (push v ""))
				((eql f "```") (setq state nil))))
			((eql f "```") (setq state t))
			(t (elem-set -2 v (cat (elem -2 v) _ (char 10)))))) "doc/CLASSES.md")
	(each (lambda (k v)
		(when (ne 0 (length v))
			(defq _ (split k ":"))
			(cond
				((and (ge (length (elem 0 _)) 4) (eql "lisp" (slice 0 4 (elem 0 _)))))
				((and (ge (length (elem 1 _)) 5) (eql "lisp_" (slice 0 5 (elem 1 _)))))
				(t (push keys k) (push vals v))))) k v)
	(each (lambda (_)
		(def (defq b (create-button)) 'text _ 'border 0
			'flow_flags (bit-or flow_flag_align_vcenter flow_flag_align_hleft))
		(view-add-child index (button-connect-click b event_win_button))) keys)
	(def vdu 'vdu_width
		(reduce max (map (lambda (_)
			(reduce max (map length (split _ (char 10))))) vals))))

(ui-tree window (create-window window_flag_close) ('color argb_black)
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh flow_flag_lastw)
		'font (create-font "fonts/Hack-Regular.ttf" 16))
		(ui-element index_scroll (create-scroll scroll_flag_vertical) ('color argb_green)
			(ui-element index (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw)
				'color argb_white)))
		(ui-element vdu (create-vdu) ('vdu_height vdu_height 'ink_color argb_cyan))))

(populate-help)
(bind '(w h) (view-pref-size index))
(view-change index 0 0 w h)
(def index_scroll 'min_width w)
(window-set-title window "Help")
(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 32 32 w h))

(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((eq id event_win_button)
			(defq _ (find (sym (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text)) keys))
			(when _
				(vdu-print vdu (char 10))
				(vdu-print vdu "----------------------") (vdu-print vdu (char 10))
				(vdu-print vdu (elem _ keys)) (vdu-print vdu (char 10))
				(vdu-print vdu "----------------------") (vdu-print vdu (char 10))
				(vdu-print vdu (elem _ vals))
				(vdu-print vdu "----------------------") (vdu-print vdu (char 10))))
		(t (view-event window msg))))
