;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defun-bind to-utf8 (_)
	(cat (ascii-char (+ 0xe0 (logand (>> _ 12) 0x3f)))
		(ascii-char (+ 0x80 (logand (>> _ 6) 0x3f)))
		(ascii-char (+ 0x80 (logand _ 0x3f)))))

(defun-bind to-hex (_)
	(cat "0x"
		(to-base-char (logand 0xf (>> _ 12)))
		(to-base-char (logand 0xf (>> _ 8)))
		(to-base-char (logand 0xf (>> _ 4)))
		(to-base-char (logand 0xf _))))

(defq id t range_start 0xe900 range_end 0xea50
	grid_width 6 grid_height (/ (- range_end range_start) grid_width))

(ui-tree window (create-window window_flag_close) nil
	(ui-element symbol_scroll (create-scroll scroll_flag_vertical)
			('color slider_col 'font (create-font "fonts/Entypo.ctf" 42))
		(ui-element symbol_grid (create-grid) ('grid_width (inc grid_width) 'grid_height grid_height
				'color argb_white)
			(each (lambda (c)
				(setq c (+ range_start (* c grid_width)))
				(ui-element _ (create-label) ('font (create-font "fonts/Hack-Regular.ctf" 12) 'text (to-hex c)))
				(each (lambda (c)
					(ui-element _ (create-label) ('flow_flags flow_flag_align_hcenter 'text (to-utf8 c))))
						(range c (+ c grid_width)))) (range 0 grid_height)))))

(bind '(w h) (view-pref-size symbol_grid))
(def symbol_scroll 'min_width w 'min_height 640)
(view-change symbol_grid 0 0 w h)
(gui-add (apply view-change (cat (list window 200 48)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Entypo")))))

(while id
	(cond
		((>= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			;close button
			(setq id nil))
		(t (view-event window msg))))

(view-hide window)
