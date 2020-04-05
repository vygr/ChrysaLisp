;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_prev 'win_next))

(defun-bind num-to-hex-str (_)
	(cat "0x"
		(num-to-char (logand 0xf (>> _ 12)))
		(num-to-char (logand 0xf (>> _ 8)))
		(num-to-char (logand 0xf (>> _ 4)))
		(num-to-char (logand 0xf _))))

(defun-bind win-refresh (i)
	(defq ctf (elem (setq index i) fonts) font (create-font ctf 42) grid_width 8 grid_height 0
		ranges (font-glyph-ranges font) symbol_grid (create-grid))
	(def fontname 'text ctf)
	(while (defq e (pop ranges) s (pop ranges))
		(defq s (logand s (neg grid_width)) e (align e grid_width) n (/ (- e s) grid_width))
		(setq grid_height (+ grid_height n))
		(each (lambda (c)
			(def (defq c (+ s (* c grid_width)) l (create-label)) 'flow_flags (const (logior flow_flag_right flow_flag_align_vcenter))
				'border 0 'font (const *env_small_terminal_font*) 'text (num-to-hex-str c))
			(view-add-child symbol_grid l)
			(each (lambda (c)
				(def (defq l (create-label)) 'border -1 'flow_flags flow_flag_align_hcenter 'text (num-to-utf8 c))
				(view-add-child symbol_grid l)) (range c (+ c grid_width)))) (range 0 n)))
	(def symbol_grid 'grid_width (inc grid_width) 'grid_height grid_height
		'color (const *env_toolbar_col*) 'font font)
	(bind '(w h) (view-pref-size symbol_grid))
	(view-change symbol_grid 0 0 w h)
	(def symbol_scroll 'min_width w 'min_height (min h 720))
	(view-add-child symbol_scroll symbol_grid)
	(apply view-change-dirty (cat (list window) (view-get-pos window) (view-pref-size window))))

(defq index 0 fonts '("fonts/Entypo.ctf" "fonts/OpenSans-Regular.ctf" "fonts/Hack-Regular.ctf"))

(ui-window window ()
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-title-flow _ "Fonts" (0xea19) (const event_win_close))
		(ui-flow _ ('flow_flags flow_right_fill 'color *env_toolbar_col* 'font *env_toolbar_font*)
			(ui-buttons (0xe91d 0xe91e) (const event_win_prev))
			(ui-label fontname ('font *env_window_font* 'border -1)))
		(ui-element symbol_scroll (create-scroll scroll_flag_vertical) ('color *env_slider_col*))))

(defun-bind main ()
	(win-refresh index)
	(gui-add (apply view-set-pos (cat (list window 200 48))))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			;close button
			nil)
		((= id event_win_next)
			(win-refresh (% (inc index) (length fonts))))
		((= id event_win_prev)
			(win-refresh (% (+ (dec index) (length fonts)) (length fonts))))
		(t (view-event window msg))))
	(view-hide window))
