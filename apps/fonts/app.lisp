;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_next 'win_prev))

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
			(defq c (+ s (* c grid_width)) l (create-label))
			(def l 'font (create-font "fonts/Hack-Regular.ctf" 12) 'text (num-to-hex-str c))
			(view-add-child symbol_grid l)
			(each (lambda (c)
				(def (defq l (create-label)) 'border -1 'flow_flags flow_flag_align_hcenter 'text (num-to-utf8 c))
				(view-add-child symbol_grid l)) (range c (+ c grid_width)))) (range 0 n)))
	(def symbol_grid 'grid_width (inc grid_width) 'grid_height grid_height
		'color (const toolbar_col) 'font font)
	(bind '(w h) (view-pref-size symbol_grid))
	(view-change symbol_grid 0 0 w h)
	(def symbol_scroll 'min_width w 'min_height (min h 720))
	(view-add-child symbol_scroll symbol_grid)
	(bind '(x y) (view-get-pos window))
	(bind '(w h) (view-pref-size window))
	(view-layout fontname)
	(view-change-dirty window x y w h))

(defq id t index 0 fonts '("fonts/Entypo.ctf" "fonts/OpenSans-Regular.ctf" "fonts/Hack-Regular.ctf"))

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh flow_flag_lastw)
			'color toolbar_col 'font (create-font "fonts/Entypo.ctf" 32))
		(component-connect (ui-element _ (create-button) ('text (num-to-utf8 0xe91d))) event_win_prev)
		(component-connect (ui-element _ (create-button) ('text (num-to-utf8 0xe91e))) event_win_next)
		(ui-element fontname (create-label) ('font (create-font "fonts/OpenSans-Regular.ctf" 18) 'border -1)))
	(ui-element symbol_scroll (create-scroll scroll_flag_vertical) ('color slider_col)))

(win-refresh index)
(gui-add (apply view-change (cat (list window 200 48)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Fonts")))))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			;close button
			(setq id nil))
		((= id event_win_next)
			(win-refresh (% (inc index) (length fonts))))
		((= id event_win_prev)
			(win-refresh (% (+ (dec index) (length fonts)) (length fonts))))
		(t (view-event window msg))))

(view-hide window)
