(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum prev next))

(defun num-to-hex-str (_)
	(cat "0x"
		(num-to-char (logand 0xf (>> _ 12)))
		(num-to-char (logand 0xf (>> _ 8)))
		(num-to-char (logand 0xf (>> _ 4)))
		(num-to-char (logand 0xf _))))

(defun win-refresh (i)
	(defq ctf (elem (setq index i) fonts) font (create-font ctf 42) grid_width 8 grid_height 0
		ranges (font-glyph-ranges font) symbol_grid (Grid))
	(def fontname :text ctf)
	(while (defq e (pop ranges) s (pop ranges))
		(defq s (logand s (neg grid_width)) e (align e grid_width) n (/ (- e s) grid_width))
		(setq grid_height (+ grid_height n))
		(each (lambda (c)
			(def (defq c (+ s (* c grid_width)) l (Label)) :flow_flags  (const (logior +flow_flag_right +flow_flag_align_vcenter))
				:border 0 :font (const *env_small_terminal_font*) :text (num-to-hex-str c))
			(. symbol_grid :add_child l)
			(each (lambda (c)
				(def (defq l (Label)) :border -1 :flow_flags  +flow_flag_align_hcenter :text (num-to-utf8 c))
				(. symbol_grid :add_child l)) (range c (+ c grid_width)))) (range 0 n)))
	(def symbol_grid :grid_width (inc grid_width) :grid_height grid_height
		:color (const *env_toolbar_col*) :font font)
	(bind '(w h) (. symbol_grid :pref_size))
	(. symbol_grid :change 0 0 w h)
	(def symbol_scroll :min_width w :min_height (min h 720))
	(. symbol_scroll :add_child symbol_grid)
	(bind '(x y w h) (apply view-fit (cat (. mywindow :get_pos) (. mywindow :pref_size))))
	(. mywindow :change_dirty x y w h))

(defun all-fonts (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".ctf" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq index 1 fonts (all-fonts "fonts/"))

(ui-window mywindow ()
	(ui-title-bar _ "Fonts" (0xea19) +event_close)
	(ui-tool-bar _ (:flow_flags +flow_right_fill)
		(ui-buttons (0xe91d 0xe91e) +event_prev)
		(ui-label fontname (:font *env_window_font* :border -1)))
	(ui-scroll symbol_scroll +scroll_flag_vertical))

(defun main ()
	(win-refresh index)
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(while (cond
		((= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			;close button
			nil)
		((<= +event_prev id +event_next)
			(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length fonts)) (length fonts))))
		(t (. mywindow :event msg))))
	(. mywindow :hide))
