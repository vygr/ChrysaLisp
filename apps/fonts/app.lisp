(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum prev next))

(enums +select 0
	(enum main tip))

(defun num-to-hex-str (_)
	(cat "0x"
		(num-to-char (logand 0xf (>> _ 12)))
		(num-to-char (logand 0xf (>> _ 8)))
		(num-to-char (logand 0xf (>> _ 4)))
		(num-to-char (logand 0xf _))))

(defun win-refresh (i)
	(defq ctf (elem-get (setq index i) fonts) font (create-font ctf 42) grid_width 8 grid_height 0
		ranges (font-glyph-ranges font) symbol_grid (Grid))
	(def window_title :text (cat "Fonts -> " (slice (inc (find-rev "/" ctf)) -1 ctf)))
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
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun all-fonts (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".ctf" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq index 1 id t fonts (all-fonts "fonts/"))

(ui-window *window* ()
	(ui-title-bar window_title "" (0xea19) +event_close)
	(ui-tool-bar main_toolbar ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll symbol_scroll +scroll_flag_vertical))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("prev" "next")))

(defun main ()
	(defq select (alloc-select +select_size))
	(tooltips)
	(win-refresh index)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(while id
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id nil))
			((<= +event_prev id +event_next)
				(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length fonts)) (length fonts))))
			(t (. *window* :event *msg*))))
	(free-select select)
	(gui-sub *window*))
