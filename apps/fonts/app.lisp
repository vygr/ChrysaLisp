(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "service/clipboard/app.inc")

(enums +event 0
	(enum close)
	(enum prev next)
	(enum copy))

(enums +select 0
	(enum main tip))

(defun num-to-hex-str (_)
	(cat "0x" (short-to-hex-str _)))

(defun win-refresh (i)
	(defq ctf (elem-get fonts (setq index i)) font (create-font ctf 42) grid_width 8 grid_height 0
		ranges (font-glyph-ranges font) symbol_grid (Grid))
	(def *window_title* :text (cat "Fonts -> " (slice ctf (inc (find-rev "/" ctf)) -5)))
	(while (defq e (pop ranges) s (pop ranges))
		(defq s (logand s (neg grid_width)) e (align e grid_width) n (/ (- e s) grid_width))
		(setq grid_height (+ grid_height n))
		(each (lambda (c)
			(def (defq c (+ s (* c grid_width)) l (Label))
				:border 0
				:flow_flags (const (logior +flow_flag_right +flow_flag_align_vcenter))
				:font (const *env_small_terminal_font*)
				:text (num-to-hex-str c))
			(. symbol_grid :add_child l)
			(each (lambda (c)
					(def (. (defq l (Button)) :connect +event_copy)
						:border 1
						:flow_flags +flow_flag_align_hcenter
						:text (num-to-utf8 c)
						:tip_text (num-to-hex-str c))
					(. symbol_grid :add_child l))
				(range c (+ c grid_width)))) (range 0 n)))
	(def symbol_grid
		:grid_width (inc grid_width)
		:grid_height grid_height
		:color (const *env_toolbar_col*)
		:font font)
	(bind '(w h) (. symbol_grid :pref_size))
	(. symbol_grid :change 0 0 w h)
	(def *symbol_scroll* :min_width w :min_height (min h 720))
	(. *symbol_scroll* :add_child symbol_grid)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defq index 1 id :t fonts (sort (files-all "fonts/" '(".ctf"))))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (0xea19) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll *symbol_scroll* +scroll_flag_vertical))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("prev" "next")))

(defun main ()
	(defq select (alloc-select +select_size))
	(tooltips)
	(win-refresh index)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while id
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((<= +event_prev id +event_next)
				(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length fonts)) (length fonts))))
			((= +event_copy id)
				(clip-put-rpc (get :tip_text
					(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
			(:t (. *window* :event *msg*))))
	(free-select select)
	(gui-sub-rpc *window*))
