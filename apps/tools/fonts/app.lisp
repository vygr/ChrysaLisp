(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "service/clipboard/app.inc")
(import "apps/system/files/picker.inc")

(enums +event +event_user
	(enum open)
	(enum copy))

(enums +select 0
	(enum main tip picker))

(defun num-to-hex-str (%0)
	(cat "0x" (short-to-hex-str %0)))

(defun win-refresh (ctf)
	;a font of the host's, (create-font) reads it, so files only
	(defq font (create-font ctf 42) grid_width 8 grid_height 0
		ranges (reverse (font-glyph-ranges font)) symbol_grid (Grid)
		drawn_rows (list))
	(setq *file* ctf)
	(def *window_title* :text (cat "Fonts -> " (slice ctf (ifn (rfind "/" ctf) 0) -5)))
	(while (defq s (pop ranges) e (pop ranges))
		(defq s (logand s (neg grid_width)) e (align e grid_width) n (/ (- e s) grid_width))
		(each (lambda (row_idx)
			(defq c (+ s (* row_idx grid_width)))
			(unless (find c drawn_rows)
				(push drawn_rows c)
				(setq grid_height (inc grid_height))
				(def (defq l (Label))
					:border 0
					:flow_flags (const (logior +flow_flag_right +flow_flag_align_vcenter))
					:text (num-to-hex-str c))
				(view-variant l :font :font_small_terminal)
				(. symbol_grid :add_child l)
				(each (lambda (char_code)
						(def (. (defq l (Button)) :connect +event_copy)
							:border 1
							:flow_flags +flow_flag_align_hcenter
							:text (num-to-utf8 char_code)
							:tip_text (num-to-hex-str char_code))
						(. symbol_grid :add_child l))
					(range c (+ c grid_width))))) (range 0 n)))
	(def symbol_grid
		:grid_width (inc grid_width)
		:grid_height grid_height
		:font font)
	(view-variant symbol_grid :paper :paper_toolbar)
	(bind '(w h) (. symbol_grid :pref_size))
	(. symbol_grid :change 0 0 w h)
	(def *symbol_scroll* :min_width w :min_height (min h 720))
	(. *symbol_scroll* :add_child symbol_grid)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

;the font shown, opened by the file picker, which keeps where fonts were
;taken from; the second of the tree's to start
(defq id :t *file* (second (sort (files-all "fonts/" '(".ctf")))) *picker* :nil)

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (+sym_open) +event_open))
	(ui-scroll *symbol_scroll* +scroll_flag_vertical))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("open")))

(defun main ()
	(defq select (task-mboxes +select_size))
	(tooltips)
	(win-refresh *file*)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while id
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_picker)
				;the font taken, or "" none
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "")
					(win-refresh file)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((= id +event_open)
				;the picker, at the font shown, the one open told to go first
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get select +select_picker) "Open font..." *file* ".ctf" :nil "fonts" :nil '("fonts"))))
			((= +event_copy id)
				(clip-put-rpc (to-lower (get :tip_text
					(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))))
			((. *window* :event *msg*))))
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(gui-sub-rpc *window*))
