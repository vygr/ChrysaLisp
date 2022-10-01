;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(import "gui/label/lisp.inc")
(import "gui/font/lisp.inc")
(import "lib/consts/chars.inc")
(import "apps/clipboard/app.inc")

(defclass Textfield () (Label)
	; (Textfield) -> textfield
	(def this :cursor 0 :anchor 0 :clear_text "" :hint_text "" :text ""
		:mode :nil :state 1)

	(defmethod :draw (this)
		; (. textfield :draw) -> textfield
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(raise :font :text :cursor :anchor)
		(when (and font text)
			(bind '(tw th) (font-glyph-bounds font (slice 0 cursor text)))
			(when (/= cursor anchor)
				(bind '(sw _) (font-glyph-bounds font (slice 0 (min anchor cursor) text)))
				(bind '(sw1 _) (font-glyph-bounds font (slice 0 (max anchor cursor) text)))
				(.-> this
					(:ctx_set_color (get :hint_color this))
					(:ctx_filled_box sw (>>> (- h th) 1) (- sw1 sw) th)))
			(.-> this
				(:ctx_set_color (get :ink_color this))
				(:ctx_filled_box tw (>>> (- h th) 1) 2 th)))
		this)

	(defmethod :layout (this)
		; (. textfield :layout) -> textfield
		(cond
			((eql (defq text (get :clear_text this)) "")
				(defq text (get :hint_text this) mode :nil)
				(def this :ink_color (get :hint_color this)))
			(:t  (defq mode (get :mode this))
				(def this :ink_color (get :no_hint_color this))))
		(if mode
			(def this :text (pad "" (length text) "******************"))
			(def this :text text))
		(.super this :layout))

	(defmethod :key_down (this event)
		; (. textfield :key_down event) -> textfield
		(raise :clear_text :cursor :anchor :mode)
		(defq key (getf event +ev_msg_key_key) mod (getf event +ev_msg_key_mod)
			cursor (min cursor (length clear_text)))
		(cond
			((/= 0 (logand mod (const (+ +ev_key_mod_control +ev_key_mod_command))))
				;control/command key
				(cond
					((= key (ascii-code "a"))
						;select all
						(setq anchor 0 cursor (length clear_text)))
					((= key (ascii-code "x"))
						;cut
						(unless (or mode (= cursor anchor))
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(clipboard-put (slice sx sx1 clear_text))
							(setq clear_text (erase clear_text sx sx1)
								cursor sx anchor cursor)))
					((= key (ascii-code "c"))
						;copy
						(unless (or mode (= cursor anchor))
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(clipboard-put (slice sx sx1 clear_text))))
					((= key (ascii-code "v"))
						;paste
						(when (/= cursor anchor)
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(setq clear_text (erase clear_text sx sx1)
								cursor sx anchor cursor))
						(unless (eql (defq data (clipboard-get)) "")
							(unless (defq lf (find (ascii-char +char_lf) data))
								(setq lf (length data)))
							(setq clear_text (insert clear_text cursor (slice 0 lf data))
								cursor (+ cursor lf) anchor cursor)))))
			((/= 0 (logand mod +ev_key_mod_shift))
				;shift key
				(cond
					((= key 0x40000050)
						;left
						(setq cursor (max 0 (dec cursor))))
					((= key 0x4000004f)
						;right
						(setq cursor (min (length clear_text) (inc cursor))))
					((<= +char_space key +char_tilda)
						;insert char
						(when (/= cursor anchor)
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(setq clear_text (erase clear_text sx sx1) cursor sx))
						(setq clear_text (insert clear_text cursor (char key))
							cursor (inc cursor) anchor cursor))))
			((or (= key +char_lf) (= key +char_cr))
				;enter action
				(setq anchor cursor)
				(. this :emit))
			((= key +char_backspace)
				;backspace
				(cond
					((/= cursor anchor)
						(defq sx (min anchor cursor) sx1 (max anchor cursor))
						(setq clear_text (erase clear_text sx sx1) cursor sx))
					((and (/= cursor 0) (/= (length clear_text) 0))
						(setq clear_text (erase clear_text (dec cursor) cursor)
							cursor (dec cursor))))
				(setq anchor cursor))
			((= key +char_delete)
				;delete
				(cond
					((/= cursor anchor)
						(defq sx (min anchor cursor) sx1 (max anchor cursor))
						(setq clear_text (erase clear_text sx sx1) cursor sx))
					((and (/= cursor (length clear_text)) (/= (length clear_text) 0))
						(setq clear_text (erase clear_text cursor (inc cursor)))))
				(setq anchor cursor))
			((= key 0x40000050)
				;left
				(if (= cursor anchor)
					(setq cursor (max 0 (dec cursor)))
					(setq cursor (min anchor cursor)))
				(setq anchor cursor))
			((= key 0x4000004f)
				;right
				(if (= cursor anchor)
					(setq cursor (min (length clear_text) (inc cursor)))
					(setq cursor (max anchor cursor)))
				(setq anchor cursor))
			((<= +char_space key +char_tilda)
				;insert char
				(when (/= cursor anchor)
					(defq sx (min anchor cursor) sx1 (max anchor cursor))
					(setq clear_text (erase clear_text sx sx1) cursor sx))
				(setq clear_text (insert clear_text cursor (char key))
					cursor (inc cursor) anchor cursor)))
		(lower :clear_text :cursor :anchor)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_down (this event)
		; (. textfield :mouse_down event) -> textfield
		(unless (eql (get :clear_text this) "")
			(raise :text :font)
			(defq rx (getf event +ev_msg_mouse_rx) anchor -1)
			(while (and (<= (setq anchor (inc anchor)) (length text))
						(> rx (elem-get 0 (font-glyph-bounds font (slice 0 anchor text))))))
			(setq anchor (max 0 (dec anchor)))
			(lower :anchor (:cursor anchor :state -1))
			(.-> this :layout :dirty_all))
		this)

	(defmethod :mouse_up (this event)
		; (. textfield :mouse_up event) -> textfield
		(when (/= (get :state this) 1)
			(def this :state 1))
		this)

	(defmethod :mouse_move (this event)
		; (. textfield :mouse_move event) -> textfield
		(unless (or (eql (get :clear_text this) "") (= (get :state this) 1))
			(raise :text :font)
			(defq rx (getf event +ev_msg_mouse_rx) cursor -1)
			(while (and (<= (setq cursor (inc cursor)) (length text))
						(> rx (elem-get 0 (font-glyph-bounds font (slice 0 cursor text))))))
			(setq cursor (max 0 (dec cursor)))
			(lower :cursor)
			(.-> this :layout :dirty_all))
		this)
	)
