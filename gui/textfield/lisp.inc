;;;;;;;;;;;;;;;;;;
; textfield widget
;;;;;;;;;;;;;;;;;;

(import "././label/lisp.inc")
(import "././font/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/files/urls.inc")
(import "service/clipboard/app.inc")

;module
(env-push)

(defun setoffset ()
	;adjust text offset
	(raise :clear_text :label_text :font :border :cursor)
	(bind '(tw th) (font-glyph-bounds font (slice clear_text 0 cursor)))
	(bind '(w h) (. this :get_size))
	(defq offset (get :offset label_text) border (abs border))
	(cond
		((> (+ tw offset (* border 2) 2) w)
			(setq offset (- w tw border 2)))
		((< (- (+ tw offset) border) 0)
			(setq offset (- 2 tw border))))
	(set label_text :offset offset))

(defclass Textfield () (Label)
	; (Textfield) -> textfield
	(def this :cursor 0 :anchor 0 :clear_text "" :hint_text "" :text ""
		:mode :nil :state 1)

	(defmethod :draw ()
		; (. textfield :draw) -> textfield
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(raise :font :text :cursor :anchor :label_text)
		(when (and font text)
			(defq offset (get :offset label_text))
			(bind '(tw th) (font-glyph-bounds font (slice text 0 cursor)))
			(when (/= cursor anchor)
				(bind '(sw _) (font-glyph-bounds font (slice text 0 (min anchor cursor))))
				(bind '(sw1 _) (font-glyph-bounds font (slice text 0 (max anchor cursor))))
				(.-> this
					(:ctx_set_color (get :hint_color this))
					(:ctx_filled_box (+ sw offset) (>>> (- h th) 1) (- sw1 sw) th)))
			(.-> this
				(:ctx_set_color (get :ink_color this))
				(:ctx_filled_box (+ tw offset) (>>> (- h th) 1) 2 th)))
		this)

	(defmethod :layout ()
		; (. textfield :layout) -> textfield
		(cond
			((eql (defq text (get :clear_text this)) "")
				(defq text (get :hint_text this) mode :nil)
				(def this :ink_color (get :hint_color this)))
			(:t (defq mode (get :mode this))
				(def this :ink_color (get :no_hint_color this))))
		(if mode
			(def this :text (pad "" (length text) "******************"))
			(def this :text text))
		(.super this :layout))

	(defmethod :constraint ()
		; (. textfield :constraint) -> (width height)
		(bind '(w h) (.super this :constraint))
		(list 128 h))

	(defmethod :set_text (text)
		; (. textfield :set_text text) -> textfield
		(def this :clear_text text :cursor (length text) :anchor 0)
		this)

	(defmethod :get_text ()
		; (. textfield :get_text) -> text
		(get :clear_text this))

	(defmethod :key_down (event)
		; (. textfield :key_down event) -> textfield
		(raise :clear_text :cursor :anchor :mode)
		(defq key (getf event +ev_msg_key_key) mod (getf event +ev_msg_key_mod)
			cursor (min cursor (length clear_text)))
		(cond
			((bits? mod +ev_key_mod_control +ev_key_mod_meta)
				;control/command key
				(cond
					((= key (ascii-code "a"))
						;select all
						(setq anchor 0 cursor (length clear_text)))
					((= key (ascii-code "x"))
						;cut
						(unless (or mode (= cursor anchor))
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(clip-put-rpc (slice clear_text sx sx1))
							(setq clear_text (erase clear_text sx sx1)
								cursor sx anchor cursor)))
					((= key (ascii-code "c"))
						;copy
						(unless (or mode (= cursor anchor))
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(clip-put-rpc (slice clear_text sx sx1))))
					((= key (ascii-code "v"))
						;paste
						(when (/= cursor anchor)
							(defq sx (min anchor cursor) sx1 (max anchor cursor))
							(setq clear_text (erase clear_text sx sx1)
								cursor sx anchor cursor))
						(unless (eql (defq data (clip-get-rpc)) "")
							(unless (defq lf (find (ascii-char +char_lf) data))
								(setq lf (length data)))
							(setq clear_text (insert clear_text cursor (slice data 0 lf))
								cursor (+ cursor lf) anchor cursor)))))
			((bits? mod +ev_key_mod_shift)
				;shift key
				(cond
					((= key 0x40000050)
						;left
						(setq cursor (max 0 (dec cursor))))
					((= key 0x4000004f)
						;right
						(setq cursor (min (length clear_text) (inc cursor))))
					((<= +char_space key +char_tilde)
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
			((= key +char_tab)
				;tab
				(setq clear_text (cat (slice clear_text 0 cursor)
						(url-ext clear_text cursor 1))
					anchor (length clear_text) cursor anchor))
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
			((<= +char_space key +char_tilde)
				;insert char
				(when (/= cursor anchor)
					(defq sx (min anchor cursor) sx1 (max anchor cursor))
					(setq clear_text (erase clear_text sx sx1) cursor sx))
				(setq clear_text (insert clear_text cursor (char key))
					cursor (inc cursor) anchor cursor)))
		(lower :clear_text :cursor :anchor)
		;adjust text offset
		(setoffset)
		(.-> this (:constrain :t) :dirty_all))

	(defmethod :mouse_down (event)
		; (. textfield :mouse_down event) -> textfield
		(unless (eql (get :clear_text this) "")
			(raise :text :font :label_text)
			(defq rx (- (getf event +ev_msg_mouse_rx) (get :offset label_text)) anchor -1)
			(while (and (<= (++ anchor) (length text))
						(> rx (first (font-glyph-bounds font (slice text 0 anchor))))))
			(setq anchor (max 0 (dec anchor)))
			(lower :anchor (:cursor anchor :state -1))
			;adjust text offset
			(setoffset)
			(.-> this (:constrain :t) :dirty_all))
		this)

	(defmethod :mouse_up (event)
		; (. textfield :mouse_up event) -> textfield
		(when (/= (get :state this) 1)
			(def this :state 1))
		this)

	(defmethod :mouse_move (event)
		; (. textfield :mouse_move event) -> textfield
		(unless (or (eql (get :clear_text this) "") (= (get :state this) 1))
			(raise :text :font :label_text)
			(defq rx (- (getf event +ev_msg_mouse_rx) (get :offset label_text)) cursor -1)
			(while (and (<= (++ cursor) (length text))
						(> rx (first (font-glyph-bounds font (slice text 0 cursor))))))
			(setq cursor (max 0 (dec cursor)))
			(lower :cursor)
			;adjust text offset
			(setoffset)
			(.-> this (:constrain :t) :dirty_all))
		this)
	)

;module
(export-classes '(Textfield))
(env-pop)
