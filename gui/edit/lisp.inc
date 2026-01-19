;;;;;;;;;;;;;
; edit widget
;;;;;;;;;;;;;

(import "././vdu/lisp.inc")
(import "././mask/lisp.inc")
(import "././canvas/lisp.inc")
(import "lib/text/buffer.inc")

;module
(env-push)

(defq +block_comment ";; " +bracket_char (nums 0x7f))

(defun blank-line? (line)
	(eql (rbskip +char_class_white_space line -1) 0))

(defun lines-selected (selected)
	(defq cursors (list))
	(each (lambda ((sx sy sx1 sy1))
			(push cursors (. buffer :clip_cursor 0 sy1 0 sy)))
		selected)
	(. buffer :set_cursors cursors))

(defmacro undoable (&rest _)
	`(progn
		(raise :buffer)
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursors (. buffer :get_cursors)))
		~_
		(. buffer :push_undo (list :mark mark))))

(defclass Edit () (View)
	; (Edit) -> edit
	(def this
		:vdu_text (defq vdu_text (Vdu))
		:mask_ink (defq mask_ink (Mask))
		:mask_selected (defq mask_selected (Mask))
		:mask_found (defq mask_found (Mask))
		:mask_region (defq mask_region (Mask))
		:scroll_x 0 :scroll_y 0
		:find_x 0 :find_y 0 :find_x1 0 :find_y1 0
		:buffer (Buffer)
		:vdu_width 80 :vdu_height 40 :min_width 0 :min_height 0)
	(each (# (. this :add_child (. %0 :set_flags 0 +view_flag_solid)))
		(list vdu_text mask_ink mask_selected mask_found mask_region))

	(defsetmethod :buffer)
	(defgetmethod :buffer)
	(defgetmethod :vdu_text)

	(defproxymethod :left () :buffer)
	(defproxymethod :right () :buffer)
	(defproxymethod :up () :buffer)
	(defproxymethod :down () :buffer)
	(defproxymethod :home () :buffer)
	(defproxymethod :end () :buffer)
	(defproxymethod :top () :buffer)
	(defproxymethod :bottom () :buffer)
	(defproxymethod :left_select () :buffer)
	(defproxymethod :right_select () :buffer)
	(defproxymethod :up_select () :buffer)
	(defproxymethod :down_select () :buffer)
	(defproxymethod :home_select () :buffer)
	(defproxymethod :end_select () :buffer)
	(defproxymethod :top_select () :buffer)
	(defproxymethod :bottom_select () :buffer)
	(defproxymethod :backspace () :buffer)
	(defproxymethod :delete () :buffer)
	(defproxymethod :select_word () :buffer)
	(defproxymethod :select_line () :buffer)
	(defproxymethod :select_paragraph () :buffer)
	(defproxymethod :floor_selection () :buffer)
	(defproxymethod :insert (text) :buffer)
	(defproxymethod :get_cursor () :buffer :t)
	(defproxymethod :set_cursor (cx cy &optional ax ay) :buffer)
	(defproxymethod :clip_cursor (cx cy &optional ax ay sx) :buffer :t)

	(defmethod :max_size ()
		; (. edit :max_size) -> (width height)
		(bind '(w h) (. this :get_size))
		(bind '(cw ch) (. (get :vdu_text this) :char_size))
		(list (/ w cw) (/ h ch)))

	(defmethod :constraint ()
		; (. edit :constraint) -> (width height)
		(defq min_w (def? :min_width this) min_h (def? :min_height this)
			vdu_w (get :vdu_width this) vdu_h (get :vdu_height this)
			w 0 h 0)
		(each (lambda (child)
				(def child
					:min_width min_w :min_height min_h
					:vdu_width vdu_w :vdu_height vdu_h)
				(bind '(cw ch) (. child :constraint))
				(setq w (max w cw) h (max h ch)))
			(. this :children))
		(list w h))

	(defmethod :layout ()
		; (. edit :layout) -> edit
		(defq min_w (def? :min_width this) min_h (def? :min_height this)
			vdu_w (get :vdu_width this) vdu_h (get :vdu_height this))
		(bind '(w h) (. this :get_size))
		(each (lambda (child)
				(def child
					:min_width min_w :min_height min_h
					:vdu_width vdu_w :vdu_height vdu_h)
				(. child :set_bounds 0 0 w h))
			(. this :children))
		this)

	(defmethod :get_select ()
		; (. edit :get_select) -> text
		(raise :buffer)
		(bind '(cx cy ax ay) (.-> buffer :get_cursor))
		(defq text (. buffer :icopy cx cy ax ay))
		(first (split text +char_class_lf)))

	(defmethod :get_scroll ()
		; (. edit :get_scroll) -> (x y)
		(list (get :scroll_x this) (get :scroll_y this)))

	(defmethod :get_focus ()
		; (. edit :get_focus) -> (x y x1 y1)
		(list (get :find_x this) (get :find_y this)
			(get :find_x1 this) (get :find_y1 this)))

	(defmethod :set_focus (find_x find_y find_x1 find_y1)
		; (. edit :set_focus x y x1 y1) -> edit
		(lower :find_x :find_y :find_x1 :find_y1) this)

	(defmethod :set_scroll (scroll_x scroll_y)
		; (. edit :set_scroll x y) -> edit
		(lower :scroll_x :scroll_y) this)

	(defmethod :char_pos (event)
		; (. edit :char_pos event) -> (x y)
		(raise :scroll_x :scroll_y :vdu_text)
		(bind '(w h) (. vdu_text :char_size))
		(defq x (getf event +ev_msg_mouse_rx) y (getf event +ev_msg_mouse_ry))
		(setq x (if (>= x 0) x (- x w)) y (if (>= y 0) y (- y h)))
		(list (+ scroll_x (/ x w)) (+ scroll_y (/ y h))))

	(defmethod :left_bracket ()
		; (. edit :left_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :left_bracket))
			(. this :set_cursor cx cy)))

	(defmethod :right_bracket ()
		; (. edit :right_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :right_bracket))
			(. this :set_cursor cx cy)))

	(defmethod :select_all ()
		; (. edit :select_all) -> edit
		(raise :buffer)
		(bind '(w h) (. buffer :get_size))
		(defq max_x (dec (length (. buffer :get_text_line h))))
		(. buffer :set_cursor max_x h 0 0)
		this)

	(defmethod :get_selected_extent ()
		; (. edit :get_selected_extent) -> csr
		(defq max_x 0 min_x +max_int max_y 0 min_y +max_int)
		(each (lambda ((sx sy sx1 sy1))
				(setq max_x (max max_x sx1) min_x (min min_x sx)
					max_y (max max_y sy1) min_y (min min_y sy)))
			(map (const csr-floor) (. (get :buffer this) :get_selected)))
		(list min_x min_y max_x max_y))

	(defmethod :select_block ()
		; (. edit :select_block) -> edit
		;FIXME to separate regions
		(raise :buffer)
		(if (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(. buffer :set_cursor x1 y1 x y)))
		this)

	(defmethod :tab ()
		; (. edit :tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(cx cy &ignore) (csr-sort (nums cx cy ax ay)))
					(. buffer :iinsert cx cy
						(pad "" (- tab_width (% cx tab_width)) "        ")))
				(. buffer :merge_cursors (. buffer :get_cursors)))
			(reach (lambda ((cx cy ax ay &ignore)) (. buffer :idelete cx cy ax ay))
				(. buffer :get_cursors)))
		this)

	(defmethod :left_tab ()
		; (. edit :left_tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width)
				selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(defq n (min tab_width (bskip " " (. buffer :get_text_line sy) 0)))
						(. buffer :idelete 0 sy n sy)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :right_tab ()
		; (. edit :right_tab) -> edit
		(undoable
			(defq tab (pad "" (. buffer :get_tab_width) "        ")
				selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(. buffer :iinsert 0 sy tab)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :to_lower ()
		; (. edit :to_lower) -> edit
		(undoable
			(defq cursors (. buffer :get_cursors))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(sx sy &ignore) (csr-sort (nums cx cy ax ay)))
					(. buffer :iinsert sx sy
						(to-lower (. buffer :icopy cx cy ax ay))))
				(. buffer :merge_cursors cursors))
			(reach (lambda ((cx cy ax ay &ignore)) (. buffer :idelete cx cy ax ay))
				(. buffer :get_cursors))
			(. buffer :set_cursors cursors))
		this)

	(defmethod :to_upper ()
		; (. edit :to_upper) -> edit
		(undoable
			(defq cursors (. buffer :get_cursors))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(sx sy &ignore) (csr-sort (nums cx cy ax ay)))
					(. buffer :iinsert sx sy
						(to-upper (. buffer :icopy cx cy ax ay))))
				(. buffer :merge_cursors cursors))
			(reach (lambda ((cx cy ax ay &ignore)) (. buffer :idelete cx cy ax ay))
				(. buffer :get_cursors))
			(. buffer :set_cursors cursors))
		this)

	(defmethod :reflow ()
		; (. edit :reflow) -> edit
		(undoable
			(. this :select_paragraph)
			(reach (lambda ((sx1 sy1 sx sy &ignore))
				(when (/= sy sy1)
					(defq lines (.-> buffer :get_syntax_engine
							(:text_flow (split (. buffer :icopy 0 sy 0 sy1) +char_class_white_space)
								(. buffer :get_wrap_width))))
					(.-> buffer
						(:idelete 0 sy 0 sy1)
						(:iinsert 0 sy (join lines +char_class_lf 2)))))
				(. buffer :get_selected)))
		this)

	(defmethod :split ()
		; (. edit :split) -> edit
		(undoable
			(. this :select_paragraph)
			(reach (lambda ((sx1 sy1 sx sy &ignore))
				(when (/= sy sy1)
					(defq lines (split (. buffer :icopy 0 sy 0 sy1) +char_class_white_space))
					(.-> buffer
						(:idelete 0 sy 0 sy1)
						(:iinsert 0 sy (join lines +char_class_lf 2)))))
				(. buffer :get_selected)))
		this)

	(defmethod :sort ()
		; (. edit :sort) -> edit
		(undoable
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. buffer :get_text_line y))
						(++ y))
					(setq lines (reverse (sort lines)) y sy)
					(while (< y sy1)
						(.-> buffer
							(:idelete 0 y (dec (length (. buffer :get_text_line y))) y)
							(:iinsert 0 y (most (pop lines))))
						(++ y)))
				(. buffer :get_selected))
			(lines-selected (. buffer :get_selected)))
		this)

	(defmethod :invert ()
		; (. edit :invert) -> edit
		(undoable
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. buffer :get_text_line y))
						(++ y))
					(setq y sy)
					(while (< y sy1)
						(.-> buffer
							(:idelete 0 y (dec (length (. buffer :get_text_line y))) y)
							(:iinsert 0 y (most (pop lines))))
						(++ y)))
				(. buffer :get_selected))
			(lines-selected (. buffer :get_selected)))
		this)

	(defmethod :unique ()
		; (. edit :unique) -> edit
		(undoable
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. buffer :get_text_line y))
						(++ y))
					(setq lines (reverse (unique lines)) y sy)
					(while (< y sy1)
						(if (defq line (pop lines))
							(.-> buffer
								(:idelete 0 y (dec (length (. buffer :get_text_line y))) y)
								(:iinsert 0 y (most line)))
							(progn
								(. buffer :idelete 0 y 0 sy1)
								(setq y sy1)))
						(++ y)))
				(. buffer :get_selected))
			(lines-selected (. buffer :get_selected)))
		this)

	(defmethod :comment ()
		; (. edit :comment) -> edit
		(undoable
			(defq selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(defq line (. buffer :get_text_line sy))
						(cond
							((blank-line? line))
							((starts-with +block_comment line)
								(. buffer :idelete 0 sy (length +block_comment) sy))
							((. buffer :iinsert 0 sy +block_comment)))
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :set_ink_color (col)
		; (. edit :set_ink_color argb) -> edit
		(def (get :mask_ink this) :color col)
		this)

	(defmethod :set_select_color (col)
		; (. edit :set_select_color argb) -> edit
		(def (get :mask_selected this) :color col)
		this)

	(defmethod :set_found_color (col)
		; (. edit :set_found_color argb) -> edit
		(def (get :mask_found this) :color col)
		this)

	(defmethod :set_region_color (col)
		; (. edit :set_region_color argb) -> edit
		(def (get :mask_region this) :color col)
		this)

	(defmethod :underlay ()
		; (. edit :underlay) -> edit
		;create the masks for selections, brackets and cursors
		(raise :buffer :mask_ink :mask_selected :mask_found
			:mask_region :scroll_x :scroll_y)
		(bind '(w h) (. this :get_size))
		(bind '(bw bh) (. buffer :get_size))
		(bind '(cw ch) (. (get :vdu_text this) :char_size))
		(bind '(fx fy fx1 fy1) (. this :get_focus))
		(defq cc (max (>> cw 2) 1) cb (max (>> cw 3) 1)
			scroll_y (min (max scroll_y 0) bh)
			scroll_y1 (min (max (+ scroll_y (/ h ch)) scroll_y) bh))
		;set the region mask
		(. mask_region :clr_opaque)
		(unless (or (<= fy1 scroll_y) (>= fy scroll_y1))
			(. mask_region :add_opaque
				0 (* (- fy scroll_y) ch)
				w (* (- (min fy1 scroll_y1) fy) ch)))
		;set found mask
		(. mask_found :clr_opaque)
		(each! (# (when %0
				(defq y (* (- (!) scroll_y) ch))
				(each (lambda (((x x1) &ignore))
					(defq x1 (* (- x1 x) cw) x (* (- x scroll_x) cw))
					(. mask_found :add_opaque x y x1 ch)) %0)))
			(list (. buffer :get_buffer_found)) scroll_y scroll_y1)
		;set ink mask
		(-- scroll_y1)
		(. mask_ink :clr_opaque)
		(when (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(when (<= scroll_y y scroll_y1)
					(setq x (* (- x scroll_x) cw) y (* (- y scroll_y) ch))
					(. mask_ink :add_opaque x y cb ch)
					(. mask_ink :add_opaque (+ x cw (neg cb)) y cb ch)
					(. mask_ink :add_opaque x y cw cb)
					(. mask_ink :add_opaque x (+ y ch (neg cb)) cw cb))
				(when (<= scroll_y y1 scroll_y1)
					(setq x1 (* (- x1 scroll_x) cw) y1 (* (- y1 scroll_y) ch))
					(. mask_ink :add_opaque x1 y1 cb ch)
					(. mask_ink :add_opaque (+ x1 cw (neg cb)) y1 cb ch)
					(. mask_ink :add_opaque x1 y1 cw cb)
					(. mask_ink :add_opaque x1 (+ y1 ch (neg cb)) cw cb))))
		;set selected mask
		(. mask_selected :clr_opaque)
		(each (lambda (csr)
			(task-slice)
			(bind '(cx cy &ignore) csr)
			(when (<= scroll_y cy scroll_y1)
				(. mask_ink :add_opaque
					(* (- cx scroll_x) cw)
					(* (- cy scroll_y) ch)
					cc ch))
			(bind '(cx cy ax ay) (csr-sort csr))
			(unless (or (< ay scroll_y) (> cy scroll_y1))
				(cond
					((= cy ay)
						;selection on same line
						(when (<= scroll_y cy scroll_y1)
							(. mask_selected :add_opaque
								(* (- cx scroll_x) cw)
								(* (- cy scroll_y) ch)
								(* (- ax cx) cw)
								ch)))
					(:t ;multi line selection
						(when (<= scroll_y cy scroll_y1)
							(. mask_selected :add_opaque
								(* (- cx scroll_x) cw)
								(* (- cy scroll_y) ch)
								(* (- (length (. buffer :get_text_line cy)) cx) cw)
								ch))
						(while (< (++ cy) ay)
							(task-slice)
							(when (<= scroll_y cy scroll_y1)
								(. mask_selected :add_opaque
									(* (neg scroll_x) cw)
									(* (- cy scroll_y) ch)
									(* (length (. buffer :get_text_line cy)) cw)
									ch)))
						(when (<= scroll_y ay scroll_y1)
							(. mask_selected :add_opaque
								(* (neg scroll_x) cw)
								(* (- ay scroll_y) ch)
								(* ax cw)
								ch))))))
			(. buffer :get_selected_unsorted))
		this)

	(defmethod :mouse_down (event)
		; (. edit :mouse_down event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(if (= 1 (defq mouse_state (getf event +ev_msg_mouse_buttons)))
			(. buffer :set_cursor x y x y))
		(. buffer :set_tcursors (list (. buffer :clip_cursor x y)))
		(lower :mouse_state)
		this)

	(defmethod :mouse_move (event)
		; (. edit :mouse_move event) -> edit
		(raise :buffer :mouse_state)
		(bind '(x y) (. this :char_pos event))
		(bind '((cx cy ax ay sx)) (. buffer :get_tcursors))
		(. buffer :set_tcursors (list (. buffer :clip_cursor x y ax ay)))
		(if (= 1 mouse_state)
			(. buffer :set_cursor x y ax ay))
		this)

	(defmethod :mouse_up (event)
		; (. vdu :mouse_up event) -> vdu
		(raise :buffer :mouse_state)
		(bind '((cx cy ax ay sx)) (. buffer :get_tcursors))
		(if (= 1 mouse_state)
			(. buffer :set_cursor cx cy ax ay)
			(. buffer :add_cursor cx cy ax ay))
		(. buffer :set_tcursors '())
		this)

	(defmethod :mouse_wheel (event)
		; (. edit :mouse_wheel event) -> edit
		(raise :scroll_x :scroll_y)
		(setq scroll_x (+ scroll_x (getf event +ev_msg_wheel_x))
			scroll_y (- scroll_y (getf event +ev_msg_wheel_y)))
		(lower :scroll_x :scroll_y)
		this)
	)

;module
(export-classes '(Edit))
(env-pop)
