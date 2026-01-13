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
			(defq ai (. buffer :cursor_to_index 0 sy)
				ci (. buffer :cursor_to_index 0 sy1))
			(push cursors (list ci ai ci)))
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
	(defproxymethod :insert (text) :buffer)

	(defmethod :get_cursor ()
		; (. edit :get_cursor) -> (cx cy ax ay)
		(. (get :buffer this) :get_cursor))

	(defmethod :set_cursor (cx cy &optional ax ay)
		; (. edit :set_cursor) -> edit
		(. (get :buffer this) :set_cursor cx cy ax ay)
		this)

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
		(bind '(ci ai) (.-> buffer :get_icursor))
		(defq text (. buffer :icopy ci ai))
		(first (split text +char_class_lf)))

	(defmethod :get_scroll ()
		; (. edit :get_scroll) -> (x y)
		(list (get :scroll_x this) (get :scroll_y this)))

	(defmethod :get_find ()
		; (. edit :get_find) -> (x y x1 y1)
		(list (get :find_x this) (get :find_y this)
			(get :find_x1 this) (get :find_y1 this)))

	(defmethod :set_find (find_x find_y find_x1 find_y1)
		; (. edit :set_find x y x1 y1) -> edit
		(lower :find_x :find_y :find_x1 :find_y1) this)

	(defmethod :clip_cursor (x y)
		; (. edit :clip_cursor x y) -> (x y)
		(raise :buffer)
		(bind '(w h) (. buffer :get_size))
		(cond
			((< y 0) (setq x 0 y 0))
			((> y h) (setq x 0 y h))
			((< x 0) (setq x 0))
			((> x (defq max_x (dec (length (elem-get (. buffer :get_buffer_lines) y)))))
				(setq x max_x)))
		(list x y))

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
		(defq ci (. buffer :cursor_to_index 0 h))
		(. buffer :set_icursor ci 0)
		this)

	(defmethod :get_selected_extent ()
		; (. edit :get_selected_extent) -> csr
		(defq max_x 0 min_x +max_int max_y 0 min_y +max_int)
		(each (lambda ((sx sy sx1 sy1))
				(setq max_x (max max_x sx1) min_x (min min_x sx)
					max_y (max max_y sy1) min_y (min min_y sy)))
			(map (const csr-floor) (. (get :buffer this) :get_selected)))
		(list min_x min_y max_x max_y))

	(defmethod :select_word ()
		; (. edit :select_word) -> edit
		;FIXME to separate regions
		(raise :buffer)
		(bind '(cx cy &ignore) (. buffer :get_cursor))
		(defq line (. buffer :get_text_line cy)
			x (rbskipn +char_class_not_whole_word line cx)
			x1 (bskipn +char_class_not_whole_word line cx))
		(. buffer :set_icursor
			(. buffer :cursor_to_index x1 cy)
			(. buffer :cursor_to_index x cy))
		this)

	(defmethod :select_line ()
		; (. edit :select_line) -> edit
		;FIXME to separate regions
		(raise :buffer)
		(bind '(cx cy &ignore) (. buffer :get_cursor))
		(. buffer :set_icursor
			(. buffer :cursor_to_index 0 (inc cy))
			(. buffer :cursor_to_index 0 cy))
		this)

	(defmethod :select_paragraph ()
		; (. edit :select_paragraph) -> edit
		;FIXME to separate regions
		(raise :buffer)
		(defq buffer_lines (. buffer :get_buffer_lines))
		(bind '(cx cy &ignore) (. buffer :get_cursor))
		(defq y (some! (# (if (blank-line? %0) (inc (!))))
				(list buffer_lines) :nil cy 0)
			y1 (some! (# (if (blank-line? %0) (!)))
				(list buffer_lines) :nil cy (bind '(w h) (. buffer :get_size))))
		(setd y 0 y1 h)
		(. buffer :set_icursor
			(. buffer :cursor_to_index 0 y1)
			(. buffer :cursor_to_index 0 y))
		this)

	(defmethod :select_block ()
		; (. edit :select_block) -> edit
		;FIXME to separate regions
		(raise :buffer)
		(if (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(defq ci (. buffer :cursor_to_index x y)
					ai (. buffer :cursor_to_index x1 y1))
				(. buffer :set_icursor ci (inc ai))))
		this)

	(defmethod :tab ()
		; (. edit :tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width))
			(reach (lambda ((ci ai sx))
					(defq ci (min ci ai))
					(bind '(cx cy) (. buffer :index_to_cursor ci))
					(. buffer :iinsert ci
						(pad "" (- tab_width (% cx tab_width)) "        ")))
				(. buffer :merge_cursors (. buffer :get_cursors)))
			(reach (lambda ((ci ai sx)) (. buffer :idelete ci ai))
				(. buffer :get_cursors)))
		this)

	(defmethod :left_tab ()
		; (. edit :left_tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width)
				selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(defq si (. buffer :cursor_to_index 0 sy)
							ei (+ si (min tab_width (bskip " " (. buffer :get_text_line sy) 0))))
						(. buffer :idelete si ei)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :right_tab ()
		; (. edit :right_tab) -> edit
		(undoable
			(defq tab (pad "" (. buffer :get_tab_width) "        ")
				selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(. buffer :iinsert
							(. buffer :cursor_to_index 0 sy)
							tab)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :to_lower ()
		; (. edit :to_lower) -> edit
		(undoable
			(defq cursors (. buffer :get_cursors))
			(reach (lambda ((ci ai sx))
					(. buffer :iinsert (min ci ai)
						(to-lower (. buffer :icopy ci ai))))
				(. buffer :merge_cursors cursors))
			(reach (lambda ((ci ai sx)) (. buffer :idelete ci ai))
				(. buffer :get_cursors))
			(. buffer :set_cursors cursors))
		this)

	(defmethod :to_upper ()
		; (. edit :to_upper) -> edit
		(undoable
			(defq cursors (. buffer :get_cursors))
			(reach (lambda ((ci ai sx))
					(. buffer :iinsert (min ci ai)
						(to-upper (. buffer :icopy ci ai))))
				(. buffer :merge_cursors cursors))
			(reach (lambda ((ci ai sx)) (. buffer :idelete ci ai))
				(. buffer :get_cursors))
			(. buffer :set_cursors cursors))
		this)

	(defmethod :reflow ()
		; (. edit :reflow) -> edit
		;FIXME to separate regions
		(undoable
			(bind '(sx sy sx1 sy1) (. this :get_selected_extent))
			(defq si (. buffer :cursor_to_index 0 sy)
				ei (. buffer :cursor_to_index 0 sy1)
				lines (.-> buffer :get_syntax_engine
					(:text_flow (split (. buffer :icopy si ei) +char_class_white_space)
						(. buffer :get_wrap_width))))
			(.-> buffer
				(:idelete si ei)
				(:iinsert si (join lines +char_class_lf 2))
				(:set_icursor (. buffer :cursor_to_index 0 (+ sy (length lines))) si)))
		this)

	(defmethod :split ()
		; (. edit :split) -> edit
		;FIXME to separate regions
		(undoable
			(bind '(sx sy sx1 sy1) (. this :get_selected_extent))
			(defq si (. buffer :cursor_to_index 0 sy)
				ei (. buffer :cursor_to_index 0 sy1)
				lines (split (. buffer :icopy si ei) +char_class_white_space))
			(.-> buffer
				(:idelete si ei)
				(:iinsert si (join lines +char_class_lf 2))
				(:set_icursor (. buffer :cursor_to_index 0 (+ sy (length lines))) si)))
		this)

	(defmethod :sort ()
		; (. edit :sort) -> edit
		(undoable
			(defq selected (map (const csr-floor) (. buffer :get_selected))
				lines (list))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(push lines (. buffer :get_text_line sy))
						(++ sy)))
				selected)
			(setq lines (reverse (sort lines)))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(defq si (. buffer :cursor_to_index 0 sy))
						(.-> buffer
							;avoid causing line breaks !
							(:idelete si (+ si (dec (length (. buffer :get_text_line sy)))))
							(:iinsert si (most (pop lines))))
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :unique ()
		; (. edit :unique) -> edit
		;FIXME
		(undoable
			(defq selected (map (const csr-floor) (. buffer :get_selected))
				lines (list))
			(reach (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(push lines (. buffer :get_text_line sy))
						(++ sy)))
				selected)
			(setq lines (reverse (unique lines)))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(defq si (. buffer :cursor_to_index 0 sy))
						(.-> buffer
							;avoid causing line breaks !
							(:idelete si (+ si (dec (length (. buffer :get_text_line sy)))))
							(:iinsert si (most (ifn (pop lines) +char_class_lf))))
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :invert ()
		; (. edit :invert) -> edit
		(undoable
			(defq selected (map (const csr-floor) (. buffer :get_selected))
				lines (list))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(push lines (. buffer :get_text_line sy))
						(++ sy)))
				selected)
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(defq si (. buffer :cursor_to_index 0 sy))
						(.-> buffer
							;avoid causing line breaks !
							(:idelete si (+ si (dec (length (. buffer :get_text_line sy)))))
							(:iinsert si (most (pop lines))))
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :comment ()
		; (. edit :comment) -> edit
		(undoable
			(defq selected (map (const csr-floor) (. buffer :get_selected)))
			(each (lambda ((sx sy sx1 sy1))
					(until (= sy sy1)
						(defq line (. buffer :get_text_line sy)
							si (. buffer :cursor_to_index 0 sy))
						(cond
							((blank-line? line))
							((starts-with +block_comment line)
								(. buffer :idelete si (+ si (const (length +block_comment)))))
							((. buffer :iinsert si +block_comment)))
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
		(bind '(fx fy fx1 fy1) (. this :get_find))
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
			(bind '(sx sy &ignore) csr)
			(when (<= scroll_y sy scroll_y1)
				(. mask_ink :add_opaque
					(* (- sx scroll_x) cw)
					(* (- sy scroll_y) ch)
					cc ch))
			(bind '(sx sy sx1 sy1) (csr-sort-csr csr))
			(unless (or (< sy1 scroll_y) (> sy scroll_y1))
				(cond
					((= sy sy1)
						;selection on same line
						(when (<= scroll_y sy scroll_y1)
							(. mask_selected :add_opaque
								(* (- sx scroll_x) cw)
								(* (- sy scroll_y) ch)
								(* (- sx1 sx) cw)
								ch)))
					(:t ;multi line selection
						(when (<= scroll_y sy scroll_y1)
							(. mask_selected :add_opaque
								(* (- sx scroll_x) cw)
								(* (- sy scroll_y) ch)
								(* (- (length (. buffer :get_text_line sy)) sx) cw)
								ch))
						(while (< (++ sy) sy1)
							(task-slice)
							(when (<= scroll_y sy scroll_y1)
								(. mask_selected :add_opaque
									(* (neg scroll_x) cw)
									(* (- sy scroll_y) ch)
									(* (length (. buffer :get_text_line sy)) cw)
									ch)))
						(when (<= scroll_y sy1 scroll_y1)
							(. mask_selected :add_opaque
								(* (neg scroll_x) cw)
								(* (- sy1 scroll_y) ch)
								(* sx1 cw)
								ch))))))
			(. buffer :get_selected_unsorted))
		this)

	(defmethod :mouse_down (event)
		; (. edit :mouse_down event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(if (= 1 (defq ai (. buffer :cursor_to_index x y)
				mouse_state (getf event +ev_msg_mouse_buttons)))
			(. buffer :set_icursor ai ai))
		(. buffer :set_tcursors `((,ai ,ai 0)))
		(lower :mouse_state)
		this)

	(defmethod :mouse_move (event)
		; (. edit :mouse_move event) -> edit
		(raise :buffer :mouse_state)
		(bind '(x y) (. this :char_pos event))
		(bind '((ci ai sx)) (. buffer :get_tcursors))
		(defq ci (. buffer :cursor_to_index x y))
		(. buffer :set_tcursors `((,ci ,ai 0)))
		(if (= 1 mouse_state)
			(. buffer :set_icursor ci ai))
		this)

	(defmethod :mouse_up (event)
		; (. vdu :mouse_up event) -> vdu
		(raise :buffer :mouse_state)
		(bind '((ci ai sx)) (. buffer :get_tcursors))
		(if (= 1 mouse_state)
			(. buffer :set_icursor ci ai)
			(. buffer :add_icursor ci ai))
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
