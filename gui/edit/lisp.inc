;;;;;;;;;;;;;
; edit widget
;;;;;;;;;;;;;

(import "././vdu/lisp.inc")
(import "././canvas/lisp.inc")
(import "lib/text/buffer.inc")

;module
(env-push)

(defq +block_comment ";; " +bracket_char (nums 0x7f)
	+blank_line (nums) +max_line_len 1024
	+not_selected (apply nums (map (lambda (&ignore) 0) (str-alloc +max_line_len))))

(defun chunk (color len)
	(while (> len (length color))
		(setq color (cat color color)))
	(slice color 0 len))

(defun overlay (color x x1)
	(if (and (<= 0 x (length color)) (<= 0 x1 (length color)))
		(slice color x x1) color))

(defmacro not-whole-word-char? (_)
	`(bfind ,_ +char_class_not_whole_word))

(defun blank-line? (line)
	(every (# (eql " " %0)) line))

(defun selection? (this)
	(bind '(cx cy) (. this :get_cursor))
	(bind '(ax ay) (. this :get_anchor))
	(or (/= ax cx) (/= ay cy)))

(defun sort-selection (this)
	(bind '(sx sy) (. this :get_cursor))
	(bind '(sx1 sy1) (. this :get_anchor))
	(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
	(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))
	(list sx sy sx1 sy1))

(defun select-lines (this)
	(bind '(sx sy sx1 sy1) (sort-selection this))
	(if (or (> sx1 0) (= sy sy1)) (setq sy1 (inc sy1)))
	(list sy sy1))

(defun select-word (this)
	(defq buffer (. this :get_buffer))
	(bind '(cx cy) (. this :get_cursor))
	(defq line (. buffer :get_text_line cy)
		x (some! (# (if (not-whole-word-char? %0) (inc (!))))
			(list line) :nil cx 0)
		x1 (some! (# (if (not-whole-word-char? %0) (!)))
			(list line) :nil cx))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1))

(defun select-paragraph (this)
	(defq buffer (. this :get_buffer))
	(bind '(cx cy) (. this :get_cursor))
	(defq y (some! (# (if (blank-line? %0) (inc (!))))
			(list (. buffer :get_text_lines)) :nil cy 0)
		y1 (some! (# (if (blank-line? %0) (!)))
			(list (. buffer :get_text_lines)) :nil cy (bind '(w h) (. buffer :get_size))))
	(unless y (setq y 0))
	(unless y1 (setq y1 h))
	(list y y1))

(defmacro undoable (&rest _)
	`(progn
		(raise :buffer)
		(bind '(cx cy) (. this :get_cursor))
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor cx cy))
		~_
		(. buffer :push_undo (list :mark mark))))

(defclass Edit () (View)
	; (Edit) -> edit
	(def this :vdu_text (defq vdu_text (Vdu))
		:vdu_paper (defq vdu_paper (Vdu)) :vdu_ink (defq vdu_ink (Vdu))
		:anchor_x 0 :anchor_y 0 :scroll_x 0 :scroll_y 0
		:find_x 0 :find_y 0 :find_x1 0 :find_y1 0
		:buffer (Buffer) :underlay '()
		:color_select +not_selected :color_found +not_selected
		:color_region +not_selected
		:vdu_width 80 :vdu_height 40 :min_width 0 :min_height 0)
	(. vdu_text :set_flags 0 +view_flag_solid)
	(. vdu_paper :set_flags 0 +view_flag_solid)
	(. vdu_ink :set_flags 0 +view_flag_solid)
	(.-> this (:add_child vdu_text) (:add_child vdu_ink) (:add_child vdu_paper))

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

	(defmethod :get_buffer ()
		; (. edit :get_buffer) -> text_buffer
		(get :buffer this))

	(defmethod :get_vdu_text ()
		; (. edit :get_vdu_text) -> vdu_text
		(get :vdu_text this))

	(defmethod :get_cursor ()
		; (. edit :get_cursor) -> (x y)
		(raise :buffer)
		(. buffer :get_cursor))

	(defmethod :get_anchor ()
		; (. edit :get_anchor) -> (x y)
		(list (get :anchor_x this) (get :anchor_y this)))

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

	(defmethod :set_buffer (buffer)
		; (. edit :set_buffer text_buffer) -> this
		(lower :buffer) this)

	(defmethod :set_cursor (x y)
		; (. edit :set_cursor x y) -> edit
		(raise :buffer)
		(. buffer :set_cursor x y) this)

	(defmethod :set_anchor (anchor_x anchor_y)
		; (. edit :set_anchor x y) -> edit
		(lower :anchor_x :anchor_y) this)

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

	(defmethod :clear_selection ()
		; (. edit :clear_selection) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor cx cy) (:set_anchor cx cy)))

	(defmethod :home ()
		; (. edit :home) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor 0 cy) (:set_anchor 0 cy)))

	(defmethod :end ()
		; (. edit :end) -> edit
		(raise :buffer)
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(cx cy) (. buffer :constrain +max_int cy))
		(.-> this (:set_cursor cx cy) (:set_anchor cx cy)))

	(defmethod :left ()
		; (. edit :left) -> edit
		(cond
			((selection? this)
				(bind '(sx sy sx1 sy1) (sort-selection this))
				(. this :set_cursor sx sy))
			(:t (. (get :buffer this) :left)))
		(. this :clear_selection))

	(defmethod :right ()
		; (. edit :right) -> edit
		(cond
			((selection? this)
				(bind '(sx sy sx1 sy1) (sort-selection this))
				(. this :set_cursor sx1 sy1))
			(:t (. (get :buffer this) :right)))
		(. this :clear_selection))

	(defmethod :up ()
		; (. edit :up) -> edit
		(when (selection? this)
			(bind '(sx sy sx1 sy1) (sort-selection this))
			(. this :set_cursor sx sy))
		(. (get :buffer this) :up)
		(. this :clear_selection))

	(defmethod :down ()
		; (. edit :down) -> edit
		(when (selection? this)
			(bind '(sx sy sx1 sy1) (sort-selection this))
			(. this :set_cursor sx1 sy1))
		(. (get :buffer this) :down)
		(. this :clear_selection))

	(defmethod :home_select ()
		; (. edit :home_select) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(. this :set_cursor 0 cy))

	(defmethod :end_select ()
		; (. edit :end_select) -> edit
		(raise :buffer)
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(cx cy) (. buffer :constrain +max_int cy))
		(. this :set_cursor cx cy))

	(defmethod :left_select ()
		; (. edit :left_select) -> edit
		(. (get :buffer this) :left)
		this)

	(defmethod :right_select ()
		; (. edit :right_select) -> edit
		(. (get :buffer this) :right)
		this)

	(defmethod :up_select ()
		; (. edit :up_select) -> edit
		(. (get :buffer this) :up)
		this)

	(defmethod :down_select ()
		; (. edit :down_select) -> edit
		(. (get :buffer this) :down)
		this)

	(defmethod :left_bracket ()
		; (. edit :left_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :left_bracket))
			(. this :set_cursor cx cy))
		(. this :clear_selection))

	(defmethod :right_bracket ()
		; (. edit :right_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :right_bracket))
			(. this :set_cursor cx cy))
		(. this :clear_selection))

	(defmethod :top ()
		; (. edit :top) -> edit
		(.-> this (:set_cursor 0 0) :clear_selection))

	(defmethod :bottom ()
		; (. edit :bottom) -> edit
		(bind '(w h) (. (get :buffer this) :get_size))
		(.-> this (:set_cursor 0 h) :clear_selection))

	(defmethod :select_all ()
		; (. edit :select_all) -> edit
		(bind '(w h) (. (get :buffer this) :get_size))
		(.-> this (:set_cursor 0 h) (:set_anchor 0 0)))

	(defmethod :select_word ()
		; (. edit :select_word) -> edit
		(bind '(x x1) (select-word this))
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor x1 cy) (:set_anchor x cy)))

	(defmethod :select_line ()
		; (. edit :select_line) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor 0 (inc cy)) (:set_anchor 0 cy)))

	(defmethod :select_paragraph ()
		; (. edit :select_paragraph) -> edit
		(bind '(y y1) (select-paragraph this))
		(.-> this (:set_cursor 0 y1) (:set_anchor 0 y)))

	(defmethod :select_block ()
		; (. edit :select_block) -> edit
		(raise :buffer)
		(if (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(.-> this (:set_cursor (inc x1) y1) (:set_anchor x y))))
		this)

	(defmethod :insert (string)
		; (. edit :insert string) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(. buffer :cut ax ay)
		(. buffer :insert string)
		(. this :clear_selection))

	(defmethod :backspace ()
		; (. edit :backspace) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(when (eql (. buffer :cut ax ay) "")
			(. buffer :backspace))
		(. this :clear_selection))

	(defmethod :delete ()
		; (. edit :delete) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(when (eql (. buffer :cut ax ay) "")
			(. buffer :delete))
		(. this :clear_selection))

	(defmethod :break ()
		; (. edit :break) -> edit
		(undoable
			(bind '(ax ay) (. this :get_anchor))
			(. buffer :cut ax ay)
			(bind '(x y) (.-> buffer :break :get_cursor))
			(defq line (. buffer :get_text_line (dec y))
				indent (some (# (unless (eql %0 (ascii-char +char_space)) (!))) line))
			(unless indent (setq indent (length line)))
			(if (> indent 0) (. buffer :insert (pad "" indent "        "))))
		(. this :clear_selection))

	(defmethod :tab ()
		; (. edit :tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width))
			(. buffer :cut ax ay)
			(bind '(cx cy) (. buffer :get_cursor))
			(. buffer :insert (pad "" (- tab_width (% cx tab_width)) "        ")))
		(. this :clear_selection))

	(defmethod :left_tab ()
		; (. edit :left_tab) -> edit
		(undoable
			(defq tab_width (. buffer :get_tab_width))
			(bind '(y y1) (select-lines this))
			(each! (lambda (line)
				(task-slice)
				(defq idx -1)
				(while (and
					(< (++ idx) (length line))
					(< idx tab_width)
					(eql (elem-get line idx) " ")))
				(if (> idx 0)
					(.-> buffer (:set_cursor 0 (!)) (:delete (min tab_width idx)))))
				(list (. buffer :get_text_lines)) y y1)
			(. this :set_cursor cx cy))
		this)

	(defmethod :right_tab ()
		; (. edit :right_tab) -> edit
		(undoable
			(defq tab (pad "" (. buffer :get_tab_width) "        "))
			(bind '(y y1) (select-lines this))
			(each! (lambda (line)
				(task-slice)
				(unless (eql line "")
					(.-> buffer (:set_cursor 0 (!)) (:insert tab))))
				(list (. buffer :get_text_lines)) y y1)
			(. this :set_cursor cx cy))
		this)

	(defmethod :to_lower ()
		; (. edit :to_lower) -> edit
		(when (selection? this)
			(undoable
				(bind '(ax ay) (. this :get_anchor))
				(. buffer :paste (to-lower (. buffer :cut ax ay)))))
		this)

	(defmethod :to_upper ()
		; (. edit :to_upper) -> edit
		(when (selection? this)
			(undoable
				(bind '(ax ay) (. this :get_anchor))
				(. buffer :paste (to-upper (. buffer :cut ax ay)))))
		this)

	(defmethod :reflow ()
		; (. edit :reflow) -> edit
		(undoable
			(bind '(y y1) (select-paragraph this))
			(each (lambda (line)
					(task-slice)
					(.-> buffer (:insert line) :break))
				(.-> buffer :get_syntax (:text_flow
					(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1))
						(const (char-class " \n\r")))
					(. buffer :get_wrap_width))))
			(bind '(x y) (. buffer :get_cursor))
			(bind '(x y) (. buffer :constrain x (inc y)))
			(.-> this (:set_cursor x y) (:set_anchor x y)))
		this)

	(defmethod :split ()
		; (. edit :split) -> edit
		(undoable
			(bind '(y y1) (select-paragraph this))
			(each (lambda (line)
					(task-slice)
					(.-> buffer (:insert line) :break))
				(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1))
					(const (char-class " \n\r"))))
			(bind '(x y) (. buffer :get_cursor))
			(bind '(x y) (. buffer :constrain x (inc y)))
			(.-> this (:set_cursor x y) (:set_anchor x y)))
		this)

	(defmethod :sort ()
		; (. edit :sort) -> edit
		(when (selection? this)
			(undoable
				(bind '(y y1) (select-lines this))
				(. this :set_anchor 0 y)
				(. buffer :paste (join
					(sort (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
					(ascii-char +char_lf) 2))))
		this)

	(defmethod :unique ()
		; (. edit :unique) -> edit
		(when (selection? this)
			(undoable
				(bind '(y y1) (select-lines this))
				(. this :set_anchor 0 y)
				(. buffer :paste (join
					(unique	(split (.-> buffer
						(:set_cursor 0 y1)
						(:cut 0 y)) (ascii-char +char_lf)))
					(ascii-char +char_lf) 2))))
		this)

	(defmethod :invert ()
		; (. edit :invert) -> edit
		(when (selection? this)
			(undoable
				(bind '(y y1) (select-lines this))
				(. this :set_anchor 0 y)
				(. buffer :paste (join
					(reverse (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
					(ascii-char +char_lf) 2))))
		this)

	(defmethod :comment ()
		; (. edit :comment) -> edit
		(undoable
			(bind '(y y1) (select-lines this))
			(. this :set_anchor 0 y)
			(each! (lambda (line)
				(task-slice)
				(cond
					((blank-line? line))
					((starts-with +block_comment line)
						(.-> buffer (:set_cursor 0 (!))
							(:delete (const (length +block_comment)))))
					((.-> buffer (:set_cursor 0 (!))
							(:insert +block_comment)))))
				(list (. buffer :get_text_lines)) y y1)
			(bind '(x y) (. buffer :get_cursor))
			(bind '(x y) (. buffer :constrain 0 (inc y)))
			(. this :set_cursor x y))
		this)

	(defmethod :set_select_color (col)
		; (. edit :set_select_color argb) -> edit
		(lower (:color_select (apply nums (map (lambda (_)
			(<< (canvas-from-argb32 col 15) 48)) +not_selected))))
		this)

	(defmethod :set_found_color (col)
		; (. edit :set_found_color argb) -> edit
		(lower (:color_found (apply nums (map (lambda (_)
			(<< (canvas-from-argb32 col 15) 48)) +not_selected))))
		this)

	(defmethod :set_region_color (col)
		; (. edit :set_region_color argb) -> edit
		(lower (:color_region (apply nums (map (lambda (_)
			(<< (canvas-from-argb32 col 15) 48)) +not_selected))))
		this)

	(defmethod :underlay_paper ()
		; (. edit :underlay_paper) -> edit
		;create the underlay for selections
		(raise :buffer :underlay :color_select :color_found :color_region
			:vdu_paper :scroll_x :scroll_y)
		(defq found (. buffer :get_found) scroll_y1 (second (. vdu_paper :vdu_size)))
		(bind '(fx fy fx1 fy1) (. this :get_find))
		(bind '(sx sy sx1 sy1) (sort-selection this))
		(clear underlay)
		(setq scroll_y1 (min (length found) (+ scroll_y1 scroll_y)) fy1 (dec fy1))
		(while (< scroll_y scroll_y1)
			(defq background (if (<= fy scroll_y fy1) color_region +not_selected)
				background (cond
					((and (defq match (elem-get found scroll_y)) (nempty? match))
						(defq cx 0 line (clear '()))
						(each (lambda (((x x1) &rest _))
							(push line
								(chunk background (- x cx))
								(chunk color_found (- x1 x)))
							(setq cx x1)) match)
						(push line (chunk background (max 0 (- +max_line_len cx))))
						(apply (const cat) line))
					(:t background)))
			(when (<= sy scroll_y sy1)
				(defq tl (inc (length (. buffer :get_text_line scroll_y))))
				(cond
					((= sy scroll_y sy1)
						(setq background (cat
							(overlay background 0 sx)
							(chunk color_select (- sx1 sx))
							(overlay background sx1 +max_line_len))))
					((= sy scroll_y)
						(setq background (cat
							(overlay background 0 sx)
							(chunk color_select (- tl sx))
							(overlay background tl +max_line_len))))
					((= sy1 scroll_y)
						(setq background (cat
							(chunk color_select sx1)
							(overlay background sx1 +max_line_len))))
					(:t (setq background (cat
							(chunk color_select tl)
							(overlay background tl +max_line_len))))))
			(push underlay background)
			(setq scroll_y (inc scroll_y)))
		(. vdu_paper :load underlay scroll_x 0 -1 -1)
		this)

	(defmethod :underlay_ink ()
		; (. edit :underlay_ink) -> edit
		;create the underlay for just bracket indicators
		(raise :buffer :underlay :color_select :vdu_ink :scroll_x :scroll_y)
		(clear underlay)
		(when (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(defq scroll_y1 (min (inc y1) (+ scroll_y (second (. vdu_ink :vdu_size)))))
				(while (< scroll_y scroll_y1)
					(push underlay (cond
						((= y scroll_y y1)
							(cat (chunk +not_selected x) +bracket_char
								(chunk +not_selected (- x1 x 1)) +bracket_char))
						((= y scroll_y)
							(cat (chunk +not_selected x) +bracket_char))
						((= y1 scroll_y)
							(cat (chunk +not_selected x1) +bracket_char))
						(:t +blank_line)))
					(setq scroll_y (inc scroll_y)))))
		(. vdu_ink :load underlay scroll_x 0 -1 -1)
		this)

	(defmethod :mouse_down (event)
		; (. edit :mouse_down event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(bind '(x y) (. buffer :constrain x y))
		(. buffer :set_cursor x y)
		(. this :set_anchor x y)
		this)

	(defmethod :mouse_move (event)
		; (. edit :mouse_move event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(bind '(x y) (. buffer :constrain x y))
		(. buffer :set_cursor x y)
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
