;;;;;;;;;;;;;
; edit widget
;;;;;;;;;;;;;

(import "gui/vdu/lisp.inc")
(import "gui/canvas/lisp.inc")
(import "lib/text/buffer.inc")

;module
(env-push)

(defq +not_whole_word_chars " .,;'`(){}[]/" +block_comment ";; "
	+not_selected (apply nums (map (lambda (_) 0) (str-alloc 8192)))
	+bracket_char (nums 0x7f))

(defmacro not-whole-word-char? (_)
	`(find ,_ +not_whole_word_chars))

(defun blank-line? (line)
	(every (# (eql " " %0)) line))

(defun selection? (this)
	(bind '(cx cy) (. this :get_cursor))
	(bind '(ax ay) (. this :get_anchor))
	(or (/= ax cx) (/= ay cy)))

(defun sort-selection (this)
	(bind '(cx cy) (. this :get_cursor))
	(bind '(ax ay) (. this :get_anchor))
	(defq sx cx sy cy sx1 ax sy1 ay)
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
		x (some! cx 0 nil (#
			(if (not-whole-word-char? %0) (inc _))) (list line))
		x1 (some! cx -1 nil (#
		   (if (not-whole-word-char? %0) _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1))

(defun select-paragraph (this)
	(defq buffer (. this :get_buffer))
	(bind '(cx cy) (. this :get_cursor))
	(defq y (some! cy 0 nil
				(# (if (blank-line? %0) (inc _)))
			(list (. buffer :get_text_lines)))
		y1 (some! cy (bind '(w h) (. buffer :get_size)) nil
				(# (if (blank-line? %0) _))
			(list (. buffer :get_text_lines))))
	(unless y (setq y 0))
	(unless y1 (setq y1 h))
	(list y y1))

(defmacro undoable (this buffer &rest _)
	`(progn
		(setd buffer (. this :get_buffer))
		(bind '(cx cy) (. this :get_cursor))
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor cx cy))
		~_
		(. buffer :push_undo (list :mark mark))))

(defclass Edit () (View)
	(def this :vdu_text (defq vdu_text (Vdu))
		:vdu_underlay (defq vdu_underlay (Vdu))
		:anchor_x 0 :anchor_y 0
		:scroll_x 0 :scroll_y 0
		:buffer (Buffer t) :underlay (list)
		:selected +not_selected
		:vdu_width 80 :vdu_height 40
		:min_width 0 :min_height 0)
	(. vdu_text :set_flags 0 +view_flag_solid)
	(. vdu_underlay :set_flags 0 +view_flag_solid)
	(.-> this (:add_child vdu_text) (:add_child vdu_underlay))

	(defmethod :max_size (this)
		; (. edit :max_size) -> (width height)
		(bind '(w h) (. this :get_size))
		(bind '(cw ch) (. (get :vdu_text this) :char_size))
		(list (/ w cw) (/ h ch)))

	(defmethod :pref_size (this)
		; (. edit :pref_size) -> (width height)
		(defq min_w (def? :min_width this) min_h (def? :min_height this)
			vdu_w (get :vdu_width this) vdu_h (get :vdu_height this)
			w 0 h 0)
		(each (lambda (child)
				(def child
					:min_width min_w :min_height min_h
					:vdu_width vdu_w :vdu_height vdu_h)
				(bind '(cw ch) (. child :pref_size))
				(setq w (max w cw) h (max h ch)))
			(. this :children))
		(list w h))

	(defmethod :layout (this)
		; (. edit :layout) -> edit
		(defq min_w (def? :min_width this) min_h (def? :min_height this)
			vdu_w (get :vdu_width this) vdu_h (get :vdu_height this))
		(bind '(w h) (. this :get_size))
		(each (lambda (child)
				(def child
					:min_width min_w :min_height min_h
					:vdu_width vdu_w :vdu_height vdu_h)
				(. child :change 0 0 w h t))
			(. this :children))
		this)

	(defmethod :get_buffer (this)
		; (. edit :get_buffer) -> text_buffer
		(get :buffer this))

	(defmethod :get_vdu_text (this)
		; (. edit :get_vdu_text) -> vdu_text
		(get :vdu_text this))

	(defmethod :get_cursor (this)
		; (. edit :get_cursor) -> (list x y)
		(raise :buffer)
		(. buffer :get_cursor))

	(defmethod :get_anchor (this)
		; (. edit :get_anchor) -> (list x y)
		(list (get :anchor_x this) (get :anchor_y this)))

	(defmethod :get_scroll (this)
		; (. edit :get_scroll) -> (list x y)
		(list (get :scroll_x this) (get :scroll_y this)))

	(defmethod :set_buffer (this buffer)
		; (. edit :set_buffer text_buffer) -> this
		(lower :buffer) this)

	(defmethod :set_cursor (this x y)
		; (. edit :set_cursor x y) -> this
		(raise :buffer)
		(. buffer :set_cursor x y) this)

	(defmethod :set_anchor (this anchor_x anchor_y)
		; (. edit :set_anchor x y) -> this
		(lower :anchor_x :anchor_y) this)

	(defmethod :set_scroll (this scroll_x scroll_y)
		; (. edit :set_scroll x y) -> this
		(lower :scroll_x :scroll_y) this)

	(defmethod :char_pos (this event)
		; (. edit :char_pos event) -> (list x y)
		(raise :scroll_x :scroll_y :vdu_text)
		(bind '(w h) (. vdu_text :char_size))
		(defq x (getf event +ev_msg_mouse_rx) y (getf event +ev_msg_mouse_ry))
		(setq x (if (>= x 0) x (- x w)) y (if (>= y 0) y (- y h)))
		(list (+ scroll_x (/ x w)) (+ scroll_y (/ y h))))

	(defmethod :clear_selection (this)
		; (. edit :clear_selection) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor cx cy) (:set_anchor cx cy)))

	(defmethod :home (this)
		; (. edit :home) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor 0 cy) (:set_anchor 0 cy)))

	(defmethod :end (this)
		; (. edit :end) -> edit
		(raise :buffer)
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(cx cy) (. buffer :constrain +max_int cy))
		(.-> this (:set_cursor cx cy) (:set_anchor cx cy)))

	(defmethod :left (this)
		; (. edit :left) -> edit
		(cond
			((selection? this)
				(bind '(sx sy sx1 sy1) (sort-selection this))
				(. this :set_cursor sx sy))
			(t  (. (get :buffer this) :left)))
		(. this :clear_selection))

	(defmethod :right (this)
		; (. edit :right) -> edit
		(cond
			((selection? this)
				(bind '(sx sy sx1 sy1) (sort-selection this))
				(. this :set_cursor sx1 sy1))
			(t  (. (get :buffer this) :right)))
		(. this :clear_selection))

	(defmethod :up (this)
		; (. edit :up) -> edit
		(when (selection? this)
			(bind '(sx sy sx1 sy1) (sort-selection this))
			(. this :set_cursor sx sy))
		(. (get :buffer this) :up)
		(. this :clear_selection))

	(defmethod :down (this)
		; (. edit :down) -> edit
		(when (selection? this)
			(bind '(sx sy sx1 sy1) (sort-selection this))
			(. this :set_cursor sx1 sy1))
		(. (get :buffer this) :down)
		(. this :clear_selection))

	(defmethod :home_select (this)
		; (. edit :home_select) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(. this :set_cursor 0 cy))

	(defmethod :end_select (this)
		; (. edit :end_select) -> edit
		(raise :buffer)
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(cx cy) (. buffer :constrain +max_int cy))
		(. this :set_cursor cx cy))

	(defmethod :left_select (this)
		; (. edit :left_select) -> edit
		(. (get :buffer this) :left)
		this)

	(defmethod :right_select (this)
		; (. edit :right_select) -> edit
		(. (get :buffer this) :right)
		this)

	(defmethod :up_select (this)
		; (. edit :up_select) -> edit
		(. (get :buffer this) :up)
		this)

	(defmethod :down_select (this)
		; (. edit :down_select) -> edit
		(. (get :buffer this) :down)
		this)

	(defmethod :left_bracket (this)
		; (. edit :left_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :left_bracket))
			(. this :set_cursor cx cy))
		(. this :clear_selection))

	(defmethod :right_bracket (this)
		; (. edit :right_bracket) -> edit
		(if (bind '(cx cy) (. (get :buffer this) :right_bracket))
			(. this :set_cursor cx cy))
		(. this :clear_selection))

	(defmethod :select_all (this)
		; (. edit :select_all) -> edit
		(bind '(w h) (. (get :buffer this) :get_size))
		(.-> this (:set_cursor 0 h) (:set_anchor 0 0)))

	(defmethod :select_word (this)
		; (. edit :select_word) -> edit
		(bind '(x x1) (select-word this))
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor x1 cy) (:set_anchor x cy)))

	(defmethod :select_line (this)
		; (. edit :select_line) -> edit
		(bind '(cx cy) (. this :get_cursor))
		(.-> this (:set_cursor 0 (inc cy)) (:set_anchor 0 cy)))

	(defmethod :select_paragraph (this)
		; (. edit :select_paragraph) -> edit
		(bind '(y y1) (select-paragraph this))
		(.-> this (:set_cursor 0 y1) (:set_anchor 0 y)))

	(defmethod :select_block (this)
		; (. edit :select_block) -> edit
		(raise :buffer)
		(if (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(.-> this (:set_cursor (inc x1) y1) (:set_anchor x y))))
		this)

	(defmethod :insert (this string)
		; (. edit :insert string) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(. buffer :cut ax ay)
		(. buffer :insert string)
		(. this :clear_selection))

	(defmethod :backspace (this)
		; (. edit :backspace) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(when (eql (. buffer :cut ax ay) "")
			(. buffer :backspace))
		(. this :clear_selection))

	(defmethod :delete (this)
		; (. edit :delete) -> edit
		(raise :buffer)
		(bind '(ax ay) (. this :get_anchor))
		(when (eql (. buffer :cut ax ay) "")
			(. buffer :delete))
		(. this :clear_selection))

	(defmethod :break (this)
		; (. edit :break) -> edit
		(undoable this nil
			(bind '(ax ay) (. this :get_anchor))
			(. buffer :cut ax ay)
			(bind '(x y) (.-> buffer :break :get_cursor))
			(defq line (. buffer :get_text_line (dec y))
				indent (some (# (unless (eql %0 (ascii-char +char_space)) _)) line))
			(unless indent (setq indent (length line)))
			(if (> indent 0) (. buffer :insert (pad "" indent "        "))))
		(. this :clear_selection))

	(defmethod :tab (this)
		; (. edit :tab) -> edit
		(undoable this nil
			(defq tab_width (. buffer :get_tab_width))
			(. buffer :cut ax ay)
			(bind '(cx cy) (. buffer :get_cursor))
			(. buffer :insert (pad "" (- tab_width (% cx tab_width)) "        ")))
		(. this :clear_selection))

	(defmethod :left_tab (this)
		; (. edit :left_tab) -> edit
		(undoable this nil
			(defq tab_width (. buffer :get_tab_width))
			(bind '(y y1) (select-lines this))
			(each! y y1 (lambda (line)
					(task-slice)
					(defq idx -1)
					(while (and
						(< (setq idx (inc idx)) (length line))
						(< idx tab_width)
						(eql (elem-get idx line) " ")))
					(if (> idx 0)
						(.-> buffer (:set_cursor 0 _) (:delete (min tab_width idx)))))
				(list (. buffer :get_text_lines)))
			(. this :set_cursor cx cy))
		this)

	(defmethod :right_tab (this)
		; (. edit :right_tab) -> edit
		(undoable this nil
			(defq tab (pad "" (. buffer :get_tab_width) "        "))
			(bind '(y y1) (select-lines this))
			(each! y y1 (lambda (line)
					(task-slice)
					(unless (eql line "")
						(.-> buffer (:set_cursor 0 _) (:insert tab))))
				(list (. buffer :get_text_lines)))
			(. this :set_cursor cx cy))
		this)

	(defmethod :to_lower (this)
		; (. edit :to_lower) -> edit
		(when (selection? this)
			(undoable this nil
				(bind '(ax ay) (. this :get_anchor))
				(. buffer :paste (to-lower (. buffer :cut ax ay)))))
		this)

	(defmethod :to_upper (this)
		; (. edit :to_upper) -> edit
		(when (selection? this)
			(undoable this nil
				(bind '(ax ay) (. this :get_anchor))
				(. buffer :paste (to-upper (. buffer :cut ax ay)))))
		this)

	(defmethod :reflow (this)
		; (. edit :reflow) -> edit
		(undoable this nil
			(bind '(y y1) (select-paragraph this))
			(each (lambda (line)
					(task-slice)
					(.-> buffer (:insert line) :break))
				(.-> buffer :get_syntax (:text_flow
					(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1))
						(const (cat " " (ascii-char +char_lf))))
					(. buffer :get_wrap_width))))
			(bind '(x y) (. buffer :get_cursor))
			(bind '(x y) (. buffer :constrain x (inc y)))
			(.-> this (:set_cursor x y) (:set_anchor x y)))
		this)

	(defmethod :ordered (this)
		; (. edit :ordered) -> edit
		(when (selection? this)
			(undoable this nil
				(bind '(y y1) (select-lines this))
				(. this :set_anchor 0 y)
				(. buffer :paste (cat (join
					(sort cmp (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
					(ascii-char +char_lf)) (ascii-char +char_lf)))))
		this)

	(defmethod :ordered_unique (this)
		; (. edit :ordered_unique) -> edit
		(when (selection? this)
			(undoable this nil
				(bind '(y y1) (select-lines this))
				(. this :set_anchor 0 y)
				(. buffer :paste (cat (join
					(reduce (# (if (or (<= (length %0) 0)
									(not (eql (elem-get -2 %0) %1))) (push %0 %1) %0))
					(sort cmp (split (.-> buffer (:set_cursor 0 y1) (:cut 0 y)) (ascii-char +char_lf)))
					(list)) (ascii-char +char_lf)) (ascii-char +char_lf)))))
		this)

	(defmethod :comment (this)
		; (. edit :comment) -> edit
		(undoable this nil
			(bind '(y y1) (select-lines this))
			(. this :set_anchor 0 y)
			(each! y y1 (lambda (line)
					(task-slice)
					(cond
						((blank-line? line))
						((starts-with +block_comment line)
							(.-> buffer (:set_cursor 0 _)
								(:delete (const (length +block_comment)))))
						((.-> buffer (:set_cursor 0 _)
								(:insert +block_comment)))))
				(list (. buffer :get_text_lines)))
			(bind '(x y) (. buffer :get_cursor))
			(bind '(x y) (. buffer :constrain 0 (inc y)))
			(. this :set_cursor x y))
		this)

	(defmethod :set_underlay_color (this col)
		; (. edit :set_underlay_color argb) -> edit
		(lower (:selected (apply nums (map (lambda (_)
			(<< (canvas-from-argb32 col 15) 48)) +not_selected))))
		this)

	(defmethod :underlay_selection (this)
		; (. edit :underlay_selection) -> edit
		(raise :buffer :underlay :selected :vdu_underlay :scroll_x :scroll_y)
		(bind '(cx cy) (. this :get_cursor))
		(bind '(ax ay) (. this :get_anchor))
		(defq x1 ax y1 ay)
		(if (> cy y1) (defq st cx cx x1 x1 st st cy cy y1 y1 st))
		(and (= cy y1) (> cx x1) (defq st cx cx x1 x1 st))
		(cap (inc y1) (clear underlay))
		(defq uy -1 buffer (. buffer :get_text_lines))
		(while (< (setq uy (inc uy)) cy) (push underlay ""))
		(cond
			((= cy y1)
				(push underlay (cat (slice 0 cx +not_selected) (slice cx x1 selected))))
			(t  (push underlay (cat
					(slice 0 cx +not_selected)
					(slice cx (inc (length (elem-get cy buffer))) selected)))
				(while (< (setq cy (inc cy)) y1)
					(push underlay (slice 0 (inc (length (elem-get cy buffer))) selected)))
				(push underlay (slice 0 x1 selected))))
		(. vdu_underlay :load underlay scroll_x scroll_y -1 -1)
		this)

	(defmethod :underlay_brackets (this)
		; (. edit :underlay_brackets) -> edit
		;create the underlay for just bracket indicators
		(raise :buffer :underlay :selected :vdu_underlay :scroll_x :scroll_y)
		(clear underlay)
		(when (bind '(x y) (. buffer :left_bracket))
			(when (bind '(x1 y1) (. buffer :right_bracket))
				(cap (inc y1) underlay)
				(defq uy -1)
				(while (< (setq uy (inc uy)) y) (push underlay ""))
				(cond
					((= y y1)
						(push underlay (cat
							(slice 0 x +not_selected) +bracket_char
							(slice x (dec x1) +not_selected) +bracket_char)))
					(t  (push underlay (cat (slice 0 x +not_selected) +bracket_char))
						(while (< (setq y (inc y)) y1) (push underlay ""))
						(push underlay (cat (slice 0 x1 +not_selected) +bracket_char))))))
		(. vdu_underlay :load underlay scroll_x scroll_y -1 -1)
		this)

	(defmethod :underlay_clear (this)
		; (. edit :underlay_clear) -> edit
		;create the underlay for clear
		(raise :underlay :vdu_underlay :scroll_x :scroll_y)
		(clear underlay)
		(. vdu_underlay :load underlay scroll_x scroll_y -1 -1)
		this)

	(defmethod :mouse_down (this event)
		; (. edit :mouse_down event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(bind '(x y) (. buffer :constrain x y))
		(. buffer :set_cursor x y)
		(. this :set_anchor x y)
		this)

	(defmethod :mouse_move (this event)
		; (. edit :mouse_move event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(bind '(x y) (. buffer :constrain x y))
		(. buffer :set_cursor x y)
		this)

	(defmethod :mouse_wheel (this event)
		; (. edit :mouse_wheel event) -> edit
		(raise :scroll_x :scroll_y)
		(setq scroll_x (+ scroll_x (getf event +ev_msg_wheel_x))
			scroll_y (- scroll_y (getf event +ev_msg_wheel_y)))
		(lower :scroll_x :scroll_y)
		this)
	)

;module
(export-classes
	Edit)
(env-pop)
