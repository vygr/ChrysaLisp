;Visual buffer input
; TODO: custom margins and line wrap
(structure +mouse 0
	(byte up down double_click long_click hover))

(defq text_select (xmap-kv
		:mouse_state +mouse_up
		:selection_mode nil
		:highlighted nil
		:selection (list)
		:clipboard (list)
		:copied (list)))

(defun sort-xy ((fx fy) (lx ly))
	(if (or (< fy ly) (and (= ly fy) (< fx lx)))
		(list fx fy lx ly)
		(list lx ly fx fy)))

(defun inbounds (y seq)
	(max 0 (min y (length seq))))

(defun single? (seq)
	(if (= (length seq) 1) t nil))

(defun get-buffer ()
	(elem +text_buffer current_text))

(defun get-position ()
	(elem +text_position current_text))

(defun get-cursor ()
	(list (elem +pos_cx (get-position)) (elem +pos_cy (get-position))))

(defun set-buffer (buf)
	(elem-set +text_buffer current_text buf))

(defun set-position (ox oy cx cy sx)
	(cursor-visible)
	(elem-set +text_position current_text (list ox oy cx cy sx)))

(defun set-cursor (cx cy)
	(elem-set +pos_cx (get-position) cx)
	(elem-set +pos_cy (get-position) cy)
	(list cx cy))

(defun vdu-keyboard (msg)
	(and (= (getf msg +ev_msg_type) +ev_type_key)
		(> (getf msg +ev_msg_key_keycode) 0)
		(vdu-input (getf msg +ev_msg_key_key))
		(. text_select :insert :highlighted nil)))

(defun vdu-mouse-down (msg)
	(when (and (= (getf msg +ev_msg_type) +ev_type_mouse)
			(/= (getf msg +ev_msg_mouse_buttons) 0)
		(defq mxy (list (getf msg +ev_msg_mouse_rx)
				(getf msg +ev_msg_mouse_ry))))
		(defq buffer (get-buffer))
		(bind '(ox oy _ _ sx) (get-position))
		(bind '(cx cy) (mouse-xy (get-buffer) mxy))
		(cursor-visible)
		(set-cursor cx cy)
		(when (= (. text_select :find :mouse_state) +mouse_up)
			(. text_select :insert :mouse_state +mouse_down)
			(. text_select :insert :selection (list (get-cursor) (get-cursor))))
		(pop 	(. text_select :find :selection))
		(push 	(. text_select :find :selection) (get-cursor))
		(. text_select :insert :selection_mode t)
		(vdu-colorise)))

(defun vdu-mouse-up (msg)
	(when (and (= (. text_select :find :mouse_state) +mouse_down)
		(= (getf msg +ev_msg_type) +ev_type_mouse)
		(= (getf msg +ev_msg_mouse_buttons) 0))
		(. text_select :insert :mouse_state +mouse_up)
		(. text_select :insert :selection_mode nil)))

(defun on-paste (msg)
	(bind '(cx cy) (get-cursor))
	(defq buffer (paste-text (get-buffer) cx cy msg))
	(set-buffer buffer)
	(vdu-colorise))

(defun mouse-xy (buf mxy)
	(bind '(ox oy cx cy sx) (get-position))
	(defq cxy (map + (map / mxy (. vdu :char_size)) (list ox oy))
			cx (first cxy) cy (second cxy))
	(cond 
		((>= cy (length (get-buffer))) (setq cy (dec (length (get-buffer)))))
		((< cy 0) (setq cy 0)))
	(cond 
		((> cx (length (elem cy (get-buffer)))) (setq cx (length (elem cy (get-buffer)))))
		((< cx 0) (setq cx 0 sx 0)))
	(list cx cy))

(defun copy-text (seq (fxy lxy))
	(bind '(fx fy lx ly) (sort-xy fxy lxy))
	(defq selection (list))
	(each (lambda (ln)
		(defq line (elem ln seq) start 0 stop -1)
		(when (= ln fy)
			(setq start (max fx 0)))
		(when (= ln ly)
			(setq stop (min lx (length line))))
		(push selection (setq line (slice start stop line))))
		(range fy (inc ly)))
	selection)

;cx will either shift in a single line paste or stay the same.
(defun paste-text (seq cx cy seq_sel)
	(defq seq_front (slice 0 cy seq) seq_back (slice (inc cy) -1 seq)
			line_front (slice 0 cx (elem cy seq))
			line_back (slice cx -1 (elem cy seq)) new_sel (list))
	(each (lambda (line)
		(cond
			((= (length seq_sel) 1)
				(push new_sel (cat line_front (first seq_sel) line_back))
				(setq cx (+ cx (length (first seq_sel)))))
			((= _ 0)
				(push new_sel (cat line_front line))
				(setq cy (inc cy)))
			((= _ (dec (length seq_sel)))
				(push new_sel (cat line line_back))
				(setq cx (length line)))
			(t 	(setq cy (inc cy))
				(push new_sel line))))
		seq_sel)
	(set-cursor cx cy)
	(cat seq_front new_sel seq_back))

(defun cut-text (seq (fxy lxy))
	(bind '(fx fy lx ly) (sort-xy fxy lxy))
	(defq seq_sel (list) seq_front (slice 0 fy seq)
			seq_back (slice (min (inc ly) (length seq)) -1 seq)
			first_line_front (slice 0 fx (elem fy seq))
			last_line_back (slice (min lx (length (elem ly seq))) -1 (elem ly seq)))
	(cat seq_front (list (cat first_line_front last_line_back)) seq_back))

;;;;;;;;;;;;;;
; buffer input
;;;;;;;;;;;;;;
(defun printable (c)
	;insert the tab/char at cursor or append to end etc
	(defq line (if (>= cy (length buffer)) "" (elem cy buffer))
		line (insert line (min cx (length line)) (char c)))
	(setq cx (inc cx))
	(if (>= cy (length buffer))
		(push buffer line)
		(elem-set cy buffer line)))

(defun return ()
;return key
	(cond
		((>= cy (length buffer))
			;off end of text so just append a blank line
			(push buffer "")
			(setq cy (length buffer)))
		(t	;break this line
			(defq line (elem cy buffer)
				line_front (slice 0 (min cx (length line)) line)
				line_back (slice (min cx (length line)) -1 line))
					(elem-set cy buffer line_front)
					(setq cy (min (inc cy) (length buffer))
						buffer (insert buffer cy (list line_back))))))

(defun backspace ()
	(cond
		((> cx 0)
			(elem-set cy buffer (erase (elem cy buffer)
				(dec cx) cx))
			(setq cx (dec cx)))
		((<= cx 0)
			(unless (<= cy 0) 
				;backspace into previous line
				(defq prev_line (elem (dec cy) buffer)
					cat_line (cat prev_line (elem cy buffer)))
				(setq cx (length prev_line) cy (dec cy))
				(elem-set cy buffer cat_line)
				(setq buffer (erase buffer (inc cy) (+ cy 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
; editor cursor behavior
;;;;;;;;;;;;;;;;;;;;;;;;

(defun cursor-visible ()
	(cond
		((< cx ox)
			(setq ox cx))
		((>= cx (+ ox vdu_width))
			(setq ox (- cx vdu_width -1))))
	(cond
		((< cy oy)
			(setq oy cy))
		((>= cy (+ oy vdu_height))
			(setq oy (- cy vdu_height -1))))
	(list ox oy))

(defun set-sticky ()
	(defq line (elem cy buffer))
	(cond
		((and (> cx sx) (<= sx (length line)))
			(setq cx sx))
		((>= cx (length line))
			(setq cx (length line)))))

(defun left ()
	(cond
		((<= cx 0)
			(cond
				((<= cy 0)
					(setq cy 0 cx 0))
				((> cy 0)
					(setq cy (dec cy) cx (setq cx 
						(length (elem cy buffer)))))))
		((> cx 0)
			(setq cx (dec cx)))))

(defun right ()
	(cond
		((>= cx (length (elem cy buffer)))
			(cond
				((>= cy (dec (length buffer)))
					(setq cx (length (elem cy buffer))))
				((< cy (dec (length buffer)))
					(setq cy (inc cy) cx (setq cx 0)))))
		((< cx (length (elem cy buffer)))
			(setq cx (inc cx)))))

(defun up ()
	(cond
		((<= cy 0)
			(setq cx 0))
		((> cy 0)
			(setq cy (dec cy) cx (min sx (length (elem cy buffer)))))))

(defun down ()
	(cond
		((>= cy (dec (length buffer)))
			(setq cy (dec (length buffer)) cx (length (elem cy buffer)) sx (length (elem cy buffer))))
		((< cy (dec (length buffer)))
			(setq cy (inc cy) cx (min sx (length (elem cy buffer)))))))
