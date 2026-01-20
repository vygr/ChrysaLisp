;;;;;;;;;;;;;;;;;;
; doc buffer class
;;;;;;;;;;;;;;;;;;

(import "./buffer.inc")

;module
(env-push)

(defq +block_comment ";; " +bracket_char (nums 0x7f))

(defun lines-selected (selected)
	(defq cursors (list))
	(each (lambda ((sx sy sx1 sy1))
			(push cursors (. this :clip_cursor 0 sy1 0 sy)))
		selected)
	(. this :set_cursors cursors))

(defclass Document (&optional flags syntax) (Buffer flags syntax)
	; (Document [flags syntax]) -> document

	(defmethod :select_word ()
		; (. document :select_word) -> document
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy &ignore) csr)
					(defq line (elem-get buffer_lines cy)
						x (bskipn +char_class_not_whole_word line cx)
						x1 (rbskipn +char_class_not_whole_word line cx))
					(if (= x x1) csr (nums x cy x1 cy -1)))
				(get :cursors this)))))
		this)

	(defmethod :select_line ()
		; (. document :select_line) -> document
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy &ignore) (csr-floor csr))
					(nums cx (inc cy) cx cy -1))
				(get :cursors this)))))
		this)

	(defmethod :select_paragraph ()
		; (. document :select_paragraph) -> document
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy &ignore)) (task-slice)
					(defq y (some! (# (if (blank-line? %0) (inc (!))))
							(list buffer_lines) :nil cy 0)
						y1 (some! (# (if (blank-line? %0) (!)))
							(list buffer_lines) :nil cy (bind '(w h) (. this :get_size))))
					(setd y 0 y1 h)
					(nums 0 y1 0 y -1))
				(get :cursors this)))))
		this)

	(defmethod :select_block ()
		; (. document :select_block) -> document
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy &ignore) csr)
					(if (bind '(x y) (. this :find_left_bracket cx cy))
						(if (bind '(x1 y1) (. this :find_right_bracket cx cy))
							(nums (inc x1) y1 x y -1)
							csr)
						csr))
				(get :cursors this)))))
		this)

	(defmethod :select_all ()
		; (. document :select_all) -> document
		(. this :set_cursor 0 (dec (length (get :buffer_lines this))) 0 0))

	(defmethod :tab ()
		; (. document :tab) -> document
		(undoable this
			(defq tab_width (. this :get_tab_width))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(cx cy &ignore) (csr-sort (nums cx cy ax ay)))
					(. this :iinsert cx cy
						(pad "" (- tab_width (% cx tab_width)) "        ")))
				(. this :merge_cursors (. this :get_cursors)))
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors)))
		this)

	(defmethod :left_tab ()
		; (. document :left_tab) -> document
		(undoable this
			(defq tab_width (. this :get_tab_width)
				selected (map (const csr-floor) (. this :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(defq n (min tab_width (bskip " " (. this :get_text_line sy) 0)))
						(. this :idelete 0 sy n sy)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :right_tab ()
		; (. document :right_tab) -> document
		(undoable this
			(defq tab (pad "" (. this :get_tab_width) "        ")
				selected (map (const csr-floor) (. this :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(. this :iinsert 0 sy tab)
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :to_lower ()
		; (. document :to_lower) -> document
		(undoable this
			(defq cursors (. this :get_cursors))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(sx sy &ignore) (csr-sort (nums cx cy ax ay)))
					(. this :iinsert sx sy
						(to-lower (. this :icopy cx cy ax ay))))
				(. this :merge_cursors cursors))
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors))
			(. this :set_cursors cursors))
		this)

	(defmethod :to_upper ()
		; (. document :to_upper) -> document
		(undoable this
			(defq cursors (. this :get_cursors))
			(reach (lambda ((cx cy ax ay &ignore))
					(bind '(sx sy &ignore) (csr-sort (nums cx cy ax ay)))
					(. this :iinsert sx sy
						(to-upper (. this :icopy cx cy ax ay))))
				(. this :merge_cursors cursors))
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors))
			(. this :set_cursors cursors))
		this)

	(defmethod :reflow ()
		; (. document :reflow) -> document
		(undoable this
			(. this :select_paragraph)
			(reach (lambda ((sx1 sy1 sx sy &ignore))
				(when (/= sy sy1)
					(defq lines (.-> this :get_syntax_engine
							(:text_flow (split (. this :icopy 0 sy 0 sy1) +char_class_white_space)
								(. this :get_wrap_width))))
					(.-> this
						(:idelete 0 sy 0 sy1)
						(:iinsert 0 sy (join lines +char_class_lf 2)))))
				(. this :get_selected)))
		this)

	(defmethod :split ()
		; (. document :split) -> document
		(undoable this
			(. this :select_paragraph)
			(reach (lambda ((sx1 sy1 sx sy &ignore))
				(when (/= sy sy1)
					(defq lines (split (. this :icopy 0 sy 0 sy1) +char_class_white_space))
					(.-> this
						(:idelete 0 sy 0 sy1)
						(:iinsert 0 sy (join lines +char_class_lf 2)))))
				(. this :get_selected)))
		this)

	(defmethod :sort ()
		; (. document :sort) -> document
		(undoable this
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. this :get_text_line y))
						(++ y))
					(setq lines (reverse (sort lines)) y sy)
					(while (< y sy1)
						(.-> this
							(:idelete 0 y (dec (length (. this :get_text_line y))) y)
							(:iinsert 0 y (most (pop lines))))
						(++ y)))
				(. this :get_selected))
			(lines-selected (. this :get_selected)))
		this)

	(defmethod :invert ()
		; (. document :invert) -> document
		(undoable this
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. this :get_text_line y))
						(++ y))
					(setq y sy)
					(while (< y sy1)
						(.-> this
							(:idelete 0 y (dec (length (. this :get_text_line y))) y)
							(:iinsert 0 y (most (pop lines))))
						(++ y)))
				(. this :get_selected))
			(lines-selected (. this :get_selected)))
		this)

	(defmethod :unique ()
		; (. document :unique) -> document
		(undoable this
			(. this :floor_selection)
			(reach (lambda ((sx sy sx1 sy1 &ignore))
					(defq lines (list) y sy)
					(while (< y sy1)
						(push lines (. this :get_text_line y))
						(++ y))
					(setq lines (reverse (unique lines)) y sy)
					(while (< y sy1)
						(if (defq line (pop lines))
							(.-> this
								(:idelete 0 y (dec (length (. this :get_text_line y))) y)
								(:iinsert 0 y (most line)))
							(progn
								(. this :idelete 0 y 0 sy1)
								(setq y sy1)))
						(++ y)))
				(. this :get_selected))
			(lines-selected (. this :get_selected)))
		this)

	(defmethod :comment ()
		; (. document :comment) -> document
		(undoable this
			(defq selected (map (const csr-floor) (. this :get_selected)))
			(each (lambda ((sx sy sx1 sy1 &ignore))
					(until (= sy sy1)
						(defq line (. this :get_text_line sy))
						(cond
							((blank-line? line))
							((starts-with +block_comment line)
								(. this :idelete 0 sy (length +block_comment) sy))
							((. this :iinsert 0 sy +block_comment)))
						(++ sy)))
				selected)
			(lines-selected selected))
		this)

	(defmethod :trim ()
		; (. document :comment) -> document
		(when (> (bind '(w h) (. this :get_size)) 0)
			(undoable this
				;trim front
				(defq y (some! (# (if (blank-line? %0) :nil (!)))
					(list (. this :get_buffer_lines)) :nil 0 h))
				(setd y h)
				(. this :idelete 0 0 0 y)
				;trim end
				(defq y (some! (# (if (blank-line? %0) :nil (inc (!))))
					(list (. this :get_buffer_lines)) :nil (bind '(w h) (. this :get_size)) 0))
				(setd y 0)
				(. this :idelete 0 y 0 h)
				;trim lines
				(each! (lambda (line)
						(defq l (dec (length line)))
						(when (< (defq si (rbskip +char_class_white_space line -1)) l)
							(defq ei (- l si))
							(. this :idelete si (!) (+ si ei) (!))))
					(list (. this :get_buffer_lines)) 0 (bind '(w h) (. this :get_size)))))
		this)
	)

;module
(export-classes '(Document))
(env-pop)