;;;;;;;;;;;;;;;;;;
; doc buffer class
;;;;;;;;;;;;;;;;;;

(import "./buffer.inc")

;module
(env-push)

(defq +block_comment ";; ")

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
						cx (rbskipn +char_class_not_whole_word line cx))
					(if (= cx x) csr (nums x cy cx cy -1)))
				(get :cursors this)))))
		this)

	(defmethod :select_line ()
		; (. document :select_line) -> document
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(& & cx cy &ignore) (csr-floor csr))
					(nums 0 (inc cy) 0 cy -1))
				(get :cursors this)))))
		this)

	(defmethod :select_paragraph ()
		; (. document :select_paragraph) -> document
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((& cy &ignore)) (task-slice)
					(defq y (some! (# (if (blank-line? %0) (inc (!))))
							(list buffer_lines) :nil cy 0)
						cy (some! (# (if (blank-line? %0) (!)))
							(list buffer_lines) :nil cy (bind '(& h) (. this :get_size))))
					(setd y 0 cy h)
					(nums 0 cy 0 y -1))
				(get :cursors this)))))
		this)

	(defmethod :select_block ()
		; (. document :select_block) -> document
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy &ignore) csr)
					(if (bind '(x y) (. this :find_left_bracket cx cy))
						(if (bind '(cx cy) (. this :find_right_bracket cx cy))
							(nums (inc cx) cy x y -1)
							csr)
						csr))
				(get :cursors this)))))
		this)

	(defmethod :select_form ()
		; (. document :select_form) -> document
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy &ignore) csr)
					(defq line (elem-get buffer_lines cy))
					(cond
						((find (elem-get line cx) "()")
							(if (bind '(x y) (. this :find_left_bracket cx cy))
								(if (bind '(cx cy) (. this :find_right_bracket cx cy))
									(nums (inc cx) cy x y -1)
									csr)
								csr))
						((defq x (bskipn +char_class_not_whole_word line cx)
								cx (rbskipn +char_class_not_whole_word line cx))
							(if (= cx x) csr (nums x cy cx cy -1)))))
				(get :cursors this)))))
		this)

	(defmethod :select_all ()
		; (. document :select_all) -> document
		(. this :set_cursor 0 (dec (length (get :buffer_lines this))) 0 0))

	(defmethod :tab ()
		; (. document :tab) -> document
		(undoable this
			(raise :tab_width)
			(reach (lambda (csr)
					(bind '(cx cy &ignore) (csr-sort csr))
					(. this :iinsert cx cy
						(pad "" (- tab_width (% cx tab_width)) "        ")))
				(. this :merge_cursors (. this :get_cursors)))
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors)))
		this)

	(defmethod :left_tab ()
		; (. document :left_tab) -> document
		(undoable this
			(raise :tab_width)
			(. this :floor_selection)
			(each (lambda ((& ay & cy &ignore))
					(until (= cy ay)
						(. this :idelete 0 cy
							(min tab_width (bskip " " (. this :get_text_line cy) 0)) cy)
						(++ cy)))
					(. this :get_cursors)))
		this)

	(defmethod :right_tab ()
		; (. document :right_tab) -> document
		(undoable this
			(. this :floor_selection)
			(defq tab (pad "" (. this :get_tab_width) "        "))
			(raise :cursors)
			(each (lambda ((& ay & cy &ignore))
					(until (= cy ay) (. this :iinsert 0 cy tab) (++ cy)))
				cursors)
			(lower :cursors))
		this)

	(defmethod :to_lower ()
		; (. document :to_lower) -> document
		(undoable this
			(raise :cursors)
			(reach (lambda (csr)
					(bind '(cx cy ax ay &ignore) csr)
					(bind '(sx sy &ignore) (csr-sort-top csr))
					(. this :iinsert sx sy (to-lower (. this :icopy cx cy ax ay))))
				cursors)
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors))
			(lower :cursors))
		this)

	(defmethod :to_upper ()
		; (. document :to_upper) -> document
		(undoable this
			(raise :cursors)
			(reach (lambda (csr)
					(bind '(cx cy ax ay &ignore) csr)
					(bind '(sx sy &ignore) (csr-sort-top csr))
					(. this :iinsert sx sy (to-upper (. this :icopy cx cy ax ay))))
				cursors)
			(reach (lambda ((cx cy ax ay &ignore)) (. this :idelete cx cy ax ay))
				(. this :get_cursors))
			(lower :cursors))
		this)

	(defmethod :reflow ()
		; (. document :reflow) -> document
		(undoable this
			(. this :select_paragraph)
			(reach (lambda ((& cy & ay &ignore))
				(when (/= ay cy)
					(defq lines (.-> this :get_syntax_engine
						(:text_flow (split (. this :icopy 0 ay 0 cy) +char_class_white_space)
							(. this :get_wrap_width))))
					(.-> this
						(:idelete 0 ay 0 cy)
						(:iinsert 0 ay (join lines +char_class_lf 2)))))
				(. this :get_cursors)))
		this)

	(defmethod :split ()
		; (. document :split) -> document
		(undoable this
			(. this :select_paragraph)
			(reach (lambda ((& cy & ay &ignore))
				(when (/= ay cy)
					(defq lines (split (. this :icopy 0 ay 0 cy) +char_class_white_space))
					(.-> this
						(:idelete 0 ay 0 cy)
						(:iinsert 0 ay (join lines +char_class_lf 2)))))
				(. this :get_cursors)))
		this)

	(defmethod :sort ()
		; (. document :sort) -> document
		(undoable this
			(. this :floor_selection)
			(raise :cursors)
			(reach (lambda ((& ay & cy &ignore))
					(defq lines (list) y cy)
					(while (< y ay) (push lines (. this :get_text_line y)) (++ y))
					(setq lines (reverse (sort lines)))
					(while (< cy ay)
						(.-> this
							(:idelete 0 cy (dec (length (. this :get_text_line cy))) cy)
							(:iinsert 0 cy (most (pop lines))))
						(++ cy)))
				cursors)
			(lower :cursors))
		this)

	(defmethod :invert ()
		; (. document :invert) -> document
		(undoable this
			(. this :floor_selection)
			(raise :cursors)
			(reach (lambda ((& ay & cy &ignore))
					(defq lines (list) y cy)
					(while (< y ay) (push lines (. this :get_text_line y)) (++ y))
					(while (< cy ay)
						(.-> this
							(:idelete 0 cy (dec (length (. this :get_text_line cy))) cy)
							(:iinsert 0 cy (most (pop lines))))
						(++ cy)))
				cursors)
			(lower :cursors))
		this)

	(defmethod :unique ()
		; (. document :unique) -> document
		(undoable this
			(. this :floor_selection)
			(reach (lambda ((& ay & cy &ignore))
					(defq lines (list) y cy)
					(while (< y ay) (push lines (. this :get_text_line y)) (++ y))
					(setq lines (reverse (unique lines)))
					(while (< cy ay)
						(if (defq line (pop lines))
							(.-> this
								(:idelete 0 cy (dec (length (. this :get_text_line cy))) cy)
								(:iinsert 0 cy (most line)))
							(. this :idelete 0 cy 0 (setq cy ay)))
						(++ cy)))
				(. this :get_cursors))
			(. this :floor_selection))
		this)

	(defmethod :comment ()
		; (. document :comment) -> document
		(undoable this
			(. this :floor_selection)
			(raise :cursors)
			(reach (lambda ((& ay & cy &ignore))
					(until (= cy ay)
						(defq line (. this :get_text_line cy))
						(cond
							((blank-line? line))
							((starts-with +block_comment line)
								(. this :idelete 0 cy (const (length +block_comment)) cy))
							((. this :iinsert 0 cy +block_comment)))
						(++ cy)))
				cursors)
			(lower :cursors))
		this)

	(defmethod :trim ()
		; (. document :trim) -> document
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