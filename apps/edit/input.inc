;Visual buffer input
; TODO: custom margins and line wrap
;;;;;;;;;;;;;;
; buffer input
;;;;;;;;;;;;;;
(defun-bind cursor-visible ()
	(cond
		((< buffer_cx buffer_ox)
			(setq buffer_ox buffer_cx))
		((>= buffer_cx (+ buffer_ox vdu_width))
			(setq buffer_ox (- buffer_cx vdu_width -1))))
	(cond
		((< buffer_cy buffer_oy)
			(setq buffer_oy buffer_cy))
		((>= buffer_cy (+ buffer_oy vdu_height))
			(setq buffer_oy (- buffer_cy vdu_height -1))))
	(list buffer_ox buffer_oy))

(defun-bind set-sticky ()
	(defq line (elem buffer_cy buffer_text))
	(cond
		((and (> buffer_cx sx) (<= sx (length line)))
			(setq buffer_cx sx))
		((>= buffer_cx (length line))
			(setq buffer_cx (length line)))))

(defun-bind printable (c)
	;insert the tab/char at cursor or append to end etc
			(defq line (if (>= buffer_cy (length buffer_text)) "" (elem buffer_cy buffer_text))
				line (insert line (min buffer_cx (length line)) (ascii-char c)))
			(setq buffer_cx (inc buffer_cx))
			(if (>= buffer_cy (length buffer_text))
				(push buffer_text line)
				(elem-set buffer_cy buffer_text line)))

(defun-bind return ()
;return key
	(cond
		((>= buffer_cy (length buffer_text))
			;off end of text so just append a blank line
			(push buffer_text "")
			(setq buffer_cy (length buffer_text)))
		(t	;break this line
			(defq line (elem buffer_cy buffer_text)
				line_front (slice 0 (min buffer_cx (length line)) line)
				line_back (slice (min buffer_cx (length line)) -1 line))
					(elem-set buffer_cy buffer_text line_front)
					(setq buffer_cy (min (inc buffer_cy) (length buffer_text))
						buffer_text (insert buffer_text buffer_cy (list line_back)))))
	(setq buffer_cx 0))

(defun-bind backspace ()
	(cond
		((> buffer_cx 0)
			(elem-set buffer_cy buffer_text (erase (elem buffer_cy buffer_text)
				(dec buffer_cx) buffer_cx))
			(setq buffer_cx (dec buffer_cx)))
		((<= buffer_cx 0)
			(unless (<= buffer_cy 0) 
				;backspace into previous line
				(defq prev_line (elem (dec buffer_cy) buffer_text)
					cat_line (cat prev_line (elem buffer_cy buffer_text)))
				(setq buffer_cx (length prev_line) buffer_cy (dec buffer_cy))
				(elem-set buffer_cy buffer_text cat_line)
				(setq buffer_text (erase buffer_text (inc buffer_cy) (+ buffer_cy 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
; editor cursor behavior
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind left ()
	(cond
		((<= buffer_cx 0)
			(cond
				((<= buffer_cy 0)
					(setq buffer_cy 0 buffer_cx 0))
				((> buffer_cy 0)
					(setq buffer_cy (dec buffer_cy) buffer_cx (setq buffer_cx 
						(length (elem buffer_cy buffer_text)))))))
		((> buffer_cx 0)
			(setq buffer_cx (dec buffer_cx)))))

(defun-bind right ()
	(cond
		((>= buffer_cx (length (elem buffer_cy buffer_text)))
			(cond
				((>= buffer_cy (dec (length buffer_text)))
					(setq buffer_cx (length (elem buffer_cy buffer_text))))
				((< buffer_cy (dec (length buffer_text)))
					(setq buffer_cy (inc buffer_cy) buffer_cx (setq buffer_cx 0)))))
		((< buffer_cx (length (elem buffer_cy buffer_text)))
			(setq buffer_cx (inc buffer_cx)))))

(defun-bind up ()
	(cond
		((<= buffer_cy 0)
			(setq buffer_cx 0))
		((> buffer_cy 0)
			(setq buffer_cy (dec buffer_cy))
			(setq buffer_cx (min sx (length (elem buffer_cy buffer_text)))))))

(defun-bind down ()
	(cond
		((>= buffer_cy (dec (length buffer_text)))
			(setq buffer_cy (dec (length buffer_text)))
			(setq buffer_cx (length (elem buffer_cy buffer_text)))
			(setq sx (length (elem buffer_cy buffer_text))))
		((< buffer_cy (dec (length buffer_text)))
			(setq buffer_cy (inc buffer_cy))
			(setq buffer_cx (min sx (length (elem buffer_cy buffer_text)))))))
