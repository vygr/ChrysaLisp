;Visual buffer input
; TODO: custom margins and line wrap
;;;;;;;;;;;;;;
; buffer input
;;;;;;;;;;;;;;
(defun-bind printable (c)
	;insert the tab/char at cursor or append to end etc
	(defq line (if (>= cy (length buffer)) "" (elem cy buffer))
		line (insert line (min cx (length line)) (char c)))
	(setq cx (inc cx))
	(if (>= cy (length buffer))
		(push buffer line)
		(elem-set cy buffer line)))

(defun-bind return ()
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

(defun-bind backspace ()
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

(defun-bind cursor-visible ()
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

(defun-bind set-sticky ()
	(defq line (elem cy buffer))
	(cond
		((and (> cx sx) (<= sx (length line)))
			(setq cx sx))
		((>= cx (length line))
			(setq cx (length line)))))

(defun-bind left ()
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

(defun-bind right ()
	(cond
		((>= cx (length (elem cy buffer)))
			(cond
				((>= cy (dec (length buffer)))
					(setq cx (length (elem cy buffer))))
				((< cy (dec (length buffer)))
					(setq cy (inc cy) cx (setq cx 0)))))
		((< cx (length (elem cy buffer)))
			(setq cx (inc cx)))))

(defun-bind up ()
	(cond
		((<= cy 0)
			(setq cx 0))
		((> cy 0)
			(setq cy (dec cy) cx (min sx (length (elem cy buffer)))))))

(defun-bind down ()
	(cond
		((>= cy (dec (length buffer)))
			(setq cy (dec (length buffer)) cx (length (elem cy buffer)) sx (length (elem cy buffer))))
		((< cy (dec (length buffer)))
			(setq cy (inc cy) cx (min sx (length (elem cy buffer)))))))
