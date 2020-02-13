;Visual buffer input
; TODO: custom margins and line wrap
;;;;;;;;;;;;;;
; buffer input
;;;;;;;;;;;;;;
(defun-bind cursor-visible ()
	(cond
<<<<<<< HEAD
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

(defun-bind printable (c)
	;insert the tab/char at cursor or append to end etc
			(defq line (if (>= cy (length buffer)) "" (elem cy buffer))
				line (insert line (min cx (length line)) (ascii-char c)))
			(setq cx (inc cx))
			(if (>= cy (length buffer))
				(push buffer line)
				(elem-set cy buffer line)))
=======
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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1

(defun-bind return ()
;return key
	(cond
<<<<<<< HEAD
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
						buffer (insert buffer cy (list line_back)))))
	(setq cx 0))

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
=======
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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1

;;;;;;;;;;;;;;;;;;;;;;;;
; editor cursor behavior
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind left ()
	(cond
<<<<<<< HEAD
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

;;;;
;utility functions
;;;;
(defun-bind seql (sequence element) (elem element sequence))
(defun-bind e0s () (seql sequence 0))
(defun-bind els (y s) (length (elem y s)))
(defun-bind exeys (x y s) (elem x (elem y s)))
=======
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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1
