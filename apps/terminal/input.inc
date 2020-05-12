;;;;;;;;;;;;;
; input utils
;;;;;;;;;;;;;

(defq *line_buf* "" *line_pos* 0 *line_history* (list) *line_history_idx* 0)

;url utils
(defun-bind url-list (url)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (push out (cat f (if (eql m "4") "/" " "))))
		(unzip (split (pii-dirlist url) ",") (list (list) (list)))) out)

(defun-bind url-split (url ctx)
	(if (defq i (find-rev "/" url))
		(list (cat "./" (slice 0 (inc i) url)) (slice (inc i) -1 url))
		(list (if (= ctx 0) "./cmd/" "./") url)))

(defun-bind url-ctx (url cx)
	(defq ctx 0 i -1 s 'w)
	(until (= cx (setq i (inc i)))
		(if (= (const (ascii-code "|")) (defq c (code (elem i url))))
			(setq ctx 0 s 'w)
			(case s
				(w	(when (<= 33 c 126) (setq s 'b)))
				(b	(unless (<= 33 c 126) (setq s 'w ctx (inc ctx))))))) ctx)

(defun-bind url-ext-index (urls)
	(defq i 0)
	(while (every (lambda (url)
			(and (< i (length url))
				(eql (elem i url) (elem i (elem 0 urls))))) urls)
		(setq i (inc i))) i)

(defun-bind url-ext (url cx)
	(defq ctx (url-ctx url cx) url (slice (if (defq i (find-rev " " (slice 0 cx url))) (inc i) 0) cx url))
	(bind '(url file) (url-split url ctx))
	(defq urls (map (lambda (_) (if (= ctx 0) (cat (slice (length file) -7 _) " ") (slice (length file) -1 _)))
		(filter (lambda (_) (and (starts-with file _) (if (= ctx 0) (ends-with ".lisp " _) t))) (url-list url))))
	(if (= 0 (length urls)) "" (slice 0 (url-ext-index urls) (elem 0 urls))))

(defun-bind line-input (c)
	;key action on line input buffer
	(cond
		((or (= c 10) (= c 13))
			;enter key ?
			(when (/= (length *line_buf*) 0)
				;push new line history entry if not same as last entry
				(and (> (length (push *line_history* *line_buf*)) 1)
					(eql (elem -3 *line_history*) *line_buf*)
					(pop *line_history*))
				(setq *line_history_idx* (length *line_history*))))
		((= c 0x40000050)
			;cursor left key ?
			(setq *line_pos* (max 0 (dec *line_pos*))))
		((= c 0x4000004f)
			;cursor right key ?
			(setq *line_pos* (min (length *line_buf*) (inc *line_pos*))))
		((= c 0x40000052)
			;cursor up key ?
			(setq *line_buf* "" *line_history_idx* (max 0 (dec *line_history_idx*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*)))
			(setq *line_pos* (length *line_buf*)))
		((= c 0x40000051)
			;cursor down key ?
			(setq *line_buf* "" *line_history_idx* (min (inc *line_history_idx*) (length *line_history*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*)))
			(setq *line_pos* (length *line_buf*)))
		((and (= c 8) (/= *line_pos* 0))
			;backspace key ?
			(setq *line_buf* (erase *line_buf* (setq *line_pos* (dec *line_pos*)) (inc *line_pos*))))
		((and (= c 127) (/= *line_pos* (length *line_buf*)))
			;delete key ?
			(setq *line_buf* (erase *line_buf* *line_pos* (inc *line_pos*))))
		((= c 9)
			;tab key ?
			(defq ext (url-ext *line_buf* *line_pos*))
			(setq *line_buf* (insert *line_buf* *line_pos* ext) *line_pos* (+ *line_pos* (length ext))))
		((<= 32 c 126)
			;insert the char
			(setq *line_buf* (insert *line_buf* *line_pos* (char c)) *line_pos* (inc *line_pos*)))))

(defun-bind line-clear ()
	;clear line buffer
	(setq *line_buf* "" *line_pos* 0))
