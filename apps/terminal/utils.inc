;;;;;;;
; utils
;;;;;;;

(defmacro not-whole-word-char? (_)
	`(find ,_ +not_whole_word_chars))

(defun blank-line? (line)
	(every (# (eql " " %0)) line))

(defun selection? ()
	(bind '(x y) (. *current_buffer* :get_cursor))
	(or (/= *anchor_x* x) (/= *anchor_y* x)))

(defun sort-selection ()
	(bind '(sx sy) (. *current_buffer* :get_cursor))
	(defq sx1 *anchor_x* sy1 *anchor_y*)
	(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
	(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))
	(list sx sy sx1 sy1))

(defun select-lines ()
	(bind '(sx sy sx1 sy1) (sort-selection))
	(if (or (> sx1 0) (= sy sy1)) (setq sy1 (inc sy1)))
	(list sy sy1))

(defun select-line ()
	(bind '(x y) (. *current_buffer* :get_cursor))
	(list y (inc y)))

(defun select-word ()
	(bind '(cx y) (. *current_buffer* :get_cursor))
	(defq line (. *current_buffer* :get_text_line y)
		x (some! cx 0 nil (# (if (not-whole-word-char? %0) (inc _))) (list line))
		x1 (some! cx -1 nil (# (if (not-whole-word-char? %0) _)) (list line)))
	(unless x (setq x 0))
	(unless x1 (setq x1 (length line)))
	(list x x1 y))

(defun select-paragraph ()
	(bind '(x cy) (. *current_buffer* :get_cursor))
	(defq y (some! cy 0 nil
				(# (if (blank-line? %0) (inc _)))
			(list (. *current_buffer* :get_text_lines)))
		y1 (some! cy (bind '(w h) (. *current_buffer* :get_size)) nil
				(# (if (blank-line? %0) _))
			(list (. *current_buffer* :get_text_lines))))
	(unless y (setq y 0))
	(unless y1 (setq y1 h))
	(list y y1))

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y*)
		(. (defq buffer *current_buffer*) :set_cursor x y)
		~_
		(bind '(x y) (. buffer :get_cursor))
		(setq *cursor_x* x *cursor_y* y)))

;;;;;;;;;;;
; url utils
;;;;;;;;;;;

(defun url-list (url)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (push out (cat f (if (eql m "4") "/" " "))))
		(unzip (split (pii-dirlist url) ",") (list (list) (list)))) out)

(defun url-split (url ctx)
	(if (defq i (find-rev "/" url))
		(list (cat "./" (slice 0 (inc i) url)) (slice (inc i) -1 url))
		(list (if (= ctx 0) "./cmd/" "./") url)))

(defun url-ctx (url cx)
	(defq ctx 0 i -1 s 'w)
	(until (= cx (setq i (inc i)))
		(if (= (const (ascii-code "|")) (defq c (code (elem i url))))
			(setq ctx 0 s 'w)
			(case s
				(w	(when (<= 33 c 126) (setq s 'b)))
				(b	(unless (<= 33 c 126) (setq s 'w ctx (inc ctx))))))) ctx)

(defun url-ext-index (urls)
	(defq i 0)
	(while (every (lambda (url)
			(and (< i (length url))
				(eql (elem i url) (elem i (elem 0 urls))))) urls)
		(setq i (inc i))) i)

(defun url-ext (url cx)
	(defq ctx (url-ctx url cx) url (slice (if (defq i (find-rev " " (slice 0 cx url))) (inc i) 0) cx url))
	(bind '(url file) (url-split url ctx))
	(defq urls (map (lambda (_) (if (= ctx 0) (cat (slice (length file) -7 _) " ") (slice (length file) -1 _)))
		(filter (lambda (_) (and (starts-with file _) (if (= ctx 0) (ends-with ".lisp " _) t))) (url-list url))))
	(if (= 0 (length urls)) "" (slice 0 (url-ext-index urls) (elem 0 urls))))
