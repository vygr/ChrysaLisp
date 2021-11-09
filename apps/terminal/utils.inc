;;;;;;;
; utils
;;;;;;;

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y* buffer (. *edit* :get_buffer))
		(. buffer :set_cursor x y)
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
		(if (= (const (ascii-code "|")) (defq c (code (elem-get i url))))
			(setq ctx 0 s 'w)
			(case s
				(w  (when (<= 33 c 126) (setq s 'b)))
				(b  (unless (<= 33 c 126) (setq s 'w ctx (inc ctx))))))) ctx)

(defun url-ext-index (urls)
	(defq i 0)
	(while (every (lambda (url)
			(and (< i (length url))
				(eql (elem-get i url) (elem-get i (elem-get 0 urls))))) urls)
		(setq i (inc i))) i)

(defun url-ext (url cx)
	(defq ctx (url-ctx url cx) url (slice (if (defq i (find-rev " " (slice 0 cx url))) (inc i) 0) cx url))
	(bind '(url file) (url-split url ctx))
	(defq urls (map (lambda (_) (if (= ctx 0) (cat (slice (length file) -7 _) " ") (slice (length file) -1 _)))
		(filter (lambda (_) (and (starts-with file _) (if (= ctx 0) (ends-with ".lisp " _) t))) (url-list url))))
	(if (= 0 (length urls)) "" (slice 0 (url-ext-index urls) (elem-get 0 urls))))
