;;;;;;;;;;;
; url utils
;;;;;;;;;;;

;module
(env-push)

(defun url-list (url)
	(map! 0 -1 (lambda (f m) (cat f (if (eql m "4") "/" " ")))
		(unzip (split (pii-dirlist url) ",") 2) (list)))

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
				(w (when (<= 33 c 126) (setq s 'b)))
				(b (unless (<= 33 c 126) (setq s 'w ctx (inc ctx))))))) ctx)

(defun url-ext-index (urls)
	(defq i 0)
	(while (every (lambda (url)
			(and (< i (length url))
				(eql (elem-get i url) (elem-get i (first urls))))) urls)
		(setq i (inc i))) i)

(defun url-ext (url cx &optional ctx)
	(setd ctx (url-ctx url cx))
	(defq url (slice (if (defq i (find-rev " " (slice 0 cx url))) (inc i) 0) cx url))
	(bind '(url file) (url-split url ctx))
	(defq urls (map (# (if (= ctx 0) (cat (slice (length file) -7 %0) " ") (slice (length file) -1 %0)))
		(filter (# (and (starts-with file %0) (if (= ctx 0) (ends-with ".lisp " %0) :t)))
			(url-list url))))
	(if (= 0 (length urls)) "" (slice 0 (url-ext-index urls) (first urls))))

;module
(export-symbols '(url-ext))
(env-pop)
