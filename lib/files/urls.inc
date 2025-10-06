;;;;;;;;;;;
; url utils
;;;;;;;;;;;

;module
(env-push)

(defun url-list (url)
	(map! (lambda (f m) (cat f (if (eql m "4") "/" " ")))
		(unzip (split (pii-dirlist url) ",") 2) (list)))

(defun url-split (url ctx)
	(if (defq i (rfind "/" url))
		(list (cat "./" (slice url 0 i)) (slice url i -1))
		(list (if (= ctx 0) "./cmd/" "./") url)))

(defun url-ctx (url cx)
	(defq ctx 0 i -1 s 'w)
	(until (= cx (++ i))
		(if (= (const (ascii-code "|")) (defq c (code (elem-get url i))))
			(setq ctx 0 s 'w)
			(case s
				(w (when (<= 33 c 126) (setq s 'b)))
				(b (unless (<= 33 c 126) (setq s 'w ctx (inc ctx))))))) ctx)

(defun url-ext-index (urls)
	(defq i 0)
	(while (every (lambda (url)
			(and (< i (length url))
				(eql (elem-get url i) (elem-get (first urls) i)))) urls)
		(++ i)) i)

(defun url-ext (url cx &optional ctx)
	(setd ctx (url-ctx url cx))
	(defq url (slice url (if (defq i (rfind " " (slice url 0 cx))) i 0) cx))
	(bind '(url file) (url-split url ctx))
	(defq urls (map (# (if (= ctx 0) (cat (slice %0 (length file) -7) " ") (slice %0 (length file) -1)))
		(filter (# (and (starts-with file %0) (if (= ctx 0) (ends-with ".lisp " %0) :t)))
			(url-list url))))
	(if (= 0 (length urls)) "" (slice (first urls) 0 (url-ext-index urls))))

;module
(export-symbols '(url-ext))
(env-pop)
