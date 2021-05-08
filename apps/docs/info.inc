;module
(env-push)

(defun boot-funcs ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (func? v) (push out k))) (tolist e))
	(sort cmp out))

(defun boot-lambdas ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (lambda? v) (push out k))) (tolist e))
	(sort cmp out))

(defun boot-macros ()
	(defq out (list) e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(if (macro? v) (push out k))) (tolist e))
	(sort cmp out))

(defun paragraph (words)
	(defq cnt 0 line_width 80 line '())
	(each (lambda (word)
		(cond
			((> (setq cnt (+ cnt (length word) 1)) line_width)
				((handler-func :vdu) :vdu page (join line " "))
				(push (clear line) word)
				(setq cnt (+ (length word) 1)))
			(t	(push line word)))) words)
	(if (> (length line) 0)
		((handler-func :vdu) :vdu page (join line " ")))
	((handler-func :vdu) :vdu page "```")
	(clear line))

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			:text)
		(t	(case (sym line)
				(funcs
					(paragraph (boot-funcs)))
				(macros
					(paragraph (boot-macros)))
				(lambdas
					(paragraph (boot-lambdas))))
			state)))

;module
(export (penv)
	handler)
(env-pop)
