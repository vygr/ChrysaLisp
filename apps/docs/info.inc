;module
(env-push)

(defun boot-funcs ()
	(defq e (penv))
	(while (penv e) (setq e (penv e)))
	(sort cmp (reduce (lambda (_ (k v))
		(if (func? v) (push _ k) _)) (tolist e) (list))))

(defun boot-lambdas ()
	(defq e (penv))
	(while (penv e) (setq e (penv e)))
	(sort cmp (reduce (lambda (_ (k v))
		(if (lambda? v) (push _ k) _)) (tolist e) (list))))

(defun boot-macros ()
	(defq e (penv))
	(while (penv e) (setq e (penv e)))
	(sort cmp (reduce (lambda (_ (k v))
		(if (macro? v) (push _ k) _)) (tolist e) (list))))

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
				(boot-funcs
					(paragraph (boot-funcs)))
				(boot-macros
					(paragraph (boot-macros)))
				(boot-lambdas
					(paragraph (boot-lambdas))))
			state)))

;module
(export (penv)
	handler)
(env-pop)
