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
	(each (lambda (line)
			((handler-func :vdu) :vdu page line))
		(. syntax :text_flow words 80))
	((handler-func :vdu) :vdu page "```"))

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			:text)
		(t  (case line
				("boot-funcs"
					(paragraph (boot-funcs)))
				("boot-macros"
					(paragraph (boot-macros)))
				("boot-lambdas"
					(paragraph (boot-lambdas))))
			state)))

;module
(export-symbols handler)
(env-pop)
