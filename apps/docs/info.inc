;module
(env-push)

(defun root-funcs ()
	(sort cmp (reduce (lambda (_ (k v))
		(if (func? v) (push _ k) _)) (tolist *root_env*) (list))))

(defun root-lambdas ()
	(sort cmp (reduce (lambda (_ (k v))
		(if (lambda-func? v) (push _ k) _)) (tolist *root_env*) (list))))

(defun root-macros ()
	(sort cmp (reduce (lambda (_ (k v))
		(if (macro-func? v) (push _ k) _)) (tolist *root_env*) (list))))

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
		(:t	(case line
				("root-funcs"
					(paragraph (root-funcs)))
				("root-macros"
					(paragraph (root-macros)))
				("root-lambdas"
					(paragraph (root-lambdas))))
			state)))

;module
(export-symbols handler)
(env-pop)
