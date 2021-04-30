;lisp section handler

;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(t	(def (defq text (Text))
				:text (. coloriser :expand_tabs line 4)
				:font *env_terminal_font*
				:ink_color +argb_blue)
			(. page :add_child text)
			state)))

;module
(export (penv)
	handler)
(env-pop)
