;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(:t	(defq canvas (Canvas-from-file line +load_flag_shared))
			(. page :add_child canvas)
			state)))

;module
(export-symbols handler)
(env-pop)
