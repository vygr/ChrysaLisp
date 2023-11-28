;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(:t (import-from line '(*window*))
			(def *window* :min_height 512)
			(. *window* :set_owner (const (pad "" +net_id_size (ascii-char -1))))
			(. page :add_child *window*)
			state)))

;module
(export-symbols '(handler))
(env-pop)
