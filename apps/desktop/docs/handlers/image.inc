(import "service/lock/app.inc")

;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((match? line "^\\s*```")
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(:t (defq canvas :nil)
			(with-read-lock line
				(setq canvas (canvas-load line +load_flag_shared)))
			(when canvas (. page :add_child canvas))
			state)))

;module
(export-symbols '(handler))
(env-pop)
