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
			;a .cwb is a document, it may have been drawn again since it
			;was last shown, so it is not taken from the cache of pictures,
			;which keeps the first it was given for as long as the node is up
			(with-read-lock line
				(setq canvas (canvas-load line (if (ends-with ".cwb" line) 0 +load_flag_shared))))
			(when canvas (. page :add_child canvas))
			state)))

;module
(export-symbols '(handler))
(env-pop)
