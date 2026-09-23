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
			(when (lock-claim-rpc line +lock_mode_read)
				(setq canvas (canvas-load line +load_flag_shared))
				(lock-release-rpc line))
			(when canvas (. page :add_child canvas))
			state)))

;module
(export-symbols '(handler))
(env-pop)
