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
			(when canvas
				;embed in horizontal scroll only if wider than standard width
				(defq tw (get :min_width page))
				(bind '(rw rh) (. canvas :pref_size))
				(when (> rw tw)
					(def (defq scroll (Scroll +scroll_flag_horizontal))
						:min_width tw :min_height rh)
					(. scroll :add_child (. canvas :change 0 0 rw rh))
					(setq canvas scroll))
				(. page :add_child canvas))
			state)))

;module
(export-symbols '(handler))
(env-pop)
