;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(:t (bind '(file &optional widget mw mh)
				(first (read (string-stream (cat "(" line ")")))))
			(setd widget '*window* mw 0 mh 0)
			(import-from (str file) (list widget))
			(def (defq widget (eval widget)) :min_width mw :min_height mh
				:tip_mbox (elem-get +select_tip select))
			(. widget :set_owner (elem-get +select_embeded select))
			(. page :add_child widget)
			state)))

;module
(export-symbols '(handler))
(env-pop)
