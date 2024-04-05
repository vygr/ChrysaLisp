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
			(def (defq widget (eval widget)) :tip_mbox (elem-get select +select_tip))
			(unless (def? :min_width widget) (def widget :min_width mw))
			(unless (def? :min_height widget) (def widget :min_height mh))
			(def (defq flow (Flow)) :flow_flags +flow_right)
			(. page :add_child (. flow :add_child
				(. widget :set_owner (elem-get select +select_embeded))))
			state)))

;module
(export-symbols '(handler))
(env-pop)
