;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((match? line "^\s*```")
			(defq ss (string-stream (join lines (ascii-char 10)))
				result (eval
					(static-qq (progn
						(env-push)
						(catch (defq *result* (repl ,ss "Lisp handler"))
							(progn (prin _) (print) (defq result :nil) :t))
						(export-symbols '(*result*))
						(env-pop)
						*result*))
					*handler_env*))
			(cond
				((View? result)
					(def result :tip_mbox (elem-get select +select_tip))
					(def (defq flow (Flow)) :flow_flags +flow_right)
					(def (defq text (Text)) :text "")
					(.-> page
						(:add_child (. flow :add_child
							(. result :set_owner (elem-get select +select_embeded))))
						(:add_child text)))
				(:t ((handler-func :vdu) :vdu page (str result))
					((handler-func :vdu) :vdu page "```")))
			(clear lines)
			:text)
		(:t (push lines line)
			state)))

;module
(export-symbols '(handler))
(env-pop)
