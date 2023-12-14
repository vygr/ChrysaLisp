;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			(each (lambda (line) (task-slice)
					((handler-func :vdu) :vdu page (trim-end line (ascii-char 13))))
				lines)
			((handler-func :vdu) :vdu page "```")
			(env-push)
			(catch (defq result (repl (string-stream
					(apply cat (join lines (list (ascii-char 10))))) "Lisp handler"))
				(progn (print _)(print) (defq result :nil) :t))
			(export-symbols '(result))
			(env-pop)
			(cond
				((View? result)
					(def result :tip_mbox (elem-get +select_tip select))
					(def (defq flow (Flow)) :flow_flags +flow_right)
					(def (defq text (Text)) :text "")
					(.-> page
						(:add_child (. flow :add_child
							(. result :set_owner (elem-get +select_embeded select))))
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
