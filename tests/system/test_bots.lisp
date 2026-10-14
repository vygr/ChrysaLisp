(report-header "System: the bots band, a task that works by itself for others runs before apps, and the bots of a node rest together so apps have a turn")

;a bot, a task in the band of bots, (task-band :bot)
(assert-eq "this task is not a bot to begin with" :nil (. *bot_breaths* :find (task-mbox)))
(assert-eq "it is made one" :t (task-band :bot))
(assert-true "and is known as one, for its rests" (. *bot_breaths* :find (task-mbox)))

;a rest that is due is taken by a bot, and when the next is due moves on
(setq *bot_next* 0)
(task-breathe)
(assert-true "a bot whose rest is due sets when the next is" (> *bot_next* (pii-time)))
(assert-true "the next is so long on" (<= (- *bot_next* (pii-time)) +bot_work))

;an app is not a bot, and does not rest
(task-band :app)
(assert-eq "an app again is not known as a bot" :nil (. *bot_breaths* :find (task-mbox)))
(setq *bot_next* 0 *bot_rest_end* 0)
(task-breathe)
(assert-eq "an app does not take the rest, and moves nothing on" 0 *bot_next*)

;it is told if an app is ready, this task is one and is running, not waiting
(assert-true "there is a binding that says if an app of the node is ready"
	(find ((ffi "sys/task/lisp_apps")) '(:t :nil)))
