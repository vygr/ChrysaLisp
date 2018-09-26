;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

(structure 'sample_reply_msg 0
	(long 'command)
	(long 'cpu)
	(long 'task_count)
	(long 'mem_used))

(structure 'event 0
	(byte 'win_exit)
	(byte 'win_sample)
	(byte 'win_close)
	(byte 'win_min)
	(byte 'win_max))

(defq task_bars (list) memory_bars (list)
	cpu_total (kernel-total) cpu_count cpu_total
	id t max_tasks 0 max_memory 0)

(ui-tree window (create-window (add window_flag_close window_flag_min window_flag_max)) nil
	(ui-element _ (create-grid) ('grid_width 2 'grid_height 1 'flow_flags (bit-or flow_flag_down flow_flag_fillw) 'maximum 100 'value 0)
		(ui-element _ (create-flow) ('color argb_green)
			(ui-element _ (create-label) ('text "Tasks" 'color argb_white))
			(times cpu_total (push task_bars (ui-element _ (create-progress)))))
		(ui-element _ (create-flow) ('color argb_red)
			(ui-element _ (create-label) ('text "Memory" 'color argb_white))
			(times cpu_total (push memory_bars (ui-element _ (create-progress)))))))

(window-set-title window "Network Monitor")
(window-connect-close window event_win_close)
(window-connect-min window event_win_min)
(window-connect-max window event_win_max)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 320 32 w h))

;open global farm, create multi-cast sample command
(defq ids (open-farm "apps/netmon/child" cpu_total kn_call_open)
	sample_msg (array event_win_sample (task-mailbox)))

(while id
	;new batch of samples ?
	(if (eq cpu_count cpu_total)
		;send out multi-cast sample command
		(while (ne cpu_count 0)
			(setq cpu_count (dec cpu_count))
			(mail-send sample_msg (elem cpu_count ids))))
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_sample)
			;reply from cpu
			(defq cpu (read-long sample_reply_msg_cpu msg)
				task_val (read-long sample_reply_msg_task_count msg)
				memory_val (read-long sample_reply_msg_mem_used msg)
				task_bar (elem cpu task_bars) memory_bar (elem cpu memory_bars))
			(setq max_tasks (max max_tasks task_val) max_memory (max max_memory memory_val))
			(def task_bar 'maximum max_tasks 'value task_val)
			(def memory_bar 'maximum max_memory 'value memory_val)
			(view-dirty task_bar)
			(view-dirty memory_bar)

			;count up replies
			(setq cpu_count (inc cpu_count)))
		((eq id event_win_close)
			;close button
			(setq id nil))
		((eq id event_win_min)
			;min button
			(bind '(x y _ _) (view-get-bounds (view-dirty window)))
			(bind '(w h) (view-pref-size window))
			(view-dirty-all (view-change window x y w h)))
		((eq id event_win_max)
			;max button
			(bind '(x y _ _) (view-get-bounds (view-dirty window)))
			(bind '(w h) (view-pref-size window))
			(view-dirty-all (view-change window x y (fmul w 1.75) h)))
		(t (view-event window msg))))

;wait for outstanding replies
(while (ne cpu_count cpu_total)
	(if (eq (read-long ev_msg_target_id (mail-mymail)) event_win_sample)
		(setq cpu_count (inc cpu_count))))

;send out multi-cast exit command
(defq exit (char event_win_exit long_size))
(while (defq mbox (pop ids))
	(mail-send exit mbox))
