;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

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

(defq task_bars (list) memory_bars (list) task_scale (list) memory_scale (list)
	cpu_total (kernel-total) cpu_count cpu_total id t
	max_tasks 1 max_memory 1 last_max_tasks 0 last_max_memory 0)

(ui-tree window (create-window (add window_flag_close window_flag_min window_flag_max)) nil
	(ui-element _ (create-grid) ('grid_width 2 'grid_height 1 'flow_flags (logior flow_flag_down flow_flag_fillw) 'maximum 100 'value 0)
		(ui-element _ (create-flow) ('color argb_green)
			(ui-element _ (create-label) ('text "Tasks" 'color argb_white))
			(ui-element _ (create-grid) ('grid_width 4 'grid_height 1 'color argb_white
					'font (create-font "fonts/Hack-Regular.ttf" 14))
				(times 4 (push task_scale (ui-element _ (create-label) ('text "0|" 'flow_flags flow_flag_align_hright)))))
			(times cpu_total (push task_bars (ui-element _ (create-progress)))))
		(ui-element _ (create-flow) ('color argb_red)
			(ui-element _ (create-label) ('text "Memory" 'color argb_white))
			(ui-element _ (create-grid) ('grid_width 4 'grid_height 1 'color argb_white
					'font (create-font "fonts/Hack-Regular.ttf" 14))
				(times 4 (push memory_scale (ui-element _ (create-label) ('text "0|" 'flow_flags flow_flag_align_hright)))))
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
	(when (eq cpu_count cpu_total)
		;set scale
		(setq last_max_tasks max_tasks last_max_memory max_memory max_tasks 1 max_memory 1)
		(each (lambda (st sm vt vm)
			(def st 'text (str (div vt 1.0) "." (div (mul (logand vt fp_frac_mask) 10) 1.0) "|"))
			(def sm 'text (str (div vm 1024) "|"))
			(view-dirty (view-layout st))
			(view-dirty (view-layout sm)))
			task_scale memory_scale
			(list (fdiv last_max_tasks 4) (fdiv last_max_tasks 2)
				(add (fdiv last_max_tasks 4) (fdiv last_max_tasks 2)) (mul last_max_tasks 1.0))
			(list (div last_max_memory 4) (div last_max_memory 2)
				(add (div last_max_memory 4) (div last_max_memory 2)) last_max_memory))
		;send out multi-cast sample command
		(while (ne cpu_count 0)
			(setq cpu_count (dec cpu_count))
			(mail-send sample_msg (elem cpu_count ids))))
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_sample)
			;reply from cpu
			(defq cpu (get-long msg sample_reply_msg_cpu)
				task_val (get-long msg sample_reply_msg_task_count)
				memory_val (get-long msg sample_reply_msg_mem_used)
				task_bar (elem cpu task_bars) memory_bar (elem cpu memory_bars))
			(setq max_tasks (max max_tasks task_val) max_memory (max max_memory memory_val))
			(def task_bar 'maximum last_max_tasks 'value task_val)
			(def memory_bar 'maximum last_max_memory 'value memory_val)
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
(setq window nil)
(while (ne cpu_count cpu_total)
	(if (eq (get-long (mail-mymail) ev_msg_target_id) event_win_sample)
		(setq cpu_count (inc cpu_count))))

;send out multi-cast exit command
(defq exit (char event_win_exit long_size))
(while (defq mbox (pop ids))
	(mail-send exit mbox))
