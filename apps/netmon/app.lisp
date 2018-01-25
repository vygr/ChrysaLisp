;import settings
(run 'apps/sys.lisp)
(run 'apps/ui.lisp)

(defq
	sample_msg_command 0
	sample_msg_reply_id (add sample_msg_command long_size)
	sample_msg_cpu (add sample_msg_reply_id id_size)
	sample_msg_task_count (add sample_msg_cpu long_size)
	sample_msg_mem_used (add sample_msg_task_count long_size)
	task_bars (list) memory_bars (list) cpu_total (cpu-total) cpu_count cpu_total
	id t max_tasks 0 max_memory 0)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-grid) ('grid_width 2 'grid_height 1 'flow_flags (bit-or flow_flag_down flow_flag_fillw) 'progress_max 100 'progress_val 0)
		(ui-element left_panel (create-flow) ('color 0xff00ff00)
			(ui-element _ (create-label) ('text "Tasks" 'color 0xffffffff)))
		(ui-element right_panel (create-flow) ('color 0xffff0000)
			(ui-element _ (create-label) ('text "Memory" 'color 0xffffffff)))
		(times cpu_total
			(defq task_bar (create-progress) memory_bar (create-progress))
			(push task_bars task_bar)
			(push memory_bars memory_bar)
			(slot add_child left_panel task_bar)
			(slot add_child right_panel memory_bar))))

(slot set_title window "Network Monitor")
(slot connect_close window 0)
(bind '(w h) (slot pref_size window))
(slot change window 320 32 w h)
(slot gui_add window)

;open global farm
(defq ids (open-farm "apps/netmon/child" cpu_total kn_call_open))
(bind '(my_mbox my_cpu) (task-mailbox))

(while id
	;new batch of samples ?
	(if (eq cpu_count cpu_total)
		;send out sample commands
		(while (ne cpu_count 0)
			(setq cpu_count (dec cpu_count))
			(mail-send (cat
				(char 1 long_size)
				(char my_mbox long_size)
				(char my_cpu long_size)
				(char 0 long_size)
				(char 0 long_size)
				(char 0 long_size)) (elem (mul cpu_count 2) ids) (elem (inc (mul cpu_count 2)) ids))))
	(cond
		((ge (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) 1)
			;reply from cpu
			(defq cpu (read-long sample_msg_cpu msg)
				task_val (read-long sample_msg_task_count msg)
				memory_val (read-long sample_msg_mem_used msg)
				task_bar (elem cpu task_bars) memory_bar (elem cpu memory_bars))
			(setq max_tasks (max max_tasks task_val) max_memory (max max_memory memory_val))
			(def task_bar 'progress_max max_tasks 'progress_val task_val)
			(def memory_bar 'progress_max max_memory 'progress_val memory_val)
			(slot dirty task_bar)
			(slot dirty memory_bar)

			;count up replies
			(setq cpu_count (inc cpu_count)))
		((eq id 0)
			(setq id nil))
		(t (slot event window msg))))

;wait for outstanding replies
(while (ne cpu_count cpu_total)
	(if (eq (read-long ev_msg_target_id (mail-mymail)) 1)
		(setq cpu_count (inc cpu_count))))

;send out multi-cast exit command
(defq exit (char 2 long_size))
(while (defq cpu (pop ids) mbox (pop ids))
	(mail-send exit mbox cpu))
