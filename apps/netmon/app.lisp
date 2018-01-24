;import settings
(run 'apps/sys.lisp)
(run 'apps/ui.lisp)

(defq
	sample_msg_command 0
	sample_msg_reply_id (add sample_msg_command long_size)
	sample_msg_cpu (add sample_msg_reply_id id_size)
	sample_msg_task_count (add sample_msg_cpu long_size)
	sample_msg_mem_used (add sample_msg_task_count long_size)

	window (create-window window_flag_close) grid (create-grid)
	left_panel (create-flow) right_panel (create-flow)
	tasks (create-label) memory (create-label))

(slot set_title window "Network Monitor")
(def left_panel 'color 0xff00ff00)
(def right_panel 'color 0xffff0000)
(def grid 'grid_width 2 'grid_height 1 'flow_flags (bit-or flow_flag_down flow_flag_fillw)
	'progress_max 100 'progress_val 0)
(def tasks 'text "Tasks" 'color 0xffffffff)
(def memory 'text "Memory" 'color 0xffffffff)

(slot add_child window grid)
(slot add_child grid left_panel)
(slot add_child grid right_panel)
(slot add_child left_panel tasks)
(slot add_child right_panel memory)

(defq task_bars (list) memory_bars (list) cpu_total (cpu-total) cpu_count -1)
(while (ne (setq cpu_count (inc cpu_count)) cpu_total)
	(defq task_bar (create-progress) memory_bar  (create-progress))
	(push task_bars task_bar)
	(push memory_bars memory_bar)
	(slot add_child left_panel task_bar)
	(slot add_child right_panel memory_bar))

(slot connect_close window 0)
(bind '(w h) (slot pref_size window))
(slot change window 320 32 w h)
(slot gui_add window)

;open global farm
(defq ids (open-farm "apps/netmon/child" cpu_total kn_call_open)
	id t max_tasks 0 max_memory 0)
(bind '(my_mbox my_cpu) (task-mailbox))

(while id
	;new batch of samples ?
	(when (eq cpu_count cpu_total)
		;send out sample commands
		(while (ne cpu_count 0)
			(mail-send (cat
				(char 1 long_size)
				(char my_mbox long_size)
				(char my_cpu long_size)
				(char (setq cpu_count (dec cpu_count)) long_size)
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
	(mail-mymail)
	(setq cpu_count (inc cpu_count)))

;send out exit commands
(while (ge (setq cpu_count (dec cpu_count)) 0)
	(mail-send (char 2 long_size) (elem (mul cpu_count 2) ids) (elem (inc (mul cpu_count 2)) ids)))
