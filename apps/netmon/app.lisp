;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/netmon/child.vp)

;imports
(import 'gui/lisp.inc)

(structure 'sample_reply 0
	(int 'cpu 'task_count 'mem_used))

(structure 'event 0
	(byte 'win_close 'win_max 'win_min))

(defq task_bars (list) memory_bars (list) task_scale (list) memory_scale (list)
	cpu_total (kernel-total) cpu_count cpu_total
	max_tasks 1 max_memory 1 last_max_tasks 0 last_max_memory 0 select (array (task-mailbox) (mail-alloc-mbox))
	farm (open-farm "apps/netmon/child" cpu_total kn_call_open) sample_msg (array (elem 1 select)))

(ui-tree window (create-window) nil
	(ui-element _ (create-flow) ('flow_flags flow_down_fill)
		(ui-element _ (create-flow) ('flow_flags flow_left_fill
				'font (create-font "fonts/Entypo.ctf" 22) 'color title_col)
			(ui-buttons (0xea19 0xea1b 0xea1a) (const event_win_close))
			(ui-element _ (create-title) ('text "Network Monitor" 'font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-element _ (create-grid) ('grid_width 2 'grid_height 1 'flow_flags flow_down_fill 'maximum 100 'value 0)
			(ui-element _ (create-flow) ('color argb_green)
				(ui-element _ (create-label) ('text "Tasks" 'color argb_white))
				(ui-element _ (create-grid) ('grid_width 4 'grid_height 1 'color argb_white
						'font (create-font "fonts/Hack-Regular.ctf" 14))
					(times 4 (push task_scale (ui-element _ (create-label)
						('text "|" 'flow_flags (logior flow_flag_align_vcenter flow_flag_align_hright))))))
				(ui-element _ (create-grid) ('grid_width 1 'grid_height cpu_total)
					(times cpu_total (push task_bars (ui-element _ (create-progress))))))
			(ui-element _ (create-flow) ('color argb_red)
				(ui-element _ (create-label) ('text "Memory (kb)" 'color argb_white))
				(ui-element _ (create-grid) ('grid_width 4 'grid_height 1 'color argb_white
						'font (create-font "fonts/Hack-Regular.ctf" 14))
					(times 4 (push memory_scale (ui-element _ (create-label)
						('text "|" 'flow_flags (logior flow_flag_align_vcenter flow_flag_align_hright))))))
				(ui-element _ (create-grid) ('grid_width 1 'grid_height cpu_total)
					(times cpu_total (push memory_bars (ui-element _ (create-progress)))))))))

(defun-bind main ()
	;add window
	(gui-add (apply view-change (cat (list window 320 32) (view-pref-size window))))
	;app event loop
	(while (progn
		;new batch of samples ?
		(when (= cpu_count cpu_total)
			;set scales
			(setq last_max_tasks max_tasks last_max_memory max_memory max_tasks 1 max_memory 1)
			(each (lambda (st sm)
				(defq vt (* (inc _) (/ (* last_max_tasks 100) (length task_scale)))
					vm (* (inc _) (/ (* last_max_memory 100) (length memory_scale))))
				(def st 'text (str (/ vt 100) "." (pad (% vt 100) 2 "0") "|"))
				(def sm 'text (str (/ vm 102400) "|"))
				(view-layout st) (view-layout sm)) task_scale memory_scale)
			(view-dirty-all window)
			;send out multi-cast sample command
			(while (/= cpu_count 0)
				(setq cpu_count (dec cpu_count))
				(mail-send sample_msg (elem cpu_count farm)))
			(task-sleep 10000))

		;next event
		(defq id (mail-select select) msg (mail-read (elem id select)))
		(cond
			((= id 0)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) event_win_close)
						;close button
						(setq id nil))
					((= id event_win_min)
						;min button
						(bind '(x y _ _) (view-get-bounds window))
						(bind '(w h) (view-pref-size window))
						(view-change-dirty window x y w h))
					((= id event_win_max)
						;max button
						(bind '(x y _ _) (view-get-bounds window))
						(bind '(w h) (view-pref-size window))
						(view-change-dirty window x y (fmul w 1.75) h))
					(t (view-event window msg))))
			(t	;child info
				(defq cpu_id (get-int msg sample_reply_cpu)
					task_val (get-int msg sample_reply_task_count)
					memory_val (get-int msg sample_reply_mem_used)
					task_bar (elem cpu_id task_bars) memory_bar (elem cpu_id memory_bars))
				(setq max_tasks (max max_tasks task_val) max_memory (max max_memory memory_val))
				(def task_bar 'maximum last_max_tasks 'value task_val)
				(def memory_bar 'maximum last_max_memory 'value memory_val)
				;count up replies
				(setq cpu_count (inc cpu_count))))))
	;close window and children
	(view-hide window)
	(mail-free-mbox (elem 1 select))
	(while (defq mbox (pop farm))
		(mail-send (const (char event_win_close long_size)) mbox)))
