;jit compile apps native functions if needed
(import "lib/asm/asm.inc")
(make '("apps/netmon/child.vp"))

;imports
(import "gui/lisp.inc")

(structure 'sample_reply 0
	(nodeid 'node)
	(int 'task_count 'mem_used))

(structure '+event 0
	(byte 'close+ 'max+ 'min+))

(defq task_bars (list) memory_bars (list) task_scale (list) memory_scale (list)
	devices nil cpu_count 0 max_tasks 1 max_memory 1 last_max_tasks 0 last_max_memory 0
	select (list (task-mailbox) (mail-alloc-mbox)) farm (list) sample_msg (elem -2 select))

(ui-window mywindow ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-grid _ (:grid_width 2 :grid_height 1 :flow_flags +flow_down_fill+ :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green+)
			(ui-label _ (:text "Tasks" :color +argb_white+))
			(ui-grid _ (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (push task_scale (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+))))))
			(ui-grid task_grid (:grid_width 1 :grid_height 0)))
		(ui-flow _ (:color +argb_red+)
			(ui-label _ (:text "Memory (kb)" :color +argb_white+))
			(ui-grid _ (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (push memory_scale (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+))))))
			(ui-grid memory_grid (:grid_width 1 :grid_height 0)))))

(defun reset (new_devices)
	;reset the app to these known devices
	(while (defq mbox (pop farm)) (mail-send mbox ""))
	(setq devices new_devices cpu_count (length devices)
		farm (open-farm "apps/netmon/child" cpu_count kn_call_open devices))
	(while (defq progress (pop task_bars)) (. progress :sub))
	(while (defq progress (pop memory_bars)) (. progress :sub))
	(times cpu_count
		(. task_grid :add_child (defq progress (Progress)))
		(push task_bars progress)
		(. memory_grid :add_child (defq progress (Progress)))
		(push memory_bars progress))
	(set task_grid :grid_height cpu_count)
	(set memory_grid :grid_height cpu_count))

(defun main ()
	(reset (sort - (mail-devices)))
	;add window
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	;app event loop
	(while (progn
		;new batch of samples ?
		(when (= cpu_count (length devices))
			;should we reset the app ?
			(defq current_devices (sort - (mail-devices)))
			(unless (and (= (length current_devices) (length devices))
						(every eql current_devices devices))
				(reset current_devices)
				(bind '(x y w h) (apply view-fit (cat (. mywindow :get_pos) (. mywindow :pref_size))))
				(. mywindow :change_dirty x y w h))

			;set scales
			(setq last_max_tasks max_tasks last_max_memory max_memory max_tasks 1 max_memory 1)
			(each (lambda (st sm)
				(defq vt (* (inc _) (/ (* last_max_tasks 100) (length task_scale)))
					vm (* (inc _) (/ (* last_max_memory 100) (length memory_scale))))
				(def st :text (str (/ vt 100) "." (pad (% vt 100) 2 "0") "|"))
				(def sm :text (str (/ vm 102400) "|"))
				(. st :layout) (. sm :layout)) task_scale memory_scale)
			(. mywindow :dirty_all)
			;send out multi-cast sample command
			(while (/= cpu_count 0)
				(setq cpu_count (dec cpu_count))
				(mail-send (elem cpu_count farm) sample_msg))
			(task-sleep 10000))

		;next event
		(defq id (mail-select select) msg (mail-read (elem id select)))
		(cond
			((= id 0)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
						;close button
						nil)
					((= id +event_min+)
						;min button
						(bind '(x y w h) (apply view-fit
							(cat (. mywindow :get_pos) (. mywindow :pref_size))))
						(. mywindow :change_dirty x y w h))
					((= id +event_max+)
						;max button
						(bind '(x y) (. mywindow :get_pos))
						(bind '(w h) (. mywindow :pref_size))
						(bind '(x y w h) (view-fit x y (/ (* w 5) 3) h))
						(. mywindow :change_dirty x y w h))
					(t (. mywindow :event msg))))
			(t	;child info
				(defq index (find (slice sample_reply_node (+ sample_reply_node node_id_size) msg) devices)
					task_val (get-uint msg sample_reply_task_count)
					memory_val (get-uint msg sample_reply_mem_used)
					task_bar (elem index task_bars) memory_bar (elem index memory_bars))
				(setq max_tasks (max max_tasks task_val) max_memory (max max_memory memory_val))
				(def task_bar :maximum last_max_tasks :value task_val)
				(def memory_bar :maximum last_max_memory :value memory_val)
				;count up replies
				(setq cpu_count (inc cpu_count))))))
	;close window and children
	(. mywindow :hide)
	(mail-free-mbox (pop select))
	(while (defq mbox (pop farm))
		(mail-send mbox "")))
