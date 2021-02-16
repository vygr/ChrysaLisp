;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")

(structure 'sample_reply 0
	(nodeid 'node)
	(int 'task_count 'mem_used))

(structure '+event 0
	(byte 'close+ 'max+ 'min+))

(structure '+select 0
	(byte 'main+ 'task+ 'reply+ 'nodes+))

(defq max_tasks 1 max_memory 1 last_max_tasks 1 last_max_memory 1 rate (/ 1000000 3) id t
	select (list (task-mailbox) (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
	retry_timeout (if (starts-with "obj/vp64" (load-path)) 10000000 1000000))

(ui-window mywindow ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-grid _ (:grid_width 2 :grid_height 1 :flow_flags +flow_down_fill+ :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green+)
			(ui-label _ (:text "Tasks" :color +argb_white+))
			(ui-grid task_scale_grid (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+)))))
			(ui-grid task_grid (:grid_width 1 :grid_height 0)))
		(ui-flow _ (:color +argb_red+)
			(ui-label _ (:text "Memory (kb)" :color +argb_white+))
			(ui-grid memory_scale_grid (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+)))))
			(ui-grid memory_grid (:grid_width 1 :grid_height 0)))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq mb (Progress) tb (Progress) val (emap))
		(:insert :child (const (pad "" net_id_size)))
		(:insert :timestamp now)
		(:insert :memory_bar mb)
		(:insert :task_bar tb))
	(. memory_grid :add_child mb)
	(. task_grid :add_child tb)
	(open-task "apps/netmon/child.lisp" key kn_call_open 0 (elem +select_task+ select))
	val)

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(unless (eql (defq child (. val :find :child)) (const (pad "" net_id_size)))
		(mail-send child ""))
	(. (. val :find :memory_bar) :sub)
	(. (. val :find :task_bar) :sub))

(defun poll (key val)
	; (poll key val)
	;function called to poll entry
	(unless (eql (defq child (. val :find :child)) (const (pad "" net_id_size)))
		(mail-send child (elem +select_reply+ select))))

(defun main ()
	;add window
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(defq global_tasks (Global create destroy))
	(mail-timeout (elem +select_nodes+ select) 1)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main+)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
						;close button
						(setq id nil))
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
			((= idx +select_task+)
				;child launch responce
				(defq child (slice +long_size+ (const (+ +long_size+ net_id_size)) msg)
					val (. global_tasks :find (slice +long_size+ -1 child)))
				(when val
					(.-> val
						(:insert :child child)
						(:insert :timestamp (pii-time)))))
			((= idx +select_reply+)
				;child poll responce
				(when (defq val (. global_tasks :find (slice sample_reply_node node_id_size msg)))
					(defq task_val (get-uint msg sample_reply_task_count)
						memory_val (get-uint msg sample_reply_mem_used)
						task_bar (. val :find :task_bar)
						memory_bar (. val :find :memory_bar))
					(setq max_memory (max max_memory memory_val) max_tasks (max max_tasks task_val))
					(def task_bar :maximum last_max_tasks :value task_val)
					(def memory_bar :maximum last_max_memory :value memory_val)
					(. task_bar :dirty) (. memory_bar :dirty)
					(. val :insert :timestamp (pii-time))))
			(t	;timer event
				(mail-timeout (elem +select_nodes+ select) rate)
				(when (. global_tasks :refresh retry_timeout)
					;nodes have mutated
					(defq size (. global_tasks :size))
					(def memory_grid :grid_height size)
					(def task_grid :grid_height size)
					(. memory_grid :layout)
					(. task_grid :layout)
					(bind '(x y w h) (apply view-fit
						(cat (. mywindow :get_pos) (. mywindow :pref_size))))
					(. mywindow :change_dirty x y w h))
				;set scales
				(defq task_scale (. task_scale_grid :children)
					memory_scale (. memory_scale_grid :children))
				(each (lambda (st sm)
					(defq vt (* (inc _) (/ (* last_max_tasks 100) (length task_scale)))
						vm (* (inc _) (/ (* last_max_memory 100) (length memory_scale))))
					(def st :text (str (/ vt 100) "." (pad (% vt 100) 2 "0") "|"))
					(def sm :text (str (/ vm 102400) "|"))
					(. st :layout) (. sm :layout)) task_scale memory_scale)
				(. task_scale_grid :dirty_all)
				(. memory_scale_grid :dirty_all)
				(setq last_max_memory max_memory last_max_tasks max_tasks max_memory 1 max_tasks 1)
				;poll all nodes
				(. global_tasks :each poll))))
	;close window and children
	(. global_tasks :close)
	(each mail-free-mbox (slice 1 -1 select))
	(. mywindow :hide))
