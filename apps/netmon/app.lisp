;jit compile apps native functions if needed
(import "lib/asm/asm.inc")
(bind '(_ *cpu* *abi*) (split (load-path) "/"))
(make '("apps/netmon/child.vp") *abi* *cpu*)

;imports
(import "gui/lisp.inc")

(structure 'sample_reply 0
	(nodeid 'node)
	(int 'task_count 'mem_used))

(structure '+event 0
	(byte 'close+ 'max+ 'min+))

(defq task_scale (list) memory_scale (list) max_tasks 1 max_memory 1
	rate (/ 1000000 5) id t node_map (xmap)
	select (list (task-mailbox) (mail-alloc-mbox)))

(ui-window mywindow ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-grid _ (:grid_width 2 :grid_height 1 :flow_flags +flow_down_fill+ :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green+)
			(ui-label _ (:text "Tasks" :color +argb_white+))
			(ui-grid task_scale_grid (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (push task_scale (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+))))))
			(ui-grid task_grid (:grid_width 1 :grid_height 0)))
		(ui-flow _ (:color +argb_red+)
			(ui-label _ (:text "Memory (kb)" :color +argb_white+))
			(ui-grid memory_scale_grid (:grid_width 4 :grid_height 1 :color +argb_white+
					:font *env_medium_terminal_font*)
				(times 4 (push memory_scale (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter+ +flow_flag_align_hright+))))))
			(ui-grid memory_grid (:grid_width 1 :grid_height 0)))))

(defun open-monitor (node reply)
	;open remote monitor child task
	(mail-send (cat (char 0 (const long_size)) node)
		(cat (char 0 (const long_size)) reply
			(char kn_call_open (const long_size))
			"apps/netmon/child" (char 0))))

(defun refresh-nodes ()
	;scan known nodes and update node map
	(defq nodes (mail-nodes) old_nodes (list) mutated nil)
	(. node_map :each (lambda (key val) (push old_nodes key)))
	;test for new nodes
	(each (lambda (node)
		(unless (. node_map :find node)
			(setq mutated t)
			(defq info (emap) reply_mbox (mail-alloc-mbox) mb (Progress) tb (Progress))
			(. node_map :insert node info)
			(.-> info (:insert :mem_used 0)
				(:insert :task_cnt 0)
				(:insert :child (const (pad "" net_id_size)))
				(:insert :reply_select (list reply_mbox))
				(:insert :memory_bar mb)
				(:insert :task_bar tb))
			(. memory_grid :add_child mb)
			(. task_grid :add_child tb)
			(open-monitor node reply_mbox))) nodes)
	;test for vanished nodes
	(each (lambda (node)
		(unless (find node nodes)
			(setq mutated t)
			(defq info (. node_map :find node))
			(mail-send (. info :find :child) "")
			(mail-free-mbox (pop (. info :find :reply_select)))
			(. (. info :find :memory_bar) :sub)
			(. (. info :find :task_bar) :sub))) old_nodes)
	(def memory_grid :grid_height (length nodes))
	(def task_grid :grid_height (length nodes))
	mutated)

(defun poll-nodes ()
	(defq new_max_memory 1 new_max_tasks 1)
	(. node_map :each (lambda (key val)
		(when (mail-poll (defq select (. val :find :reply_select)))
			;reply mail waiting, is it the childs first responce ?
			(defq msg (mail-read (elem 0 select)))
			(cond
				((eql (defq child (. val :find :child)) (const (pad "" net_id_size)))
					;yes so set the task id
					(setq child (slice (const long_size) (const (+ long_size net_id_size)) msg))
					(. val :insert :child child))
				(t	;no it's a polling reply
					(defq task_val (get-uint msg sample_reply_task_count)
						memory_val (get-uint msg sample_reply_mem_used)
						task_bar (. val :find :task_bar)
						memory_bar (. val :find :memory_bar))
					(setq new_max_memory (max new_max_memory memory_val)
						new_max_tasks (max new_max_tasks task_val))
					(def task_bar :maximum max_tasks :value task_val)
					(def memory_bar :maximum max_memory :value memory_val)
					(. task_bar :dirty) (. memory_bar :dirty)))
			;send off new poll
			(mail-send child (elem 0 select)))))
	(setq max_memory new_max_memory max_tasks new_max_tasks))

(defun close-children ()
	;close all child tasks
	(. node_map :each (lambda (key val)
		(mail-send (. val :find :child) "")
		(mail-free-mbox (pop (. val :find :reply_select))))))

(defun main ()
	;add window
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(mail-timeout (elem -2 select) 1)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx 0)
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
			(t	;timer event
				(mail-timeout (elem -2 select) rate)
				(when (refresh-nodes)
					;nodes have mutated
					(bind '(x y w h) (apply view-fit
						(cat (. mywindow :get_pos) (. mywindow :pref_size))))
					(. mywindow :change_dirty x y w h))
				;poll node values
				(poll-nodes)
				;set scales
				(each (lambda (st sm)
					(defq vt (* (inc _) (/ (* max_tasks 100) (length task_scale)))
						vm (* (inc _) (/ (* max_memory 100) (length memory_scale))))
					(def st :text (str (/ vt 100) "." (pad (% vt 100) 2 "0") "|"))
					(def sm :text (str (/ vm 102400) "|"))
					(. st :layout) (. sm :layout)) task_scale memory_scale)
				(. task_scale_grid :dirty_all) (. memory_scale_grid :dirty_all))))
	;close window and children
	(mail-free-mbox (pop select))
	(close-children)
	(. mywindow :hide))
