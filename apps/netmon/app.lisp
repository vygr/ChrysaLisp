(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +task_scale_size 10 max_tasks +task_scale_size +task_align 10
	+mem_scale_size 4 +mem_align (* 1024 4096)
	max_used (* 1024 16384) max_alloc (* 1024 16384)
	id :t +poll_rate (/ 1000000 4)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1 :flow_flags +flow_down_fill :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green)
			(ui-label _ (:text "Tasks" :color +argb_white))
			(ui-grid task_scale_grid (:grid_width +task_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +task_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid task_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_yellow)
			(ui-label _ (:text "Alloc (kb)" :color +argb_white))
			(ui-grid alloc_scale_grid (:grid_width +mem_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +mem_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid alloc_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_red)
			(ui-label _ (:text "Used (kb)" :color +argb_white))
			(ui-grid used_scale_grid (:grid_width +mem_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +mem_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid used_grid (:grid_width 1)))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq ub (Progress) ab (Progress) tb (Progress) node (emap))
		(:insert :timestamp now)
		(:insert :used_bar ub)
		(:insert :alloc_bar ab)
		(:insert :task_bar tb))
	(. used_grid :add_child ub)
	(. alloc_grid :add_child ab)
	(. task_grid :add_child tb)
	(open-task "apps/netmon/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	node)

(defun destroy (key node)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. node :find :child)) (mail-send child ""))
	(.-> node (:find :used_bar) :sub)
	(.-> node (:find :alloc_bar) :sub)
	(.-> node (:find :task_bar) :sub))

(defun update-scale (scale max_scale inc_scale)
	(setq scale (.-> scale :dirty_all :children))
	(each (lambda (mark)
		(defq val (* (inc _) (/ (* max_scale 100) (length scale))))
		(def mark :text (str (/ val inc_scale) "|"))
		(. mark :layout)) scale))

(defun update-result (node val max_val max_align bsym)
	(defq bar (. node :find bsym))
	(setq max_val (align (max max_val val) max_align))
	(def bar :maximum max_val :value val)
	(. bar :dirty)
	max_val)

(defun main ()
	(defq select (alloc-select +select_size))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(. *window* :set_flags +view_flag_at_front +view_flag_at_front)
	(gui-add-front (. *window* :change x y w h))
	(defq global_tasks (Global create destroy) poll_que (list))
	(mail-timeout (elem-get +select_nodes select) 1 0)
	(while id
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((= id +event_min)
						;min button
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))
					((= id +event_max)
						;max button
						(bind '(x y) (. *window* :get_pos))
						(bind '(w h) (. *window* :pref_size))
						(bind '(x y w h) (view-fit x y (/ (* w 100) 75) h))
						(. *window* :change_dirty x y w h))
					(:t (. *window* :event msg))))
			(+select_task
				;child launch responce
				(defq child (getf msg +kn_msg_reply_id)
					node (. global_tasks :find (slice +long_size -1 child)))
				(when node
					(.-> node
						(:insert :child child)
						(:insert :timestamp (pii-time)))
					(push poll_que child)))
			(+select_reply
				;child poll responce
				(when (defq node (. global_tasks :find (getf msg +reply_node)))
					(setq max_tasks (update-result node (getf msg +reply_task_count) max_tasks +task_align :task_bar)
						max_alloc (update-result node (getf msg +reply_mem_alloc) max_alloc +mem_align :alloc_bar)
						max_used (update-result node (getf msg +reply_mem_used) max_used +mem_align :used_bar))
					(. node :insert :timestamp (pii-time))
					(push poll_que (. node :find :child))))
			(:t	;polling timer event
				(mail-timeout (elem-get +select_nodes select) +poll_rate 0)
				(when (. global_tasks :refresh +retry_timeout)
					;nodes have mutated
					(. used_grid :layout) (. alloc_grid :layout) (. task_grid :layout)
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h))
				;set scales
				(update-scale task_scale_grid max_tasks 100)
				(update-scale alloc_scale_grid max_alloc 102400)
				(update-scale used_scale_grid max_used 102400)
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
