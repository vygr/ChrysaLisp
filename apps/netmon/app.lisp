(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +task_scale_size 10 +mem_scale_size 4 +task_align 10
	+mem_align (* 1024 16) +poll_rate (/ 1000000 4)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1)
		(ui-hchart task_chart "Tasks" +task_scale_size (:color +argb_green))
		(ui-hchart alloc_chart "Alloc (kb)" +mem_scale_size (:units 1024 :color +argb_yellow))
		(ui-hchart used_chart "Used (kb)" +mem_scale_size (:units 1024 :color +argb_red))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq node (emap))
		(:insert :timestamp now)
		(:insert :used_bar (. used_chart :add_bar))
		(:insert :alloc_bar (. alloc_chart :add_bar))
		(:insert :task_bar (. task_chart :add_bar)))
	(open-task "apps/netmon/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	node)

(defun destroy (key node)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. node :find :child)) (mail-send child ""))
	(each (# (.-> node (:find %0) :sub)) '(:used_bar :alloc_bar :task_bar)))

(defun update-result (node &rest vals)
	(each (# (def %0 :maximum (align (max %1 (get :maximum %0)) %2)))
		(list task_chart alloc_chart used_chart) vals
		(list +task_align +mem_align +mem_align))
	(each (# (def (.-> node (:find %0) :dirty) :value %1))
		'(:task_bar :alloc_bar :used_bar) vals))

(defun main ()
	(defq id :t select (alloc-select +select_size)
		global_tasks (Global create destroy) poll_que (list))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(. *window* :set_flags +view_flag_at_front +view_flag_at_front)
	(gui-add-front (. *window* :change_dirty x y w h))
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
					(update-result node
						(getf msg +reply_task_count)
						(getf msg +reply_mem_alloc)
						(getf msg +reply_mem_used))
					(. node :insert :timestamp (pii-time))
					(push poll_que (. node :find :child))))
			(:t	;polling timer event
				(mail-timeout (elem-get +select_nodes select) +poll_rate 0)
				(when (. global_tasks :refresh +retry_timeout)
					;nodes have mutated
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h))
				;set scales
				(each (# (. %0 :update_scale))
					(list task_chart alloc_chart used_chart))
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
