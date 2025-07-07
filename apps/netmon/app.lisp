(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +task_scale_size 10 +mem_scale_size 4 +task_align 10 +stack_scale_size 4
	+mem_align (* 1024 16) +stack_align 1024 +poll_rate (/ 1000000 4)
	+bars ''(:task_bar :alloc_bar :used_bar :stack_bar)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid *charts* (:grid_height 1)
		(ui-hchart _ "Tasks" +task_scale_size (:color +argb_green))
		(ui-hchart _ "Alloc (kb)" +mem_scale_size (:units 1024 :color +argb_yellow))
		(ui-hchart _ "Used (kb)" +mem_scale_size (:units 1024 :color +argb_red))
		(ui-hchart _ "Stack (b)" +stack_scale_size (:color +argb_cyan))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(def (defq node (env 1)) :timestamp now)
	(each (# (def node %0 (. %1 :add_bar))) +bars charts)
	(open-task "apps/netmon/child.lisp" key +kn_call_open 0 (elem-get select +select_task))
	node)

(defun destroy (key node)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child node)) (mail-send child ""))
	(each (# (. (get %0 node) :sub)) +bars))

(defun update-result (node &rest vals)
	(each (# (def %0 :maximum (align (max %2 (get :maximum %0)) %3))
			(def (. (get %1 node) :dirty) :value %2))
		charts +bars vals (list +task_align +mem_align +mem_align +stack_align)))

(defun main ()
	(defq id :t select (alloc-select +select_size)
		global_tasks (Global create destroy) poll_que (list)
		charts (. *charts* :children))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(. *window* :set_flags +view_flag_at_front +view_flag_at_front)
	(gui-add-front-rpc (. *window* :change_dirty x y w h))
	(mail-timeout (elem-get select +select_nodes) 1 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
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
						(bind '(x y w h) (view-fit x y (/ (* w 100) 80) h))
						(. *window* :change_dirty x y w h))
					(:t (. *window* :event msg))))
			(+select_task
				;child launch response
				(defq child (getf msg +kn_msg_reply_id)
					node (. global_tasks :find (slice child +long_size -1)))
				(when node
					(def node :child child :timestamp (pii-time))
					(push poll_que child)))
			(+select_reply
				;child poll response
				(when (defq node (. global_tasks :find (getf msg +reply_node)))
					(update-result node
						(getf msg +reply_task_count)
						(getf msg +reply_mem_alloc)
						(getf msg +reply_mem_used)
						(getf msg +reply_max_stack))
					(def node :timestamp (pii-time))
					(push poll_que (get :child node))))
			(:t ;polling timer event
				(mail-timeout (elem-get select +select_nodes) +poll_rate 0)
				(when (. global_tasks :refresh +retry_timeout)
					;nodes have mutated
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h)
					(each (# (. %0 :layout_bars)) charts))
				;set scales
				(each (# (. %0 :update_scale)) charts)
				;poll any ready children
				(each (# (mail-send %0 (elem-get select +select_reply))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub-rpc *window*))
