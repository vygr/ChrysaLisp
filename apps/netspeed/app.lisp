(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +scale_size 5 +bops 1000000000 +mops 1000000
	+max_bops_align (* +scale_size +bops) +max_mops_align  (* +scale_size +mops)
	+smooth_steps 5 +poll_rate (/ 1000000 4)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Speed" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1)
		(ui-hchart net_regs_chart "Net Regs (bops/s)" +scale_size (:units +bops :color +argb_green))
		(ui-hchart net_memory_chart "Net Memory (bops/s)" +scale_size (:units +bops :color +argb_yellow))
		(ui-hchart net_reals_chart "Net Reals (mops/s)" +scale_size (:units +mops :color +argb_red)))
	(ui-grid _ (:grid_height 1)
		(ui-hchart regs_chart "Regs (bops/s)" +scale_size (:units +bops :color +argb_green))
		(ui-hchart memory_chart "Memory (bops/s)" +scale_size (:units +bops :color +argb_yellow))
		(ui-hchart reals_chart "Reals (mops/s)" +scale_size (:units +mops :color +argb_red))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq node (emap))
		(:insert :timestamp now)
		(:insert :regs_bar (. regs_chart :add_bar))
		(:insert :memory_bar (. memory_chart :add_bar))
		(:insert :reals_bar (. reals_chart :add_bar))
		(:insert :regs_results (list))
		(:insert :memory_results (list))
		(:insert :reals_results (list)))
	(open-task "apps/netspeed/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	node)

(defun destroy (key node)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. node :find :child)) (mail-send child ""))
	(each (# (.-> node (:find %0) :sub)) '(:regs_bar :memory_bar :reals_bar)))

(defun smooth-result (results val)
	(if (> (length (push results val)) +smooth_steps)
		(setq results (slice 1 -1 results)))
	(list results (/ (reduce + results 0) (length results))))

(defun update-result (node &rest vals)
	(setq vals (map (# (bind '(results val) (smooth-result (. node :find %0) %1)) (. node :insert %0 results) val)
		'(:regs_results :memory_results :reals_results) vals))
	(each (# (def %0 :maximum (align (max %2 (get :maximum %0)) %3))
			(def (.-> node (:find %1) :dirty) :value %2))
		(list regs_chart memory_chart reals_chart) '(:regs_bar :memory_bar :reals_bar)
		vals (list +max_bops_align +max_bops_align +max_mops_align)))

(defun update-net-result ()
	(bind '(regs_results total_regs) (smooth-result net_regs_results
		(reduce (# (+ %0 (get :value %1))) (.-> regs_chart :get_bar_grid :children) 0)))
	(bind '(mem_results total_mem) (smooth-result net_memory_results
		(reduce (# (+ %0 (get :value %1))) (.-> memory_chart :get_bar_grid :children) 0)))
	(bind '(real_results total_real) (smooth-result net_reals_results
		(reduce (# (+ %0 (get :value %1))) (.-> reals_chart :get_bar_grid :children) 0)))
	(setq net_regs_results regs_results net_memory_results mem_results net_reals_results real_results)
	(def net_regs_chart :maximum (align (max total_regs (get :maximum net_regs_chart)) +max_bops_align))
	(def net_memory_chart :maximum (align (max total_mem (get :maximum net_memory_chart)) +max_bops_align))
	(def net_reals_chart :maximum (align (max total_real (get :maximum net_reals_chart)) +max_mops_align))
	(def (. net_regs_bar :dirty) :value total_regs)
	(def (. net_memory_bar :dirty) :value total_mem)
	(def (. net_reals_bar :dirty) :value total_real))

(defun main ()
	(defq id :t select (alloc-select +select_size)
		net_regs_bar (. net_regs_chart :add_bar)
		net_memory_bar (. net_memory_chart :add_bar)
		net_reals_bar (. net_reals_chart :add_bar)
		net_regs_results (list) net_memory_results (list) net_reals_results (list)
		global_tasks (Global create destroy) poll_que (list))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
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
						(getf msg +reply_vops_regs)
						(getf msg +reply_vops_memory)
						(getf msg +reply_vops_reals))
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
				(update-net-result)
				(each (# (. %0 :update_scale))
					(list regs_chart memory_chart reals_chart
						net_regs_chart net_memory_chart net_reals_chart))
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
