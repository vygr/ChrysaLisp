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
	+bars ''(:regs_bar :memory_bar :reals_bar)
	+results ''(:regs_results :memory_results :reals_results)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Speed" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid net_charts (:grid_height 1)
		(ui-hchart _ "Net Regs (bops/s)" +scale_size (:units +bops :color +argb_green))
		(ui-hchart _ "Net Memory (bops/s)" +scale_size (:units +bops :color +argb_yellow))
		(ui-hchart _ "Net Reals (mops/s)" +scale_size (:units +mops :color +argb_red)))
	(ui-grid charts (:grid_height 1)
		(ui-hchart _ "Regs (bops/s)" +scale_size (:units +bops :color +argb_green))
		(ui-hchart _ "Memory (bops/s)" +scale_size (:units +bops :color +argb_yellow))
		(ui-hchart _ "Reals (mops/s)" +scale_size (:units +mops :color +argb_red))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(. (defq node (emap)) :insert :timestamp now)
	(each (# (.-> node (:insert %0 (. %2 :add_bar)) (:insert %1 (list))))
		+bars +results charts)
	(open-task "apps/netspeed/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	node)

(defun destroy (key node)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. node :find :child)) (mail-send child ""))
	(each (# (.-> node (:find %0) :sub)) +bars))

(defun smooth-result (results val)
	(if (> (length (push results val)) +smooth_steps)
		(setq results (slice 1 -1 results)))
	(list results (/ (reduce + results 0) (length results))))

(defun update-result (node &rest vals)
	(setq vals (map (# (bind '(results val) (smooth-result (. node :find %0) %1)) (. node :insert %0 results) val)
		+results vals))
	(each (# (def %0 :maximum (align (max %2 (get :maximum %0)) %3))
			(def (.-> node (:find %1) :dirty) :value %2))
		charts +bars vals (list +max_bops_align +max_bops_align +max_mops_align)))

(defun update-net-result ()
	(bind '(regs_chart memory_chart reals_chart) charts)
	(bind '(regs_results memory_results reals_results) net_results)
	(bind '(regs_results total_regs) (smooth-result regs_results
		(reduce (# (+ %0 (get :value %1))) (.-> regs_chart :get_bar_grid :children) 0)))
	(bind '(mem_results total_mem) (smooth-result memory_results
		(reduce (# (+ %0 (get :value %1))) (.-> memory_chart :get_bar_grid :children) 0)))
	(bind '(real_results total_real) (smooth-result reals_results
		(reduce (# (+ %0 (get :value %1))) (.-> reals_chart :get_bar_grid :children) 0)))
	(setq net_results (list regs_results memory_results reals_results))
	(each (# (def %0 :maximum (align (max %2 (get :maximum %0)) %3))
			(def (. %1 :dirty) :value %2))
		net_charts net_bars
		(list total_regs total_mem total_real)
		(list +max_bops_align +max_bops_align +max_mops_align)))

(defun main ()
	(defq id :t select (alloc-select +select_size)
		charts (. charts :children) net_charts (. net_charts :children)
		net_bars (map (# (. %0 :add_bar)) net_charts)
		net_results (list (list) (list) (list))
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
				(each (# (. %0 :update_scale)) (cat charts net_charts))
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
