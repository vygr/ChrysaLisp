(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +scale_size 10 +max_align +scale_size
	max_regs 10 max_memory 10 max_reals 10
	id :t +poll_rate (/ 1000000 4)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Speed" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1 :flow_flags +flow_down_fill :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green)
			(ui-label _ (:text "Regs" :color +argb_white))
			(ui-grid regs_scale_grid (:grid_width +scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid regs_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_yellow)
			(ui-label _ (:text "Memory" :color +argb_white))
			(ui-grid memory_scale_grid (:grid_width +scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid memory_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_red)
			(ui-label _ (:text "Reals" :color +argb_white))
			(ui-grid reals_scale_grid (:grid_width +scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid reals_grid (:grid_width 1)))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq ub (Progress) ab (Progress) tb (Progress) val (emap))
		(:insert :timestamp now)
		(:insert :reals_bar ub)
		(:insert :memory_bar ab)
		(:insert :regs_bar tb))
	(. reals_grid :add_child ub)
	(. memory_grid :add_child ab)
	(. regs_grid :add_child tb)
	(open-task "apps/netspeed/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	val)

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. val :find :child)) (mail-send child ""))
	(.-> val (:find :reals_bar) :sub)
	(.-> val (:find :memory_bar) :sub)
	(.-> val (:find :regs_bar) :sub))

(defun update-scale (scale max_scale)
	(each (lambda (mark)
		(defq val (* (inc _) (/ max_scale (length scale))))
		(def mark :text (str val "|"))
		(. mark :layout)) scale))

(defun main ()
	(defq select (alloc-select +select_size))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
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
					val (. global_tasks :find (slice +long_size -1 child)))
				(when val
					(.-> val
						(:insert :child child)
						(:insert :timestamp (pii-time)))
					(push poll_que child)))
			(+select_reply
				;child poll responce
				(when (defq val (. global_tasks :find (getf msg +reply_node)))
					(defq vops_regs (getf msg +reply_vops_regs)
						vops_memory (getf msg +reply_vops_memory)
						vops_reals (getf msg +reply_vops_reals)
						regs_bar (. val :find :regs_bar)
						memory_bar (. val :find :memory_bar)
						reals_bar (. val :find :reals_bar))
					(setq max_reals (align (max max_reals vops_reals) +max_align)
						max_memory (align (max max_memory vops_memory) +max_align)
						max_regs (align (max max_regs vops_regs) +max_align))
					(def regs_bar :maximum max_regs :value vops_regs)
					(def memory_bar :maximum max_memory :value vops_memory)
					(def reals_bar :maximum max_reals :value vops_reals)
					(. regs_bar :dirty) (. memory_bar :dirty) (. reals_bar :dirty)
					(. val :insert :timestamp (pii-time))
					(push poll_que (. val :find :child))))
			(:t	;polling timer event
				(mail-timeout (elem-get +select_nodes select) +poll_rate 0)
				(when (. global_tasks :refresh +retry_timeout)
					;nodes have mutated
					(. reals_grid :layout)
					(. memory_grid :layout)
					(. regs_grid :layout)
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h))
				;set scales
				(defq regs_scale (. regs_scale_grid :children)
					memory_scale (. memory_scale_grid :children)
					reals_scale (. reals_scale_grid :children))
				(update-scale regs_scale max_regs)
				(update-scale memory_scale max_memory)
				(update-scale reals_scale max_reals)
				(. regs_scale_grid :dirty_all)
				(. memory_scale_grid :dirty_all)
				(. reals_scale_grid :dirty_all)
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
