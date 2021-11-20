(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq task_scale_size 10 max_tasks task_scale_size last_max_tasks max_tasks
	used_scale_size 4 max_used (* 1024 16384) last_max_used max_used
	alloc_scale_size 4 max_alloc (* 1024 16384) last_max_alloc max_alloc
	id t rate (/ 1000000 2) +mem_align (* 1024 4096)
	retry_timeout (if (starts-with "obj/vp64" (load-path)) 10000000 1000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Monitor" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1 :flow_flags +flow_down_fill :maximum 100 :value 0)
		(ui-flow _ (:color +argb_green)
			(ui-label _ (:text "Tasks" :color +argb_white))
			(ui-grid task_scale_grid (:grid_width task_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times task_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid task_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_yellow)
			(ui-label _ (:text "Alloc (kb)" :color +argb_white))
			(ui-grid alloc_scale_grid (:grid_width alloc_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times alloc_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid alloc_grid (:grid_width 1)))
		(ui-flow _ (:color +argb_red)
			(ui-label _ (:text "Used (kb)" :color +argb_white))
			(ui-grid used_scale_grid (:grid_width used_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times used_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid used_grid (:grid_width 1)))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq ub (Progress) ab (Progress) tb (Progress) val (emap))
		(:insert :child (const (pad "" +net_id_size)))
		(:insert :timestamp now)
		(:insert :used_bar ub)
		(:insert :alloc_bar ab)
		(:insert :task_bar tb))
	(. used_grid :add_child ub)
	(. alloc_grid :add_child ab)
	(. task_grid :add_child tb)
	(open-task "apps/netmon/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	val)

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(unless (eql (defq child (. val :find :child)) (const (pad "" +net_id_size)))
		(mail-send child ""))
	(.-> val (:find :used_bar) :sub)
	(.-> val (:find :alloc_bar) :sub)
	(.-> val (:find :task_bar) :sub))

(defun poll (key val)
	; (poll key val)
	;function called to poll entry
	(unless (eql (defq child (. val :find :child)) (const (pad "" +net_id_size)))
		(mail-send child (elem-get +select_reply select))))

(defun main ()
	(defq select (alloc-select +select_size))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(defq global_tasks (Global create destroy))
	(mail-timeout (elem-get +select_nodes select) 1 0)
	(while id
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main)
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id nil))
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
					(t (. *window* :event msg))))
			((= idx +select_task)
				;child launch responce
				(defq child (getf msg +kn_msg_reply_id)
					val (. global_tasks :find (slice +long_size -1 child)))
				(when val
					(.-> val
						(:insert :child child)
						(:insert :timestamp (pii-time)))))
			((= idx +select_reply)
				;child poll responce
				(when (defq val (. global_tasks :find (getf msg +reply_node)))
					(defq task_val (getf msg +reply_task_count)
						alloc_val (getf msg +reply_mem_alloc)
						used_val (getf msg +reply_mem_used)
						task_bar (. val :find :task_bar)
						alloc_bar (. val :find :alloc_bar)
						used_bar (. val :find :used_bar))
					(setq max_used (align (max max_used used_val) +mem_align)
						max_alloc (align (max max_alloc alloc_val) +mem_align)
						max_tasks (align (max max_tasks task_val) (length task_scale)))
					(def task_bar :maximum last_max_tasks :value task_val)
					(def alloc_bar :maximum last_max_alloc :value alloc_val)
					(def used_bar :maximum last_max_used :value used_val)
					(. task_bar :dirty) (. alloc_bar :dirty) (. used_bar :dirty)
					(. val :insert :timestamp (pii-time))))
			(t  ;timer event
				(mail-timeout (elem-get +select_nodes select) rate 0)
				(when (. global_tasks :refresh retry_timeout)
					;nodes have mutated
					(defq size (. global_tasks :size))
					(. used_grid :layout)
					(. alloc_grid :layout)
					(. task_grid :layout)
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h))
				;set scales
				(defq task_scale (. task_scale_grid :children)
					alloc_scale (. alloc_scale_grid :children)
					used_scale (. used_scale_grid :children))
				(each (lambda (mark)
					(defq val (* (inc _) (/ (* last_max_tasks 100) (length task_scale))))
					(def mark :text (str (/ val 100) "|"))
					(. mark :layout)) task_scale)
				(each (lambda (mark)
					(defq val (* (inc _) (/ (* last_max_alloc 100) (length alloc_scale))))
					(def mark :text (str (/ val 102400) "|"))
					(. mark :layout)) alloc_scale)
				(each (lambda (mark)
					(defq val (* (inc _) (/ (* last_max_used 100) (length used_scale))))
					(def mark :text (str (/ val 102400) "|"))
					(. mark :layout)) used_scale)
				(. task_scale_grid :dirty_all)
				(. alloc_scale_grid :dirty_all)
				(. used_scale_grid :dirty_all)
				(setq last_max_used max_used last_max_alloc max_alloc last_max_tasks max_tasks
					max_alloc (* 1024 16384) max_used (* 1024 16384) max_tasks task_scale_size)
				;poll all nodes
				(. global_tasks :each poll))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
