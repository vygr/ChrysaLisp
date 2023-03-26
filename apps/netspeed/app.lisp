(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main task reply nodes))

(defq +vops_scale_size 4 +vops_align (* 1024 4096) max_vops (* 1024 16384)
	id :t +poll_rate (/ 1000000 4)
	+retry_timeout (if (starts-with "obj/vp64" (load-path)) 20000000 2000000))

(ui-window *window* ()
	(ui-title-bar _ "Network Speed" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-grid _ (:grid_height 1 :flow_flags +flow_down_fill :maximum 100 :value 0)
		(ui-flow _ (:color +argb_red)
			(ui-label _ (:text "VPSpeed (vops)" :color +argb_white))
			(ui-grid vops_scale_grid (:grid_width +vops_scale_size :grid_height 1 :color +argb_white
					:font *env_medium_terminal_font*)
				(times +vops_scale_size (ui-label _
					(:text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright)))))
			(ui-grid vops_grid (:grid_width 1)))))

(defun create (key now)
	; (create key now) -> val
	;function called when entry is created
	(.-> (defq ub (Progress) ab (Progress) tb (Progress) val (emap))
		(:insert :timestamp now)
		(:insert :vops_bar ab))
	(. vops_grid :add_child ab)
	(open-task "apps/netspeed/child.lisp" key +kn_call_open 0 (elem-get +select_task select))
	val)

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. val :find :child)) (mail-send child ""))
	(.-> val (:find :vops_bar) :sub))

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
					(defq vops_val (getf msg +reply_vp_speed)
						vops_bar (. val :find :vops_bar))
					(setq max_vops (align (max max_vops vops_val) +vops_align))
					(def vops_bar :maximum max_vops :value vops_val)
					(. vops_bar :dirty)
					(. val :insert :timestamp (pii-time))
					(push poll_que (. val :find :child))))
			(:t	;polling timer event
				(mail-timeout (elem-get +select_nodes select) +poll_rate 0)
				(when (. global_tasks :refresh +retry_timeout)
					;nodes have mutated
					(. vops_grid :layout)
					(bind '(x y w h) (apply view-fit
						(cat (. *window* :get_pos) (. *window* :pref_size))))
					(. *window* :change_dirty x y w h))
				;set scales
				(defq vops_scale (. vops_scale_grid :children))
				(each (lambda (mark)
					(defq val (* (inc _) (/ (* max_vops 100) (length vops_scale))))
					(def mark :text (str (/ val 102400) "|"))
					(. mark :layout)) vops_scale)
				(. vops_scale_grid :dirty_all)
				;poll any ready children
				(each (# (mail-send %0 (elem-get +select_reply select))) poll_que)
				(clear poll_que))))
	;close window and children
	(. global_tasks :close)
	(free-select select)
	(gui-sub *window*))
