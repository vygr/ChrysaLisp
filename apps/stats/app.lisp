;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq stat_data (list) stat_scale (list) cpu_total (kernel-total) frame_cnt 0
	cpu_count cpu_total max_stats 1 last_max_stats 0
	farm (open-farm "apps/stats/child.lisp" cpu_total kn_call_open) last_max_classes 0 max_classes 1
	select (array (task-mailbox) (mail-alloc-mbox)) sample_msg (array (elem 1 select)))

(ui-window window ()
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-flow _ ('flow_flags flow_left_fill
				'font (create-font "fonts/Entypo.ctf" 22) 'color title_col)
			(ui-buttons (0xea19) (const event_win_close))
			(ui-title _ ('text "Object Monitor" 'font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-grid _ ('grid_width 2 'grid_height 1 'flow_flags flow_down_fill 'maximum 100 'value 0)
			(ui-flow name_flow ('color argb_grey8)
				(ui-label _ ('text "Class" 'color argb_white))
				(ui-grid _ ('grid_width 1 'grid_height 1 'color argb_white 'font (create-font "fonts/Hack-Regular.ctf" 14))
					(ui-label _ ('text "")))
				(ui-view name_view))
			(ui-flow stat_flow ('color argb_red)
				(ui-label _ ('text "Count" 'color argb_white))
				(ui-grid _ ('grid_width 4 'grid_height 1 'color argb_white 'font (create-font "fonts/Hack-Regular.ctf" 14))
					(times 4 (push stat_scale (ui-label _
						('text "|" 'flow_flags (logior flow_flag_align_vcenter flow_flag_align_hright))))))
				(ui-view stat_view)))))

(defun-bind main ()
	(while (progn
		;new batch of samples ?
		(when (= cpu_count cpu_total)
			;set scales
			(setq last_max_stats max_stats max_stats 1)
			(each (lambda (stat)
				(defq vt (* (inc _) (/ (* last_max_stats 100) (length stat_scale))))
				(def stat 'text (str (/ vt 100) "|"))
				(view-layout stat)) stat_scale)
			;build new stats info
			(sort (lambda (x y)
				(- (elem 1 y) (elem 1 x))) stat_data)
			(defq new_name_view (create-grid) new_stat_view (create-grid))
			(def new_name_view 'grid_width 1 'grid_height max_classes)
			(def new_stat_view 'grid_width 1 'grid_height max_classes)
			(each (lambda ((name stat))
				(defq n (create-label) p (create-progress))
				(def n 'border 0 'text name)
				(def p 'maximum last_max_stats 'value stat)
				(view-add-child new_name_view n)
				(view-add-child new_stat_view p)) stat_data)
			(view-sub stat_view) (view-sub name_view)
			(view-add-child name_flow (setq name_view new_name_view))
			(view-add-child stat_flow (setq stat_view new_stat_view))
			(view-layout name_flow) (view-layout stat_flow)
			(view-dirty-all window)
			;open the window once we have data
			(when (= (setq frame_cnt (inc frame_cnt)) 2)
				(gui-add (apply view-change (cat (list window 640 32) (view-pref-size window)))))
			;resize if number of classes change
			(when (/= last_max_classes max_classes)
				(setq last_max_classes max_classes)
				(apply view-change-dirty (cat (list window) (view-get-pos window) (view-pref-size window))))
			;send out multi-cast sample command
			(while (/= cpu_count 0)
				(setq cpu_count (dec cpu_count))
				(mail-send sample_msg (elem cpu_count farm)))
			(clear stat_data) (setq max_classes 1))

		;wait for next event
		(defq id (mail-select select) msg (mail-read (elem id select)))
		(cond
			((= id 0)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) event_win_close)
						;close button
						nil)
					(t (view-event window msg))))
			(t	;child info, merge with current frames information
				(bind '(data _) (read (string-stream msg) (ascii-code " ")))
				(setq max_classes (max max_classes (length data)))
				(each (lambda (ent)
					(bind '(name stat) ent)
					(if (defq _ (some (lambda (_) (if (eql (elem 0 _) name) _)) stat_data))
						(progn
							(elem-set 1 _ (+ (elem 1 _) stat))
							(setq max_stats (max max_stats (elem 1 _))))
						(progn
							(push stat_data ent)
							(setq max_stats (max max_stats stat))))) data)
				;count up replies
				(task-sleep 10000)
				(setq cpu_count (inc cpu_count))))))
	;close window and children
	(view-hide window)
	(mail-free-mbox (elem 1 select))
	(while (defq mbox (pop farm))
		(mail-send (const (char event_win_close long_size)) mbox)))
