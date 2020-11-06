;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'close+ 'max+ 'min+))

(ui-window window ()
	(ui-title-bar _ "Services" (0xea19 0xea1b 0xea1a) +event_close+)
	(ui-scroll info_scroll scroll_flag_vertical nil
		(ui-grid info_grid (:grid_width 3 :grid_height 1 :flow_flags flow_down_fill)
			(ui-flow service_flow nil
				(ui-label _ (:text "Service" :color +argb_white+
					:flow_flags (logior flow_flag_align_vcenter flow_flag_align_hcenter))))
			(ui-flow mbox_flow nil
				(ui-label _ (:text "Mailbox" :color +argb_white+
					:flow_flags (logior flow_flag_align_vcenter flow_flag_align_hcenter))))
			(ui-flow info_flow nil
				(ui-label _ (:text "Info" :color +argb_white+
					:flow_flags (logior flow_flag_align_vcenter flow_flag_align_hcenter)))))))

(defun populate ()
	(defq new_services (mail-enquire ""))
	(sort cmp new_services)
	(unless (and (= (length new_services) (length services))
				(if (empty? new_services) t (every eql new_services services)))
		;service directory has changed
		(each view-sub service_labels)
		(each view-sub mbox_labels)
		(each view-sub info_labels)
		(clear service_labels mbox_labels info_labels)
		(setq services new_services)
		(each (#
			(defq info (split %0 ","))
			(def (defq _ (label)) :border 1 :text (elem 0 info))
			(view-add-child service_flow _) (push service_labels _)
			(def (defq _ (label)) :border 1 :text (elem 1 info))
			(view-add-child mbox_flow _) (push mbox_labels _)
			(def (defq _ (label)) :border 1 :text (if (> (length info) 2) (elem 2 info) "No Info"))
			(view-add-child info_flow _) (push info_labels _)) new_services)
		(bind '(w h) (view-pref-size info_grid))
		(view-change info_grid 0 0 w h))
		(view-dirty-all (view-layout info_scroll)))

(defun resize (mh)
	(bind '(w h) (view-get-size info_grid))
	(setq h (min h mh))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit (cat (view-get-pos window) (view-pref-size window))))
	(undef info_scroll :min_width :min_height)
	(view-change-dirty window x y w h))

(defun main ()
	(defq id t select (array (task-mailbox)) services (list)
		service_labels (list) mbox_labels (list) info_labels (list))
	(populate)
	;add window
	(bind '(w h) (view-get-size info_grid))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(undef info_scroll :min_width :min_height)
	(gui-add (view-change window x y w h))
	;app event loop
	(while id
		;next event
		(while (defq idx (mail-poll select))
			(cond
				((= (setq id (get-long (defq msg (mail-read (elem idx select))) ev_msg_target_id)) +event_close+)
					;close button
					(setq id nil))
				((= id +event_min+)
					;min button
					(resize 256))
				((= id +event_max+)
					;max button
					(resize 640))
				(t (view-event window msg))))
		(task-sleep 10000)
		(populate))
	;close window
	(view-hide window))
