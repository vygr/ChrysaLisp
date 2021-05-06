(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close max min))

(ui-window mywindow ()
	(ui-title-bar _ "Services" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-scroll info_scroll +scroll_flag_vertical nil
		(ui-flow right_flow (:flow_flags +flow_right_fill)
			(ui-flow service_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Service" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter))))
			(ui-flow mbox_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Mailbox" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter))))
			(ui-flow info_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Info" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter)))))))

(defun resize (mh)
	(bind '(w h) (. right_flow :get_size))
	(setq h (min h mh))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit (cat (. mywindow :get_pos) (. mywindow :pref_size))))
	(. mywindow :change_dirty x y w h))

(defun populate ()
	(defq new_services (mail-enquire ""))
	(sort cmp new_services)
	(unless (and (= (length new_services) (length services))
				(if (empty? new_services) t (every eql new_services services)))
		;service directory has changed
		(each (# (. %0 :sub)) service_labels)
		(each (# (. %0 :sub)) mbox_labels)
		(each (# (. %0 :sub)) info_labels)
		(clear service_labels mbox_labels info_labels)
		(setq services new_services)
		(each (#
			(defq info (split %0 ","))
			(def (defq _ (Label)) :border 1 :text (elem 0 info))
			(. service_flow :add_child _) (push service_labels _)
			(def (defq _ (Label)) :border 1 :text (elem 1 info))
			(. mbox_flow :add_child _) (push mbox_labels _)
			(def (defq _ (Label)) :border 1 :text (if (> (length info) 2) (elem 2 info) "No Info"))
			(. info_flow :add_child _) (push info_labels _)) new_services)
		(bind '(w h) (. right_flow :pref_size))
		(. right_flow :change 0 0 w h)
		(.-> info_scroll :layout :dirty_all)
		(resize 256)))

(defun main ()
	(defq id t select (list (task-mailbox)) services (list)
		service_labels (list) mbox_labels (list) info_labels (list))
	(populate)
	;add window
	(bind '(w h) (. right_flow :get_size))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (. mywindow :change x y w h))
	;app event loop
	(while id
		;next event
		(while (defq idx (mail-poll select))
			(cond
				((= (setq id (getf (defq msg (mail-read (elem idx select))) +ev_msg_target_id)) +event_close)
					;close button
					(setq id nil))
				((= id +event_min)
					;min button
					(resize 256))
				((= id +event_max)
					;max button
					(resize 640))
				(t (. mywindow :event msg))))
		(task-sleep 10000)
		(populate))
	;close window
	(. mywindow :hide))
