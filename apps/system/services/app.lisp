(import "usr/env.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close max min))

(ui-window *window* ()
	(ui-title-bar _ "Services" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-scroll info_scroll +scroll_flag_vertical (:font *env_terminal_font*)
		(ui-flow right_flow (:flow_flags +flow_right_fill)
			(ui-flow service_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Service" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter))))
			(ui-flow mbox_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Mailbox" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter))))
			(ui-flow system_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "System" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter))))
			(ui-flow info_flow (:flow_flags +flow_down_fill)
				(ui-label _ (:text "Info" :color +argb_white
					:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter)))))))

(defun resize (mh)
	(bind '(w h) (. right_flow :get_size))
	(setq h (min h mh))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun populate ()
	(defq new_services (mail-enquire ""))
	(sort new_services)
	(unless (and (= (length new_services) (length services))
				(if (empty? new_services) :t (every eql new_services services)))
		;service directory has changed
		(each (# (. %0 :sub)) service_labels)
		(each (# (. %0 :sub)) mbox_labels)
		(each (# (. %0 :sub)) system_labels)
		(each (# (. %0 :sub)) info_labels)
		(clear service_labels mbox_labels system_labels info_labels)
		(setq services new_services)
		(each (#
			(bind '(i0 i1 i2 i3) (split %0 ","))
			(def (defq _ (Label)) :border -1 :text i0)
			(. service_flow :add_child _) (push service_labels _)
			(def (defq _ (Label)) :border -1 :text i1)
			(. mbox_flow :add_child _) (push mbox_labels _)
			(def (defq _ (Label)) :border -1 :text i2)
			(. system_flow :add_child _) (push system_labels _)
			(def (defq _ (Label)) :border -1 :text i3)
			(. info_flow :add_child _) (push info_labels _)) new_services)
		(bind '(w h) (. right_flow :pref_size))
		(. right_flow :change 0 0 w h)
		(.-> info_scroll :layout :dirty_all)
		(resize 256)))

(defun main ()
	(defq id :t select (task-mboxes 1) services (list)
		service_labels (list) mbox_labels (list)
		system_labels (list) info_labels (list))
	(populate)
	;add window
	(bind '(w h) (. right_flow :get_size))
	(def info_scroll :min_width w :min_height h)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;app event loop
	(while id
		;next event
		(while (defq idx (mail-poll select))
			(cond
				((= (setq id (getf (defq msg (mail-read (elem-get select idx))) +ev_msg_target_id)) +event_close)
					;close button
					(setq id :nil))
				((= id +event_min)
					;min button
					(resize 256))
				((= id +event_max)
					;max button
					(resize 640))
				((. *window* :event msg))))
		(task-sleep 10000)
		(populate))
	;close window
	(gui-sub-rpc *window*))
