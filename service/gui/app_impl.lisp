(defq _ (env))
;import into the shared boot env of this node !
;comment next line out if need to profile...
(defq _ *root_env*)
;import the GUI system in dependency order to keep machine stack usage small
(import "lib/files/files.inc" _)
(reach (# (import %0 _)) (files-all-depends '("sys/lisp.inc" "class/lisp.inc" "gui/lisp.inc") :nil 40))
(undef (env) '_)
(task-slice)

;proceed as normal now we have the root or GUI env loaded up
(import "sys/pii/lisp.inc")
(defq *env_user* "Guest")
(import "usr/Guest/env.inc")
(import *env_keyboard_map*)
(import "./actions.inc")
(import "lib/text/searching.inc")

(enums +select 0
	(enum main timer mouse))

;profiling and stack frame callbacks on the GUI thread from :draw method etc !!!
(setq *profile_map* (env 1) *stack_frame* (list) *debug_state* :gui)

;frame rate
(defq +rate (/ 1000000 60)
	;the least time the apps on this node are left between two GUI frames
	+min_gap (/ 1000000 240))

(defun mouse-type (view rx ry)
	(if view
		(defq mouse_type (case (pop (. view :type_of))
			(:Title 13)
			((:Vdu :Textfield) 2)
			(:Slider 12)
			(:Window
				(case (if (= *mouse_buttons* 0)
						(first (. view :drag_mode rx ry))
						(get :drag_mode view))
					(1 10) (2 8) (3 5) (4 10) (6 4) (8 8) (9 4) (12 5) (:t 0)))
			(:t 0)))
		(defq mouse_type 0))
	(when (or (/= *mouse_x* *old_mouse_x*)
			(/= *mouse_y* *old_mouse_y*)
			(/= *mouse_type* mouse_type))
		(setq *old_mouse_x* *mouse_x* *old_mouse_y* *mouse_y* *mouse_type* mouse_type)
		(bind '(w h) (. *mouse* :pref_size))
		(def *mouse* :offset_x 0 :offset_y (* mouse_type w -1))
		(bind '(hx hy) (case mouse_type (0 '(0 0)) (12 '(6 0)) (:t '(8 8))))
		(. *mouse* :change_dirty (- *mouse_x* hx) (- *mouse_y* hy) w w)))

(defun dispatch (msg type)
	(and (defq action (. *event_map* :find type)) (action)))

(defun close-apps (quit)
	;send quit action to all GUI apps
	;close is one of the events every app has, service/gui/lisp.inc
	(each (lambda (child)
		(when (and (nql child *mouse*) (defq mbox (. child :find_owner)))
			(defq source_id (. child :get_id))
			(mail-send mbox (setf-> (str-alloc +ev_msg_action_size)
				(+ev_msg_type +ev_type_action)
				(+ev_msg_target_id +event_close)
				(+ev_msg_action_source_id source_id)))))
		(. *screen* :children))
	;run login app or quit ?
	(if quit
		(setq *quitting* :t)
		(open-child "apps/system/login/app.lisp" +kn_call_pin)))

(defun window-title (window)
	; (window-title window) -> :nil | str
	;the text of the title nearest the top of a window, a level at a time,
	;as a widget in it, the Files of the Docs app, can have a title bar too
	(defq todo (list window) i -1 out :nil)
	(while (and (not out) (< (++ i) (length todo)))
		(if (Title? (defq v (elem-get todo i)))
			(setq out (get :text v))
			(each (# (push todo %0)) (. v :children))))
	out)

(defun find-window (want)
	; (find-window want) -> :nil | window
	;the window whose title has want in it, or the screen if want is empty
	(defq out (if (empty? want) *screen*))
	(each (lambda (child)
			(and (not out) (defq title (window-title child)) (found? title want)
				(setq out child)))
		(. *screen* :children))
	out)

(defun widget-tree (request)
	; (widget-tree request) -> str
	;the reply to a tree: (title key value depth keys), as gui-tree-rpc
	;sends them, the tree as text, or why there is none
	(bind '(title key value depth keys) (first (read (string-stream request))))
	(cond
		((not (defq view (find-window title))) "no window of that title")
		((and (nempty? key) (not (setq view (ui-find view (sym (cat ":" key)) value))))
			(cat "no " key " of " value " in it"))
		(:t (defq out (string-stream (cat "")))
			(ui-save out view (if (>= depth 0) depth) (if (nempty? keys) keys))
			(str out))))

(defun locate (request)
	; (locate request) -> str
	;the reply to a locate: (title key value), as gui-locate-rpc sends
	;them, where that view is on the screen, and the part of it that can be
	;seen, inside all it is in, (x y w h sx sy sw sh), or why it is not
	(bind '(title key value) (first (read (string-stream request))))
	(cond
		((not (defq view (find-window title))) "no window of that title")
		((and (nempty? key) (not (setq view (ui-find view (sym (cat ":" key)) value))))
			(cat "no " key " of " value " in it"))
		((eql view *screen*) (str (cat (. view :get_bounds) (. view :get_bounds))))
		(:t (bind '(x y w h) (. *screen* :get_relative view))
			(defq sx x sy y sx1 (+ x w) sy1 (+ y h) p view)
			(while (nql (setq p (penv p)) *screen*)
				(bind '(px py pw ph) (. *screen* :get_relative p))
				(setq sx (max sx px) sy (max sy py) sx1 (min sx1 (+ px pw)) sy1 (min sy1 (+ py ph))))
			(bind '(& & w0 h0) (. *screen* :get_bounds))
			(setq sx (max sx 0) sy (max sy 0) sx1 (min sx1 w0) sy1 (min sy1 h0))
			(if (or (<= sx1 sx) (<= sy1 sy))
				(cat (if (nempty? key) (cat key " of " value) "it") " is out of sight")
				(str (list x y w h sx sy (- sx1 sx) (- sy1 sy)))))))

(defun grab (want)
	; (grab want) -> str
	;what is on the screen, of the window whose title has want in it, or
	;of all of it if want is empty, as the reply to a grab: its width and
	;its height, 4 bytes each, and its pixels. Empty if there is no such
	;window, or it can not be read. The back buffer has the last frame
	(bind '(& & sw sh) (. *screen* :get_bounds))
	(defq area (if (empty? want) (list 0 0 sw sh)))
	(each (lambda (child)
			(and (not area) (defq title (window-title child)) (found? title want)
				(setq area (. child :get_bounds))))
		(. *screen* :children))
	(cond
		((not area) "")
		(:t (bind '(x y w h) area)
			;the part of it that is on the screen
			(defq x1 (min sw (+ x w)) y1 (min sh (+ y h)) x (max 0 x) y (max 0 y) w (- x1 x) h (- y1 y))
			(cond
				((or (<= w 0) (<= h 0)) "")
				((defq pixels (gui-read-screen x y w h)) (cat (char w 4) (char h 4) pixels))
				((cat "not read " (str (list x y w h))))))))

(defun main ()
	;declare service and vars
	(defq select (task-mboxes +select_size)
		service (mail-declare (task-mbox) "Gui" "GUI Service 0.2")
		;and to every node of this system, for what does not draw on it, a
		;grab or a tree of widgets from a command on another node; an app
		;finds "Gui", which only its own node has, as its views are there
		sys_service (mail-declare (task-mbox) "@Gui" "GUI Service 0.2")
		*running* :t *quitting* :nil *old_mouse_x* -1 *old_mouse_y* -1
		*mouse_type* 0 *mouse_x* 0 *mouse_y* 0 *mouse_buttons* 0 *mouse_id* 0
		*mods* 0 *key_dispatch* (Fmap) *focus* :nil)
	;init screen widget
	(def (defq *screen* (Backdrop)) :style :grid :color +argb_grey2 :ink_color +argb_grey1)
	(. (gui-init (. *screen* :change *env_window_x* *env_window_y* *env_window_width* *env_window_height*)) :dirty_all)
	(gui-update 0 0 0)
	;init mouse widget
	(defq *mouse* (canvas-load "apps/media/images/data/mice.cpm" 0))
	(setf *mouse* +view_owner_id (elem-get select +select_mouse) 0)
	(. *mouse* :set_flags +view_flag_at_front (const (+ +view_flag_solid +view_flag_at_front)))
	(. *screen* :add_front *mouse*)
	(mouse-type *screen* 0 0)
	;fire up the login app
	(open-child "apps/system/login/app.lisp" +kn_call_pin)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(let* ((idx (mail-select select)) (msg (mail-read (elem-get select idx))))
			(case idx
				(+select_main	;main mailbox
					(bind '(cmd view owner reply) (getf-> msg
						+gui_rpc_type +gui_rpc_weak_view
						+gui_rpc_owner_id +gui_rpc_reply_id))
					;a view is an object of this node, by its address, only the
					;requests of a view have one. Another's weak ref is of the
					;sender's node, and a grab or a tree may be from another node
					(setq view (if (find cmd (list +gui_rpc_type_sub +gui_rpc_type_add_front +gui_rpc_type_add_back))
						(obj-ref view)))
					(case cmd
						(+gui_rpc_type_quit
							;quit all
							(close-apps :t))
						(+gui_rpc_type_logout
							;quit all, restart login
							(close-apps :nil))
						(+gui_rpc_type_sub
							;hide and sub view
							(.-> view :hide :sub))
						(+gui_rpc_type_add_front
							;add view at front
							(. view :set_owner owner)
							(. *screen* :add_back view)
							(. view :to_front))
						(+gui_rpc_type_theme
							;every window is told, each has fonts of its own to swap
							(defq name (slice msg +gui_rpc_size -1))
							(each (lambda (child)
								(if (defq to (. child :find_owner))
									(mail-send to (cat (setf-> (str-alloc +ev_msg_theme_size)
										(+ev_msg_type +ev_type_action)
										(+ev_msg_target_id +event_theme)
										(+ev_msg_action_source_id (. child :get_id))) name))))
								(. *screen* :children))
							(. *screen* :set_flags +view_flag_dirty_all +view_flag_dirty_all))
						(+gui_rpc_type_add_back
							;add view at back
							(. view :set_owner owner)
							(. *screen* :add_back view)
							(. view :set_flags +view_flag_dirty_all +view_flag_dirty_all))
						(+gui_rpc_type_grab
							;what is on the screen, the reply
							(setq msg (grab (slice msg +gui_rpc_size -1))))
						(+gui_rpc_type_tree
							;the tree of widgets of a window, the reply
							(setq msg (widget-tree (slice msg +gui_rpc_size -1))))
						(+gui_rpc_type_locate
							;where a window or a view in it is, the reply
							(setq msg (locate (slice msg +gui_rpc_size -1))))
						(+gui_rpc_type_input
							;events as the host gives them, from a command, input,
							;dispatched as those are, then answered
							(defq in_events (slice msg +gui_rpc_size -1) in_at 0)
							(while (< in_at (length in_events))
								(defq msg (slice in_events in_at (setq in_at (+ in_at +gui_event_size))))
								(dispatch msg (getf msg +gui_event_type)))
							(setq msg "")))
					(mail-send reply msg))
				(+select_timer	;timer event
					(defq frame_start (pii-time))
					(gui-update *mouse_x* *mouse_y* 0)
					;dispatch events, roll up mouse motion
					(defq last_motion :nil)
					(while (defq msg (gui-event))
						(cond
							((= (defq type (getf msg +gui_event_type)) +gui_ev_mouse_motion)
								(setq last_motion msg))
							(:t (when last_motion
									(dispatch last_motion +gui_ev_mouse_motion)
									(setq last_motion :nil))
								(dispatch msg type))))
					(when last_motion
						(dispatch last_motion +gui_ev_mouse_motion))
					;remove orphans
					(each (# (unless (and (defq owner (. %0 :find_owner)) (mail-validate owner))
							(.-> %0 :hide :sub))) (defq children (. *screen* :children)))
					;quit if no apps
					(and *quitting* (<= (length children) 1) (setq *running* :nil))
					;the next frame is due a frame time after this one began. But
					;an update can wait in the driver for the display, and if that
					;takes the whole frame time the next would be due at once. The
					;GUI runs above the apps on its node, so they would get no time
					;at all while the screen kept changing, a window being dragged
					;did not move till the mouse stopped. So they are left some.
					(mail-timeout (elem-get select +select_timer)
						(max +min_gap (- +rate (- (pii-time) frame_start))) 0)))))
	(mail-forget service)
	(mail-forget sys_service)
	(mail-timeout (elem-get select +select_timer) 0 0)
	(each (# (. %0 :sub)) (. *screen* :children))
	(setq *mouse* :nil *screen* :nil)
	(gui-deinit)
	;the GUI has quit, and so this node goes with it, a desktop is a node.
	;The rest of its session lives on while there is another desktop, or a
	;terminal, see funcs.sh
	(pii-exit))
