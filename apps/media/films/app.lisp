(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "lib/media/reel.inc")
(import "lib/media/mp4.inc")

;A film as a projector shows it: a reel, lib/media/reel.inc, its packets
;one a frame, and the codec that makes a frame of each on its canvas, at
;the rate the reel says. The first packet is a whole frame, and going
;back, or round again, starts from it.

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main timer tip))

(defq +file_types ''(".flm" ".mp4" ".m4v" ".mov") *canvas* :nil *reel* :nil *codec* :nil *at* -1 *playing* :t)

(defun show (n)
	;frame n on the canvas, from the frame shown or from the first
	(when (< n *at*) (setq *at* -1))
	(while (< *at* n)
		(setq *at* (inc *at*))
		(reel-decode *codec* (reel-packet *reel* *at*) *at*))
	(. *canvas* :swap +swap_write)
	(def *timeline* :value *at*)
	(. *timeline* :dirty)
	(set *status* :text (cat (str (inc *at*)) " of " (str (. *reel* :find :frames))
		", " (str (n2i (. *reel* :find :rate))) " a second" (if *playing* "" ", paused")))
	(.-> *status* :layout :dirty))

(defun play (on)
	(setq *playing* on)
	(if *reel* (show *at*)))

(defun say-what (reel file)
	;a film there is no codec for yet: what it is, and no picture
	(setq *reel* :nil)
	(set *status* :text (cat (path-name file) ": " (. reel :find :codec) " " (str (. reel :find :width)) "x"
		(str (. reel :find :height)) ", " (str (. reel :find :frames)) " frames, "
		(str (/ (n2f (n2i (* (. reel :find :rate) 100.0))) 100.0)) " a second, no codec for it here yet"))
	(.-> *status* :layout :dirty)
	(def *window_title* :text (cat "Films -> " (path-name file)))
	(. *window_title* :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun win-refresh (file)
	(cond
		((not (defq reel (reel-open file))))
		((not (reel-codec reel)) (say-what reel file))
		(:t
		;the scroll lets go of the canvas it had as it is given the new one
		(setq *reel* reel *codec* (reel-codec reel) *canvas* (. *codec* :find :canvas) *at* -1)
		(show 0)
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(def *timeline* :value 0 :maximum (dec (. reel :find :frames)))
		(def *window_title* :text (cat "Films -> " (path-name file)))
		(. *image_scroll* :add_child *canvas*)
		(. *window_title* :layout)
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *file_selector* :select_node file)
		(. *window* :change_dirty x y w h))))

;import actions and bindings directly from the images app!
(import "././images/actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun tick (select)
	;the next frame when it is due, round again after the last
	(mail-timeout (elem-get select +select_timer)
		(if *reel* (n2i (/ 1000000.0 (. *reel* :find :rate))) 33333) 0)
	(and *playing* *reel* (show (if (>= (inc *at*) (. *reel* :find :frames)) 0 (inc *at*)))))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *file_selector* :populate "." +file_types 2)
	;the window the size the film has made it
	(bind '(x y w h) (apply view-locate (. (win-refresh (cat *app_root* "data/raymarch.flm")) :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(tick select)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer) (tick select))
			((and *reel* (= (defq id (getf *msg* +ev_msg_target_id)) +event_timeline))
				(play :nil)
				(show (get :value *timeline*)))
			((and *reel* (= (getf *msg* +ev_msg_type) +ev_type_action)
					(<= +event_play id +event_start))
				(cond
					((= id +event_play) (play :t))
					((= id +event_pause) (play :nil))
					((= id +event_step) (play :nil)
						(show (if (>= (inc *at*) (. *reel* :find :frames)) 0 (inc *at*))))
					((= id +event_start) (play :nil) (show 0))))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(gui-sub-rpc *window*))
