(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "lib/media/reel.inc")
(import "lib/media/mp4.inc")

;A film as a projector shows it: a reel, lib/media/reel.inc, and the codec
;that puts each frame on its canvas, at the rate the reel says.

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main timer tip))

(defq +file_types ''(".flm" ".mp4" ".m4v" ".mov") *canvas* :nil *reel* :nil *codec* :nil *at* -1 *seek* 0 *playing* :t
	*file* :nil *scale* :nil)

(defun show (n)
	;frame n on the canvas, the codec reads the packets it needs for it
	(setq *at* n *seek* n)
	(reel-frame *codec* n)
	(. *canvas* :swap +swap_write)
	(def *timeline* :value *at*)
	(. *timeline* :dirty)
	(set *status* :text (cat (str (inc *at*)) " of " (str (. *reel* :find :frames))
		", " (str (n2i (. *reel* :find :rate))) " a second"
		(if (> (. *codec* :find :scale) 1) (cat ", 1/" (str (. *codec* :find :scale)) " size") "")
		(if *playing* "" ", paused")))
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

(defun open-path (file)
	;a film by the path typed, or why not
	(cond
		((not (find (path-ext file) +file_types))
			(say (cat file ": not a kind of film this plays")))
		((not (file-stream file)) (say (cat file ": not there")))
		((not (reel-open file)) (say (cat file ": not a film it can read")))
		((win-refresh file))))

(defun say (text)
	(set *status* :text text)
	(.-> *status* :layout :dirty))

(defun fit-scale (reel)
	;the least scale, 1 to 4, that has the film in 3/4 of the screen
	(bind '(& & sw sh) (gui-info))
	(defq w (. reel :find :width) h (. reel :find :height))
	(cond
		((<= sw 0) 1)
		((some (# (and (<= (* 4 (/ w %0)) (* 3 sw)) (<= (* 4 (/ h %0)) (* 3 sh)) %0)) '(1 2 3)))
		(4)))

(defun win-refresh (file &optional scale frame)
	;the film of a file, at a scale, the one that fits the screen if not
	;said, from a frame, the first if not said
	(cond
		((not (defq reel (reel-open file))))
		((not (defq codec (reel-codec reel (ifn scale (fit-scale reel))))) (say-what reel file))
		(:t
		;the scroll lets go of the canvas it had as it is given the new one
		(if *codec* (reel-close *codec*))
		(setq *reel* reel *codec* codec *canvas* (. *codec* :find :canvas) *at* -1
			*file* file *scale* (. codec :find :scale))
		(show (ifn frame 0))
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(def *timeline* :value *at* :maximum (dec (. reel :find :frames)))
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
	(cond
		((not *reel*))
		(*playing* (show (if (>= (inc *at*) (. *reel* :find :frames)) 0 (inc *at*))))
		((/= *seek* *at*) (show *seek*))))

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
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_path)
				(open-path (trim (get :clear_text *path*))))
			((and *reel* (= id +event_timeline))
				;a drag of it sends many, only the last is shown: the tick
				;shows it if more are waiting now
				(setq *playing* :nil *seek* (get :value *timeline*))
				(unless (mail-poll select) (show *seek*)))
			((and *reel* (= (getf *msg* +ev_msg_type) +ev_type_action)
					(<= +event_smaller id +event_bigger))
				;the film again, smaller or bigger, from the frame it is at
				(defq scale (if (= id +event_smaller) (min 4 (inc *scale*)) (max 1 (dec *scale*))))
				(if (/= scale *scale*) (win-refresh *file* scale *at*)))
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
	(if *codec* (reel-close *codec*))
	(gui-sub-rpc *window*))
