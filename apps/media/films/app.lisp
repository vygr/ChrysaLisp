(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "lib/collections/tree.inc")
(import "service/lock/app.inc")
(import "lib/media/reel.inc")
(import "lib/media/mp4.inc")

;A film as a projector shows it: a reel, lib/media/reel.inc, and the codec
;that puts each frame on its canvas, at the rate the reel says.

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main timer tip))

(defq +file_types ''(".flm" ".mp4" ".m4v" ".mov") *canvas* :nil *reel* :nil *codec* :nil *at* -1 *seek* 0 *playing* :t
	*file* :nil *scale* :nil *played_list* (list) *places* (Fmap 11) *last* :nil
	+played_max 12 +played_version 1 +played_file (cat (get :home *env*) "films.tre"))

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
			(played file :nil)
			(say (cat file ": not a kind of film this plays")))
		((not (file-stream file))
			(played file :nil)
			(say (cat file ": not there")))
		((not (reel-open file))
			(played file :nil)
			(say (cat file ": not a film it can read")))
		((win-refresh file))))

(defun say (text)
	(set *status* :text text)
	(.-> *status* :layout :dirty))

(defun played-load ()
	;the films that have played, the last first, as the user's films.tre has
	;them, where each was left, its scale, the frame it was at and if it
	;was playing, and the one shown last, which is shown first
	(defq tree (with-read-lock +played_file (tree-load (file-stream +played_file))))
	(when (and tree (eql (. tree :find :version) +played_version))
		(setq *played_list* (ifn (. tree :find :films) (list)) *last* (. tree :find :last))
		(each (lambda ((file &rest place)) (. *places* :insert file place))
			(ifn (. tree :find :places) (list)))))

(defun played-save ()
	;the places of those in the list only
	(with-write-lock +played_file
		(tree-save (file-stream +played_file +file_open_write)
			(scatter (Emap) :version +played_version :films *played_list* :last *file*
				:places (reduce (lambda (out file)
						(if (defq place (. *places* :find file)) (push out (cat (list file) place)) out))
					*played_list* (list))))))

(defun place-keep ()
	;where the film shown is, kept for when it is played again
	(and *file* *reel* (. *places* :insert *file* (list *scale* (max *at* 0) (if *playing* 1 0)))))

(defun played-show ()
	;a button a film, its name, a click plays it
	(each (# (. %0 :sub)) (. *played* :children))
	(when (nempty? *played_list*)
		(def (defq title (Title)) :text "Played")
		(view-variant title :paper :paper_title)
		(. *played* :add_child title)
		(each (lambda (file)
				(def (defq button (Button)) :text (path-name file))
				(view-variant button :font :font_button)
				(. button :connect (+ +event_played (!)))
				(. *played* :add_child button))
			*played_list*))
	;new buttons have no size till they are constrained, the first pass of
	;a layout, and the column the size it was is not laid out again by its
	;parent: the column constrained and laid out, all of it
	(.-> *left* (:constrain :t) :dirty_all))

(defun played (file ok)
	;a new film that played is put first of them, one there already keeps
	;its place, and one that did not play is let go
	(defq now (cond
		((not ok) (filter (# (nql %0 file)) *played_list*))
		((find file *played_list*) *played_list*)
		((slice (cat (list file) *played_list*) 0 (min +played_max (inc (length *played_list*)))))))
	(unless (eql (str now) (str *played_list*))
		(setq *played_list* now)
		(played-save)
		(played-show)))

(defun fit-scale (reel)
	;the least scale, 1 to 4, that has the film in 3/4 of the screen
	(bind '(& & sw sh) (gui-info))
	(defq w (. reel :find :width) h (. reel :find :height))
	(cond
		((<= sw 0) 1)
		((some (# (and (<= (* 4 (/ w %0)) (* 3 sw)) (<= (* 4 (/ h %0)) (* 3 sh)) %0)) '(1 2 3)))
		(4)))

(defun win-refresh (file &optional scale frame reel)
	;the film of a file, at a scale, from a frame, of the reel given or one
	;read from it. If not said, where it was left if it has played before,
	;else the scale that fits the screen and the first frame. Where the one
	;shown till now is, kept
	(place-keep)
	(and (not scale) (defq place (. *places* :find file))
		(setq scale (first place) frame (second place)
			*playing* (/= 0 (ifn (third place) 1))))
	(cond
		((not (setq reel (ifn reel (reel-open file)))) (played file :nil))
		((not (defq codec (reel-codec reel (ifn scale (fit-scale reel)))))
			(played file :nil)
			(say-what reel file))
		(:t
		(played file :t)
		(played-save)
		;the scroll lets go of the canvas it had as it is given the new one
		(if *codec* (reel-close *codec*))
		(setq *reel* reel *codec* codec *canvas* (. *codec* :find :canvas) *at* -1
			*file* file *scale* (. codec :find :scale))
		(show (min (ifn frame 0) (dec (. reel :find :frames))))
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
	(played-load)
	(played-show)
	;the window the size the film has made it
	;the film shown last, where it was left, else Raymarch's. Its reel read
	;the once, a film of two hours takes a second
	(bind '(x y w h) (apply view-locate (. (if (and *last* (defq reel (reel-open *last*)))
		(win-refresh *last* :nil :nil reel)
		(win-refresh (cat *app_root* "data/raymarch.flm"))) :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(tick select)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer) (tick select))
			((and (= (getf *msg* +ev_msg_type) +ev_type_action)
					(<= +event_played (defq id (getf *msg* +ev_msg_target_id)) (+ +event_played +played_max -1)))
				;one of those that played, again, its path in the field
				(when (defq file (elem-get *played_list* (- id +event_played)))
					(.-> *path* (:set_text file) :layout :dirty)
					(open-path file)))
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
				;the reel as it is, a codec of it made again at the scale
				(if (/= scale *scale*) (win-refresh *file* scale *at* *reel*)))
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
	(place-keep)
	(played-save)
	(if *codec* (reel-close *codec*))
	(gui-sub-rpc *window*))
