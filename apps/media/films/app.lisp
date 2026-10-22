(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "apps/system/files/picker.inc")
(import "lib/files/names.inc")
(import "lib/files/files.inc")
(import "lib/collections/tree.inc")
(import "service/lock/app.inc")
(import "lib/media/reel.inc")
(import "lib/media/mp4.inc")

;A film as a projector shows it: a reel, lib/media/reel.inc, and the codec
;that puts each frame on its canvas, at the rate the reel says, or kept to
;its sound as it is heard, ./sound.inc.

;our UI widgets
(import "./widgets.inc")
(import "./sound.inc")

(enums +select 0
	(enum main timer tip picker))

(defq +file_types ''(".flm" ".mp4" ".m4v" ".mov" ".mp3" ".flac" ".mkv" ".webm") *canvas* :nil *reel* :nil *codec* :nil *at* -1 *seek* 0 *playing* :t
	*file* :nil *picker* :nil *scale* :nil *played_list* (list) *places* (Fmap 11) *last* :nil *sound* :nil *sync* 0 +sync_step 40
	+played_max 12 +played_version 1 +played_file (cat (get :home *env*) "films.tre"))

(defun frame-time (n)
	; (frame-time n) -> seconds
	;when frame n is shown, of a film of an mp4, its time less where its
	;edit list starts it, else by the rate, frames of an MP3 are evenly
	;spaced
	(if (defq video (. *reel* :find :video))
		(/ (n2f (- (elem-get (. *reel* :find :shown) (elem-get (. *codec* :find :order) n)) (. *reel* :find :start)))
			(n2f (. video :find :timescale)))
		(/ (n2f n) (. *reel* :find :rate))))

(defun frame-at (time)
	; (frame-at seconds) -> n
	;the last frame shown by a time, the frames being in the order shown
	(defq low 0 high (dec (. *reel* :find :frames)))
	(cond
		((not (. *reel* :find :video)) (max low (min high (n2i (* time (. *reel* :find :rate))))))
		(:t (while (< low high)
				(defq mid (>> (+ low high 1) 1))
				(if (<= (frame-time mid) time) (setq low mid) (setq high (dec mid))))
			low)))

(defun clock-of (time)
	; (clock-of seconds) -> str
	;h:mm:ss of a time
	(defq s (n2i time))
	(cat (str (/ s 3600)) ":" (pad (str (% (/ s 60) 60)) 2 "0") ":" (pad (str (% s 60)) 2 "0")))

(defun say (text)
	;what is shown, in the line at the foot
	(set *status* :text text)
	(.-> *status* :layout :dirty))

(defun show (n)
	;frame n on the canvas, the codec reads the packets it needs for it
	(setq *at* n *seek* n)
	(reel-frame *codec* n)
	(. *canvas* :swap +swap_write)
	(def *timeline* :value *at*)
	(. *timeline* :dirty)
	(say (cat (if (. *reel* :find :audio)
			(cat (clock-of (frame-time *at*)) " of " (clock-of (frame-time (. *reel* :find :frames))))
			(cat (str (inc *at*)) " of " (str (. *reel* :find :frames))
				", " (str (n2i (. *reel* :find :rate))) " a second"))
		(if (> (. *codec* :find :scale) 1) (cat ", 1/" (str (. *codec* :find :scale)) " size") "")
		(cond ((> *sync* 0) (cat ", sound +" (str *sync*) "ms")) ((< *sync* 0) (cat ", sound " (str *sync*) "ms")) (""))
		(if *playing* "" ", paused"))))

(defun sound-on? ()
	(and *sound* *playing* (. *sound* :find :handle)))

(defun play (on)
	;the sound started from the frame shown, or stopped
	(setq *playing* on)
	(cond
		((and on *sound*) (sound-start *sound* (frame-time *at*)))
		(*sound* (sound-stop *sound*)))
	(if *reel* (show *at*)))

(defun say-what (reel file)
	;a film there is no codec for yet: what it is, and no picture
	(setq *reel* :nil)
	(say (cat (path-name file) ": " (. reel :find :codec) " " (str (. reel :find :width)) "x"
		(str (. reel :find :height)) ", " (str (. reel :find :frames)) " frames, "
		(str (/ (n2f (n2i (* (. reel :find :rate) 100.0))) 100.0)) " a second, no codec for it here yet"))
	(def *window_title* :text (cat "Films -> " (path-name file)))
	(. *window_title* :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun played-load ()
	;the films that have played, the last first, as the user's films.tre has
	;them, where each was left, its scale, the frame it was at, if it was
	;playing and how much later its sound is heard, and the one shown last,
	;which is shown first
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
	(and *file* *reel* (. *places* :insert *file* (list *scale* (max *at* 0) (if *playing* 1 0) *sync*))))

(defun played (file ok)
	;a new film that played is put first of them, one there already keeps
	;its place, and one that did not play is let go
	(defq now (cond
		((not ok) (filter (# (nql %0 file)) *played_list*))
		((find file *played_list*) *played_list*)
		((slice (cat (list file) *played_list*) 0 (min +played_max (inc (length *played_list*)))))))
	(unless (eql (str now) (str *played_list*))
		(setq *played_list* now)
		(played-save)))

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
	;another film's own sync, 0 if it has none kept
	(unless (eql file *file*)
		(defq kept (. *places* :find file))
		(setq *sync* (if (and kept (> (length kept) 3)) (elem-get kept 3) 0)))
	(cond
		;a file, or an object of the store, lib/files/names.inc
		((not (setq reel (ifn reel (reel-open file (name-stream file))))) (played file :nil))
		((not (defq codec (reel-codec reel (ifn scale (fit-scale reel)))))
			(played file :nil)
			(say-what reel file))
		(:t
		(played file :t)
		(played-save)
		;the scroll lets go of the canvas it had as it is given the new one
		(if *codec* (reel-close *codec*))
		(sound-stop *sound*)
		(setq *reel* reel *codec* codec *canvas* (. *codec* :find :canvas) *at* -1
			*file* file *scale* (. codec :find :scale))
		(show (min (ifn frame 0) (dec (. reel :find :frames))))
		;its sound, heard from the frame shown if it is playing
		(setq *sound* (sound-of reel))
		(and *sound* *playing* (sound-start *sound* (frame-time *at*)))
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(def *timeline* :value *at* :maximum (dec (. reel :find :frames)))
		(def *window_title* :text (cat "Films -> " (path-name file)))
		(. *image_scroll* :add_child *canvas*)
		(. *window_title* :layout)
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *window* :change_dirty x y w h))))

(defun open-path (file)
	;a film by the path typed, or why not
	(cond
		((not (find (path-ext file) +file_types))
			(played file :nil)
			(say (cat file ": not a kind of film this plays")))
		((not (name-size file))
			(played file :nil)
			(say (cat file ": not there")))
		((not (reel-open file (name-stream file)))
			(played file :nil)
			(say (cat file ": not a film it can read")))
		((win-refresh file))))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun tick (select)
	;the next frame when it is due, round again after the last
	(defq period (if *reel* (n2i (/ 1000000.0 (. *reel* :find :rate))) 33333) wait :nil start (pii-time))
	(cond
		((not *reel*))
		((sound-on?)
			;the frame of what is heard, the sound fed first; round again after
			;the last, sound and all
			(sound-feed *sound*)
			;the sound heard *sync* ms later than it was coded, the pictures
			;as much ahead of the clock
			(defq ahead (/ (n2f *sync*) 1000.0) n (frame-at (+ (sound-smooth *sound*) ahead)))
			(cond
				((>= n (dec (. *reel* :find :frames)))
					(show 0)
					(sound-start *sound* (frame-time 0)))
				((/= n *at*) (show n)))
			;woken when the next is due by the clock, not a period on: a sleep
			;wakes a little late, and a period on from each would let a frame
			;go now and then
			(when (< (inc *at*) (. *reel* :find :frames))
				(setq wait (max 2000 (min period
					(n2i (* (- (frame-time (inc *at*)) (+ (sound-smooth *sound*) ahead)) 1000000.0)))))))
		(*playing* (show (if (>= (inc *at*) (. *reel* :find :frames)) 0 (inc *at*))))
		((/= *seek* *at*) (show *seek*)))
	;with no sound a period from when this began
	(mail-timeout (elem-get select +select_timer) (ifn wait (max 2000 (- period (- (pii-time) start)))) 0))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(played-load)
	;the window the size the film has made it
	;the film shown last, where it was left, else Raymarch's. Its reel read
	;the once, a film of two hours takes a second
	(bind '(x y w h) (apply view-locate (. (if (and *last* (defq reel (reel-open *last* (name-stream *last*))))
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
			((= idx +select_picker)
				;the film taken, or "" none, its path in the field
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "")
					(.-> *path* (:set_text file) :layout :dirty)
					(open-path file)))
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_open)
				;the picker, at the film shown, its places those films were taken from,
				;the one open told to go first
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get select +select_picker) "Open film..." (ifn *file* ".")
					(join +file_types " ") :nil "films" :t (list (cat *app_root* "data")))))
			((= id +event_path)
				(open-path (trim (get :clear_text *path*))))
			((and *reel* (= id +event_timeline))
				;a drag of it sends many, only the last is shown: the tick
				;shows it if more are waiting now
				(setq *playing* :nil *seek* (get :value *timeline*))
				(sound-stop *sound*)
				(unless (mail-poll select) (show *seek*)))
			((and *reel* (= (getf *msg* +ev_msg_type) +ev_type_action)
					(<= +event_sooner id +event_later))
				;the sound a step sooner or later than the pictures
				(setq *sync* (+ *sync* (if (= id +event_sooner) (neg +sync_step) +sync_step)))
				(show *at*))
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
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(place-keep)
	(played-save)
	(sound-stop *sound*)
	(if *codec* (reel-close *codec*))
	(gui-sub-rpc *window*))
