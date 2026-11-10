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

;what it plays is every kind of reel its imports made, lib/media/reel.inc
(defq *file_types* (map (const first) *reel_kinds*) *canvas* :nil *reel* :nil *codec* :nil *at* -1 *seek* 0 *playing* :t
	*file* :nil *picker* :nil *scale* :nil *played_list* (list) *places* (Fmap 11) *last* :nil *sound* :nil *sync* 0 +sync_step 40
	+played_max 12 +played_version 1 +played_file (cat (get :home *env*) "films.tre")
	*counter* :t *ending* 1 *options* :nil *status_shown* :t)

(defun frame-time (n)
	; (frame-time n) -> seconds
	;when frame n is shown, of a film of an mp4, its time less where its
	;edit list starts it, else by the rate, frames of an MP3 are evenly
	;spaced
	(cond
		;a reel that knows its frames' times itself, one read as it is played
		((defq time_of (. *reel* :find :frame_time)) (time_of *reel* n))
		((defq video (. *reel* :find :video))
			(/ (n2f (- (elem-get (. *reel* :find :shown) (elem-get (. *codec* :find :order) n)) (. *reel* :find :start)))
				(n2f (. video :find :timescale))))
		((/ (n2f n) (. *reel* :find :rate)))))

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

(defun sound-on? ()
	(and *sound* *playing* (. *sound* :find :handle)))

(defun frame-draw (n)
	;frame n on the canvas, the codec reads the packets it needs for it. Of
	;an MP3 being heard, the samples its sound decoded, not decoded again:
	;the codec's own decoder is then behind, and starts again from before
	;the next it is asked for
	(ifn (and (sound-on?) (defq pcm (sound-kept *sound* n)))
		(reel-frame *codec* n)
		(reel-wave *canvas* (. *reel* :find :width) (. *reel* :find :height) pcm)
		(. *codec* :insert :at -2)))

(defun status-say (text)
	;what the line at the foot says, the line taken away when it says
	;nothing, so the film has the window to itself. Its text is set only
	;while it is in the window, it takes its colours from there
	(cond
		((eql text "")
			(when *status_shown*
				(setq *status_shown* :nil)
				(. *status* :sub)
				(. *window* :fit)))
		(*status_shown* (. *status* :set_text text))
		(:t (setq *status_shown* :t)
			(. *status_box* :add_child *status*)
			(. *status* :set_text text)
			(. *window* :fit))))

(defun options-open (on)
	;the options under the buttons, or not
	(setq *options* on)
	(if on (. *options_box* :add_child *options_row*) (. *options_row* :sub))
	(. *window* :fit))

(defun show (n)
	;frame n on the canvas
	(setq *at* n *seek* n)
	(frame-draw n)
	(. *canvas* :swap +swap_write)
	(def *timeline* :value *at*)
	(. *timeline* :dirty)
	(status-say (if *counter* (cat (if (. *reel* :find :audio)
					(cat (reel-clock (frame-time *at*)) " of " (reel-clock (frame-time (. *reel* :find :frames))))
					(cat (str (inc *at*)) " of " (str (. *reel* :find :frames))
						", " (str (n2i (. *reel* :find :rate))) " a second"))
				(if (> (. *codec* :find :scale) 1) (cat ", 1/" (str (. *codec* :find :scale)) " size") "")
				(cond ((> *sync* 0) (cat ", sound +" (str *sync*) "ms")) ((< *sync* 0) (cat ", sound " (str *sync*) "ms")) (""))
				(if *playing* "" ", paused")) "")))

(defun play (on)
	;the sound started from the frame shown, or stopped. A film stopped at
	;its end plays from its start
	(setq *playing* on)
	(and on *reel* (>= (inc *at*) (. *reel* :find :frames)) (setq *at* 0))
	(cond
		((and on *sound*) (sound-start *sound* (frame-time *at*)))
		(*sound* (sound-stop *sound*)))
	(if *reel* (show *at*)))

(defun say-what (reel file)
	;a film there is no codec for yet: what it is, and no picture
	(setq *reel* :nil)
	(status-say (cat (path-name file) ": " (. reel :find :codec) " " (str (. reel :find :width)) "x"
			(str (. reel :find :height)) ", " (str (. reel :find :frames)) " frames, "
			(str (/ (n2f (n2i (* (. reel :find :rate) 100.0))) 100.0)) " a second, no codec for it here yet"))
	(. *window_title* :set_text (cat "Films -> " (path-name file)))
	(. *window* :fit))

(defun played-load ()
	;the films that have played, the last first, as the user's films.tre has
	;them, where each was left, its scale, the frame it was at, if it was
	;playing and how much later its sound is heard, and the one shown last,
	;which is shown first
	(defq tree (with-read-lock +played_file (tree-load (file-stream +played_file))))
	(when (and tree (eql (. tree :find :version) +played_version))
		(setq *played_list* (ifn (. tree :find :films) (list)) *last* (. tree :find :last)
			*counter* (/= 0 (ifn (. tree :find :counter) 1))
			*ending* (max 0 (min 3 (ifn (. tree :find :ending) 1))))
		(each (lambda ((file &rest place)) (. *places* :insert file place))
			(ifn (. tree :find :places) (list)))))

(defun played-save ()
	;the places of those in the list only
	(with-write-lock +played_file
		(tree-save (file-stream +played_file +file_open_write)
			(scatter (Emap) :version +played_version :films *played_list* :last *file*
				:counter (if *counter* 1 0) :ending *ending*
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
			(def *timeline* :value *at* :maximum (dec (. reel :find :frames)))
			(. *window_title* :set_text (cat "Films -> " (path-name file)))
			(. *image_scroll* :add_child *canvas*)
			(. *window* :fit))))

(defun zoom (scale)
	;the film at another scale, from the frame it is at. A codec that can
	;keeps its decoder, and is given a canvas of the scale: nothing is
	;decoded again and the sound goes on. One that can not is made again,
	;from the key before the frame, which may be seconds of decoding
	(cond
		((reel-rescale *codec* scale)
			(setq *canvas* (. *codec* :find :canvas) *scale* scale)
			(show *at*)
			;the scroll lets go of the canvas it had as it is given the new one
			(. *image_scroll* :add_child *canvas*)
			(. *window* :fit))
		((win-refresh *file* scale *at* *reel*))))

(defun open-path (file)
	;a film by the path typed, or why not
	(cond
		((not (find (path-ext file) *file_types*))
			(played file :nil)
			(status-say (cat file ": not a kind of film this plays")))
		((not (name-size file))
			(played file :nil)
			(status-say (cat file ": not there")))
		((not (reel-open file (name-stream file) :t))
			(played file :nil)
			(status-say (cat file ": not a film it can read")))
		((win-refresh file))))

(defun films-beside (file)
	; (films-beside file) -> (file ...)
	;the films of the folder a film is in, it among them, in the order of
	;their names, of the host's folders or the store's
	(defq dir (path-dir file) store (name-in-store file)
		folder (cond ((nempty? dir) dir) (store +name_store) ("."))
		start (cond ((nempty? dir) (cat dir "/")) (store +name_store) (""))
		listing (name-list folder) films (list))
	(each (lambda (name kind)
			(and (find kind '("f" "l")) (find (path-ext name) *file_types*)
				(push films (cat start name))))
		(ifn (first listing) (list)) (ifn (second listing) (list)))
	(sort films))

(defun film-next (any)
	; (film-next any) -> :nil | :t
	;the film after the one shown in its folder, round to the first after
	;the last, or another of them at random, played from its start at the
	;scale of this one. Those that will not play are passed over, :nil if
	;none will, or there is no other
	(defq films (films-beside *file*) at (find *file* films)
		next (cond
			(any (shuffle (filter (# (nql %0 *file*)) films)))
			(at (cat (slice films (inc at) -1) (slice films 0 at)))
			(films)))
	(some (lambda (file)
			(setq *playing* :t)
			(win-refresh file *scale* 0)
			(when (and *reel* (eql *file* file))
				(. *path* :set_text file)
				:t))
		next))

(defun film-stop ()
	;stopped on its last frame
	(if *reel* (setq *at* (dec (. *reel* :find :frames))))
	(play :nil))

(defun film-ended ()
	;what is done at the end of a film, as the option says: stopped on its
	;last frame, played again from its start, or the next film played
	(cond
		((= *ending* 1) (show 0) (if (sound-on?) (sound-start *sound* (frame-time 0))))
		((and (>= *ending* 2) (film-next (= *ending* 3))))
		((film-stop))))

;import actions and bindings
(import "./actions.inc")

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
				;the last frame, or the end of the sound, which may come a frame
				;or two before it
				((or (>= n (dec (. *reel* :find :frames))) (sound-done? *sound*))
					(film-ended))
				((/= n *at*) (show n)))
			;woken when the next is due by the clock, not a period on: a sleep
			;wakes a little late, and a period on from each would let a frame
			;go now and then
			;the film may have stopped, or be another
			(and *reel* (sound-on?) (< (inc *at*) (. *reel* :find :frames))
				(setq wait (max 2000 (min period
							(n2i (* (- (frame-time (inc *at*)) (+ (sound-smooth *sound*) ahead)) 1000000.0)))))))
		(*playing* (cond
				((< (inc *at*) (. *reel* :find :frames)) (show (inc *at*)))
				((film-ended))))
		((/= *seek* *at*) (show *seek*)))
	;with no sound a period from when this began
	(mail-timeout (elem-get select +select_timer) (ifn wait (max 2000 (- period (- (pii-time) start)))) 0))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(played-load)
	;the options as they were left, shut
	(. *choices* :set_states (list *counter*))
	(. *endings* :set_selected *ending*)
	(. *options_row* :sub)
	;the window the size the film has made it
	;the film shown last, where it was left, else Raymarch's. Its reel read
	;the once, a film of two hours takes a second
	(if (and *last* (defq reel (reel-open *last* (name-stream *last*))))
		(win-refresh *last* :nil :nil reel)
		(win-refresh (cat *app_root* "data/raymarch.flm")))
	(. *window* :open)
	(tick select)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(. *window* :tip *msg*))
			((= idx +select_timer) (tick select))
			((= idx +select_picker)
				;the film taken, or "" none, its path in the field
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "")
					(. *path* :set_text file)
					(open-path file)))
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_open)
				;the picker, at the film shown, its places those films were taken from,
				;the one open told to go first
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get select +select_picker) "Open film..." (ifn *file* ".")
						(join *file_types* " ") :nil "films" :t (list (cat *app_root* "data")))))
			((= id +event_options)
				(options-open (not *options*)))
			((= id +event_choices)
				;the frame counter turned on or off, kept for the next time
				(setq *counter* (first (. *choices* :get_states)))
				(played-save)
				(if *reel* (show *at*)))
			((= id +event_endings)
				;what is done when a film ends, kept for the next time
				(setq *ending* (ifn (. *endings* :get_selected) 1))
				(played-save))
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
				(if (/= scale *scale*) (zoom scale)))
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
	(. *window* :close))
