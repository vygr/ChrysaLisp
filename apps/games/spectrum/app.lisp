(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/scodes.inc")
(import "apps/system/files/picker.inc")
(import "service/audio/app.inc")
(import "service/lock/app.inc")
(import "./machine.inc")

;A Spectrum 48K. The Z80 runs a frame at a time in native code, 50 frames
;a second, and the screen is drawn after each. Tapes load at once by the
;ROM's loader, and snapshots are put straight into memory. The keys of the
;host are the Spectrum's, Shift is CAPS SHIFT, Ctrl or Alt SYMBOL SHIFT,
;the arrows and Tab a Kempston joystick, Escape pauses. The beeper is a
;stream of the audio service, each frame's samples written as it is run,
;and the frames are kept to it. What it was at when closed, the game,
;the zoom, the sound, and the machine itself, all of its memory and its
;Z80, is where it opens again, paused

(import "./widgets.inc")

(enums +select 0
	(enum main timer tip picker))

(defq +zoom_min 1 +zoom_max 3 +rate (/ 1000000 50) +file_types ''(".tap" ".tzx" ".z80" ".sna")
	*zoom* 2 *running* :t *paused* :nil *frame* 0 *file* :nil *tape* :nil *picker* :nil
	*canvas* :nil *sound* :nil *quiet* :nil *held* (list) *script* (list) *key_map* (zx-key-map) *joy_map* (zx-joy-map)
	*machine* (zx-machine (load (cat *app_root* "data/48.rom")))
	+config_version 1 +config_file (cat (get :home *env*) "spectrum.tre")
	+machine_file (cat (get :home *env*) "spectrum.zxm"))

(defun say ()
	;what is shown, in the line at the foot
	(set *status* :text (cat (if *file* (path-stem *file*) "BASIC")
		(if *paused* ", paused" "") (if *quiet* ", quiet" "") ", x" (str *zoom*)))
	(.-> *status* :layout :dirty))

(defun keys ()
	;the matrix of the keys held, and those a script holds
	(zx-keys *machine* (if (nempty? *script*) (cat *held* (second (first *script*))) *held*)
		*key_map* *joy_map*))

(defun frame ()
	;a frame of the Spectrum, the tape loaded as the ROM asks for it
	(when (nempty? *script*)
		(defq step (first *script*))
		(elem-set step 0 (dec (first step)))
		(when (<= (first step) 0) (setq *script* (rest *script*)))
		(keys))
	(while (= (zx-run *machine*) +zx_stop_tape)
		(zx-tape-trap *machine* *tape*))
	(setq *frame* (inc *frame*)))

(defun sound-feed ()
	; (sound-feed) -> frames
	;the frame's samples to the stream, which is opened again if it went,
	;as one not written to for a while is. How many frames to run next, 1,
	;2 if the sound is near its end, 0 if it is well ahead
	(cond
		(*quiet* 1)
		((not (or *sound* (setq *sound* (audio-stream-open-rpc +zx_rate 1)))) 1)
		(:t (audio-stream-write-rpc *sound*
				(slice *machine* (+ +zx_size +zx_pcm) (+ +zx_size +zx_pcm (* +zx_samples +short_size))))
			(defq left (audio-stream-left-rpc *sound*))
			(cond
				((not left) (setq *sound* :nil) 1)
				((< left (* 2 +zx_samples)) 2)
				((> left (* 5 +zx_samples)) 0)
				(1)))))

(defun sound-stop ()
	(when *sound*
		(audio-stream-close-rpc *sound*)
		(setq *sound* :nil)))

(defun draw ()
	;FLASH changes every 16 frames
	(zx-render *canvas* *machine* (if (>= (logand *frame* 31) 16) 0x80 0) *zoom*)
	(. *canvas* :swap +swap_write))

(defun window-fit ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun screen-new ()
	;a canvas the size of the zoom, the old one let go
	(if *canvas* (. *canvas* :sub))
	(setq *canvas* (Canvas (* *zoom* +zx_width) (* *zoom* +zx_height) 1))
	(. *screen* :add_child *canvas*)
	(draw)
	(say))

(defun zoom (by)
	(defq z (max +zoom_min (min +zoom_max (+ *zoom* by))))
	(when (/= z *zoom*)
		(setq *zoom* z)
		(screen-new)
		(window-fit)))

(defun open-file (file)
	;a game, a tape typed in as LOAD "" once the ROM is ready, or a snapshot
	(defq data (load file) ext (path-ext file))
	(cond
		((not data) (set *status* :text (cat file ": can not be read")))
		((find ext '(".tap" ".tzx"))
			(setq *file* file *script* (list)
				*tape* (list (if (eql ext ".tap") (zx-tap-blocks data) (zx-tzx-blocks data)) 0))
			(zx-reset *machine*)
			(setf *machine* +zx_trap 1)
			;the ROM's start, then J for LOAD, "", and ENTER. The ROM takes a
			;key again only once it has been let go of for 5 frames
			(times 100 (frame))
			(each (lambda (codes)
					(push *script* (list 4 codes) (list 8 '())))
				(list (list +sc_j) (list +sc_lctrl +sc_p) (list +sc_lctrl +sc_p) (list +sc_return))))
		((cond
			((eql ext ".z80") (zx-z80 *machine* data))
			((eql ext ".sna") (zx-sna *machine* data)))
			(setq *file* file *tape* :nil *script* (list)))
		((set *status* :text (cat file ": not a snapshot of a 48K"))))
	(keys)
	(say))

(defun state-load ()
	;where it was when closed: the zoom and sound, and the game and the
	;machine as they were, paused. A tape is had again from its file, at
	;the block it was next to read, and what was left of LOAD "" typed
	(when (and (defq config (with-read-lock +config_file (tree-load (file-stream +config_file))))
			(eql (. config :find :version) +config_version))
		(setq *zoom* (max +zoom_min (min +zoom_max (ifn (. config :find :zoom) *zoom*)))
			*quiet* (. config :find :quiet))
		(when (and (defq machine (load +machine_file)) (= (length machine) +zx_machine_size))
			(defq file (. config :find :file) ext (if file (path-ext file)) data (if file (load file)))
			(setq *machine* machine *frame* (ifn (. config :find :frame) 0) *paused* :t
				*script* (ifn (. config :find :script) (list)))
			(cond
				((not data))
				((find ext '(".tap" ".tzx"))
					(setq *file* file *tape* (list (if (eql ext ".tap") (zx-tap-blocks data) (zx-tzx-blocks data))
						(ifn (. config :find :tape_at) 0))))
				((setq *file* file)))
			(keys))))

(defun state-save ()
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write)
			(scatter (Emap) :version +config_version :zoom *zoom* :quiet *quiet*
				:file *file* :tape_at (if *tape* (second *tape*) 0) :frame *frame* :script *script*)))
	(save *machine* +machine_file))

(defun key (scode down)
	; (key scode down) -> :t | :nil
	;a key of the Spectrum or the joystick held or let go
	(when (or (. *key_map* :find scode) (. *joy_map* :find scode))
		(setq *held* (filter (# (/= %0 scode)) *held*))
		(if down (push *held* scode))
		(keys)
		:t))

(defun action-close ()
	(setq *running* :nil))

(defun action-maximise ()
	(zoom 1))

(defun action-minimise ()
	(zoom -1))

(defq *event_map* (scatter (Fmap)
	+event_close action-close
	+event_max action-maximise
	+event_min action-minimise))

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size))
	(def *window* :tip_mbox (elem-get select +select_tip))
	(state-load)
	(screen-new)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_timer)
				;a frame when it is due, from when this one began
				(defq start (pii-time))
				(unless *paused*
					;3 frames at most, catching the sound up
					(defq n 1 most 3)
					(while (and (> n 0) (> most 0))
						(frame)
						(setq n (dec (+ n (sound-feed))) most (dec most)))
					(draw))
				(mail-timeout (elem-get select +select_timer) (max 2000 (- +rate (- (pii-time) start))) 0))
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_picker)
				;the game taken, or "" none
				(setq *picker* :nil)
				(unless (eql (defq file (trim *msg*)) "")
					(open-file file)))
			((and (or (= (defq type (getf *msg* +ev_msg_type)) +ev_type_key_down)
						(= type +ev_type_key_up))
					(defq scode (getf *msg* +ev_msg_key_scode))
					(cond
						((= scode +sc_escape)
							(when (= type +ev_type_key_down)
								(if (setq *paused* (not *paused*)) (sound-stop)))
							(say)
							:t)
						((key scode (= type +ev_type_key_down))))))
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_open)
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get select +select_picker) "Open game..."
					(ifn *file* (cat *app_root* "data/")) (join +file_types " ") :nil "spectrum" :nil
					(list (cat *app_root* "data")))))
			((= id +event_reset)
				;the game again from its start, else BASIC
				(if *file* (open-file *file*)
					(zx-reset *machine*) (say)))
			((= id +event_play) (setq *paused* :nil) (say))
			((= id +event_pause) (setq *paused* :t) (sound-stop) (say))
			((= id +event_quiet) (if (setq *quiet* (not *quiet*)) (sound-stop)) (say))
			((= id +event_smaller) (zoom -1))
			((= id +event_bigger) (zoom 1))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	;a picker still open goes with it
	(if *picker* (mail-send *picker* ""))
	(sound-stop)
	(setq *held* (list))
	(keys)
	(state-save)
	(gui-sub-rpc *window*))
