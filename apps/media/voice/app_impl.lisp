(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "lib/date/date.inc")
(import "lib/audio/voice.inc")
(import "service/voice/app.inc")

;our UI widgets
(import "./widgets.inc")

;Voice: calls, and notes. A call of the mesh is joined, or one started,
;through the voice service of this desktop, service/voice, which has the
;microphone and the speaker: an app only asks it, and watches the call.
;Notes are kept as sound files in the user's own folder, each named for
;when it was made. Record, stop, and the note selected played or deleted.
;In a call, what is recorded is the call, all that is said in it, mixed,
;and all in it are told that it is. Each is a sound pipeline,
;lib/audio/stages.inc: the microphone, or the call's mix, how loud, to a
;file; a file to the speaker. The app only starts them, and tells a source
;when to stop

(enums +select 0
	(enum main tip timer done level call here))

(defq +rate 250000 +folder (cat (get :home *env*) "voice_notes/") +max_notes 200
	;the Join of each call of the mesh, past the notes
	+event_join (+ +event_note 256) +max_calls 64
	*note_files* (list) *selected* :nil
	;what runs: :record or :play, the mailbox its source stops by, and when
	;it started
	*running_pipe* :nil *ctl* :nil *started* 0 *saving* :nil
	;the call as the service says it is, the calls of the mesh, the one
	;picked to join, and the tap a recording of the call is sent by
	*call* :nil *call_list* (list) *tap* :nil *beat* 0 *call_shown* :nil)

(defun note-name ()
	; (note-name) -> str
	;a note's file, named for now, 2026-10-18_10-02-31.wav
	(bind '(s m h dy mo yr &ignore) (date))
	(cat (str yr) "-" (pad (inc mo) 2 "0") "-" (pad dy 2 "0") "_"
		(pad h 2 "0") "-" (pad m 2 "0") "-" (pad s 2 "0") ".wav"))

(defun clock-text (secs)
	(cat (str (/ secs 60)) ":" (pad (% secs 60) 2 "0")))

(defun note-text (file)
	; (note-text file) -> str
	;as the list shows it, 2026-10-18 10:02:31  0:05
	(defq name (path-stem file))
	(cat (slice name 0 10) " " (join (split (slice name 11 -1) "-") ":")
		"  " (clock-text (sound-seconds file))))

(defun status (text)
	(def (. *status* :dirty) :text text)
	(. *status* :layout))

(defun level (value)
	;how loud, green while it runs, a grey trough when it does not
	(def (. *level* :dirty) :value (clamp value 0 100)
		:paper (if *running_pipe* +argb_green +argb_grey12)))

(defun notes-refresh ()
	;the notes, newest first, as buttons, the one selected marked
	(setq *note_files* (sort (files-all +folder '(".wav")) (# (cmp %1 %0))))
	(if (> (length *note_files*) +max_notes) (setq *note_files* (slice *note_files* 0 +max_notes)))
	(unless (find *selected* *note_files*) (setq *selected* :nil))
	(each (# (. %0 :sub)) (. *notes* :children))
	(each (lambda (file)
			(defq btn (Button))
			(def btn :text (note-text file) :border (if (eql file *selected*) 1 0))
			(view-variant btn :font :font_button)
			(if (eql file *selected*) (view-variant btn :paper :paper_highlight))
			(. btn :connect (+ +event_note (!)))
			(. *notes* :add_child btn))
		*note_files*)
	(when (empty? *note_files*)
		(def (defq hint (Label)) :text "No notes yet, press record" :ink +argb_grey8)
		(. *notes* :add_child hint))
	(bind '(w h) (. *notes* :pref_size))
	(bind '(sw &ignore) (. *notes_scroll* :get_size))
	(. *notes* :change_dirty 0 0 (max w (- sw 16) 300) (max h 40) :t)
	(.-> *notes_scroll* :layout :dirty_all))

(defun call-in? () (and *call* (. *call* :find :id)))

(defun call-label (text &optional font ink)
	(def (defq lbl (Label)) :text text)
	(view-variant lbl :font (ifn font :font_button))
	(if ink (def lbl :ink ink))
	lbl)

(defun call-row (left right)
	;a line of the call, who and what they are doing
	(def (defq row (Flow)) :flow_flags +flow_right_fill)
	(def (defq name (call-label left)) :min_width 200)
	(. row :add_child name)
	(. row :add_child (call-label right :font_button +argb_grey8))
	row)

(defun call-buttons (buttons)
	;a tool bar, as (ui-tool-bar) makes one, ((symbol event lit tip) ...),
	;one that is on lit
	(def (defq bar (Flow)) :flow_flags +flow_right)
	(view-variant bar :paper :paper_toolbar)
	(view-variant bar :font :font_symbol)
	(each (lambda ((sym event lit tip))
			(def (defq btn (Button)) :text (num-to-utf8 sym) :tip_text tip)
			(if lit (view-variant btn :paper :paper_highlight))
			(. btn :connect event)
			(. bar :add_child btn))
		buttons)
	bar)

(defun call-area-fit ()
	;the call part its size in its scroll, as the notes are
	(bind '(w h) (. *call_area* :pref_size))
	(bind '(sw &ignore) (. *call_scroll* :get_size))
	(. *call_area* :change_dirty 0 0 (max w (- sw 16) 300) (max h 40) :t)
	(.-> *call_scroll* :layout :dirty_all))

(defun call-show ()
	;the call part made again, if what it shows has changed: the state is
	;sent again each time the call is watched again, as well as on a change.
	;In a call: its name, who is in it and what each is doing, talking,
	;recording, and this one muted, and its buttons. In none: the calls of
	;the mesh, each to join, and one to start
	(defq shown (if (call-in?)
		(str (map (# (. *call* :find %0)) '(:id :name :muted :headphones :members :talking :inbox)))
		(str *call_list*)))
	(unless (eql shown *call_shown*)
		(setq *call_shown* shown)
		(call-show-now)))

(defun call-show-now ()
	(each (# (. %0 :sub)) (. *call_area* :children))
	(cond
		((call-in?)
			(defq talking (ifn (. *call* :find :talking) (list)) me (. *call* :find :inbox)
				muted (. *call* :find :muted))
			(. *call_area* :add_child (call-label (. *call* :find :name) :font_bold))
			(each (lambda ((who inbox rec))
					(defq talks (find inbox talking))
					(. *call_area* :add_child (call-row
						(cat (if talks "* " "  ") who (if (eql inbox me) " (you)" ""))
						(cond
							((= rec 1) "recording")
							(talks "talking")
							((and (eql inbox me) muted) "muted")
							("")))))
				(ifn (. *call* :find :members) (list)))
			(. *call_area* :add_child (call-buttons (list
				(list +sym_mute +event_mute muted "mute")
				(list +sym_headphones +event_headphones (. *call* :find :headphones) "headphones")
				(list +sym_hangup +event_leave :nil "leave")))))
		(:t (each (lambda ((id name))
					(def (defq row (Flow)) :flow_flags +flow_right_fill)
					(def (defq lbl (call-label name)) :min_width 240)
					(. row :add_child lbl)
					(. row :add_child (call-buttons (list (list +sym_dial (+ +event_join (!)) :nil "join"))))
					(. *call_area* :add_child row))
				*call_list*)
			(if (empty? *call_list*) (. *call_area* :add_child (call-label "No calls to join" :font_button +argb_grey8)))
			(. *call_area* :add_child (call-buttons (list (list +sym_dial +event_start :nil "call"))))))
	(call-area-fit))

(defun calls-refresh ()
	;the calls of the mesh, and the call part made again if they changed
	(defq was (str *call_list*))
	(setq *call_list* (call-list))
	(if (> (length *call_list*) +max_calls) (setq *call_list* (slice *call_list* 0 +max_calls)))
	(unless (or (call-in?) (eql was (str *call_list*))) (call-show)))

(defun pipe-stop ()
	;what runs is told to stop, and says when it is done
	(if *ctl* (mail-send *ctl* "")))

(defun record-start (select)
	;the microphone, or in a call the call, all of it mixed, and all in it
	;told that it is recorded
	(setq *saving* (cat +folder (note-name)) *ctl* (mail-mbox)
		*running_pipe* :record *started* (pii-time))
	(defq source (cond
		((call-in?)
			(setq *tap* (mail-mbox))
			(call-tap *tap*)
			(call-record :t)
			(voice-stage 'voice-mix (stage-mbox *tap*) (. *call* :find :inbox) (stage-mbox *ctl*)))
		((sound-stage 'sound-mic (stage-mbox *ctl*)))))
	(stages-run (list source
			(sound-stage 'sound-level (stage-mbox (elem-get select +select_level)))
			(sound-stage 'sound-to-file *saving*))
		(elem-get select +select_done))
	(status (if *tap* "Recording the call 0:00" "Recording 0:00")))

(defun play-start (select)
	(cond
		((not *selected*) (status "Select a note to play"))
		(:t (setq *ctl* (mail-mbox) *running_pipe* :play *started* (pii-time))
			(stages-run (list
					(sound-stage 'sound-from-file *selected* (stage-mbox *ctl*))
					(sound-stage 'sound-level (stage-mbox (elem-get select +select_level)))
					(sound-stage 'sound-speaker))
				(elem-get select +select_done))
			(status (cat "Playing " (note-text *selected*))))))

(defun pipe-done ()
	;the sink has finished, a note saved if it was recording
	(defq was *running_pipe*)
	(setq *running_pipe* :nil *ctl* :nil)
	(when *tap*
		(call-tap :nil)
		(call-record :nil)
		(setq *tap* :nil))
	(level 0)
	(cond
		((eql was :record)
			(cond
				((> (sound-seconds *saving*) 0)
					(setq *selected* *saving*)
					(status (cat "Saved " (note-text *saving*))))
				(:t (pii-remove *saving*)
					(status "Nothing was heard, is the microphone allowed ?")))
			(setq *saving* :nil)
			(notes-refresh))
		((status "Ready"))))

(defun note-delete ()
	(when *selected*
		(pii-remove *selected*)
		(setq *selected* :nil)
		(status "Deleted")
		(notes-refresh)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t
		;said, so a second is not opened on this desktop, app.lisp
		;said, so a second is not opened on this desktop, app.lisp: by a
		;mailbox that answers, so one that died without going is known
		entry (mail-declare (elem-get select +select_here) "Voice" "Voice App"))
	(def *window* :tip_mbox (elem-get select +select_tip))
	;the folder made, if it is not there, by saving into it
	(unless (pii-fstat +folder)
		(save "" (cat +folder ".keep")) (pii-remove (cat +folder ".keep")))
	(notes-refresh)
	(calls-refresh)
	(call-watch (elem-get select +select_call))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(if (eql *running_pipe* :record)
					(status (cat (if *tap* "Recording the call " "Recording ") (clock-text (/ (- (pii-time) *started*) 1000000)))))
				;the call watched again within its lease, and the calls of the
				;mesh looked at again
				(when (> (- (pii-time) *beat*) +call_beat)
					(setq *beat* (pii-time))
					(call-watch (elem-get select +select_call))
					(calls-refresh)))
			((= idx +select_call)
				;the call as it is now: a recording of one that is left, or
				;that ended, stops
				(when (defq state (call-state-read *msg*))
					(setq *call* state)
					(and *tap* (not (call-in?)) (pipe-stop))
					(call-show)))
			((= idx +select_level)
				(if *running_pipe* (level (/ (* (str-as-num *msg*) 100) 16384))))
			((= idx +select_here)
				;another is being opened: this one is here, it says so
				(mail-send (hex-decode *msg*) ""))
			((= idx +select_done)
				(pipe-done))
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq *running* :nil))
			((= id +event_start) (call-start (cat (ifn (get :user *env*) "Guest") "'s call")))
			((= id +event_leave) (call-leave))
			((= id +event_mute) (if (call-in?) (call-mute (not (. *call* :find :muted)))))
			((= id +event_headphones) (if (call-in?) (call-headphones (not (. *call* :find :headphones)))))
			((<= +event_join id (+ +event_join +max_calls -1))
				(if (< (defq i (- id +event_join)) (length *call_list*))
					(call-join (first (elem-get *call_list* i)))))
			((= id +event_record) (unless *running_pipe* (record-start select)))
			((= id +event_stop) (pipe-stop))
			((= id +event_play) (unless *running_pipe* (play-start select)))
			((= id +event_delete) (unless *running_pipe* (note-delete)))
			((<= +event_note id (+ +event_note +max_notes -1))
				(when (and (not *running_pipe*) (< (defq i (- id +event_note)) (length *note_files*)))
					(setq *selected* (elem-get *note_files* i))
					(notes-refresh)))
			((. *window* :event *msg*))))
	;what runs is stopped, and waited for a moment, so a note is saved
	(when *running_pipe*
		(pipe-stop)
		(mail-read-timeout (elem-get select +select_done) (task-timeout 3))
		(if *tap* (pipe-done)))
	;the call goes on, as a phone call does when its app is put away: the
	;service has it, and another app, or this one opened again, shows it
	(call-unwatch (elem-get select +select_call))
	(mail-forget entry)
	(gui-sub-rpc *window*))
