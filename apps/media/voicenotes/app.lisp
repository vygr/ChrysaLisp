(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")
(import "lib/date/date.inc")
(import "lib/audio/stages.inc")

;our UI widgets
(import "./widgets.inc")

;a voice recorder, notes kept as sound files in the user's own folder, each
;named for when it was made. Record, stop, and the note selected played or
;deleted. Each is a sound pipeline, lib/audio/stages.inc: the microphone,
;how loud, to a file; a file to the speaker. The app only starts them, and
;tells a source when to stop

(enums +select 0
	(enum main tip timer done level))

(defq +rate 250000 +folder (cat (get :home *env*) "voice_notes/") +max_notes 200
	*note_files* (list) *selected* :nil
	;what runs: :record or :play, the mailbox its source stops by, and when
	;it started
	*running_pipe* :nil *ctl* :nil *started* 0 *saving* :nil)

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
	(defq name (slice file (ifn (rfind "/" file) 0) -5))
	(cat (slice name 0 10) " " (join (split (slice name 11 -1) "-") ":")
		"  " (clock-text (sound-seconds file))))

(defun status (text)
	(def (. *status* :dirty) :text text)
	(. *status* :layout))

(defun level (value)
	(def (. *level* :dirty) :value (max 0 (min 100 value))))

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

(defun pipe-stop ()
	;what runs is told to stop, and says when it is done
	(if *ctl* (mail-send *ctl* "")))

(defun record-start (select)
	(setq *saving* (cat +folder (note-name)) *ctl* (mail-mbox)
		*running_pipe* :record *started* (pii-time))
	(stages-run (list
			(sound-stage 'sound-mic (stage-mbox *ctl*))
			(sound-stage 'sound-level (stage-mbox (elem-get select +select_level)))
			(sound-stage 'sound-to-file *saving*))
		(elem-get select +select_done))
	(status "Recording 0:00"))

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
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	;the folder made, if it is not there, by saving into it
	(unless (pii-fstat +folder)
		(save "" (cat +folder ".keep")) (pii-remove (cat +folder ".keep")))
	(notes-refresh)
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
					(status (cat "Recording " (clock-text (/ (- (pii-time) *started*) 1000000))))))
			((= idx +select_level)
				(if *running_pipe* (level (/ (* (str-as-num *msg*) 100) 16384))))
			((= idx +select_done)
				(pipe-done))
			((= (defq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq *running* :nil))
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
		(mail-read-timeout (elem-get select +select_done) (task-timeout 3)))
	(gui-sub-rpc *window*))
