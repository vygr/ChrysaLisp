(defq *app_root* (path-to-file))
(import "service/audio/app.inc")
(import "lib/media/module.inc")
(import "lib/media/tracker.inc")
(import "lib/media/spectrum.inc")
(import "lib/task/jobs.inc")
(import "./app.inc")

;The player of the Jukebox, a task of its own on a node not the window's,
;so a heavy song takes none of the time of what draws the screen. It plays
;a song a tick at a time into a stream of the Audio service, a little
;ahead of what is heard, and tells the window what is heard.
;
;Told by the window, each message a mailbox, then a letter. The mailbox a
;song came with is that song's name from then on, and where it is told of:
;what is said of a song with another's is not heard, and what this says
;of a song no longer wanted goes to a mailbox let go of, nowhere:
;	M state seconds [q] \n bytes	a module, played, held or stopped there;
;				q, quiet, for an app with no window: told only E and !
;	P			played, from its start if stopped
;	H			held, or played on if held
;	S			stopped, to play from its start
;	G mailbox seconds	played from there, and told of on that mailbox
;				from then, so what was said before the seek is not
;				taken for after it
;	K			the window is there
;	Q			go
;Its child, ./measure.lisp, a farm of one, lib/task/jobs.inc, finds the
;song's length and where its orders start, a job for each song.
;It tells the window:
;	I frames		the song is so long
;	!			not a module it can read
;	T heard order row state samples... | bands...
;	E			all of the song heard
;a quiet song is told only ! and E, so neither its places nor its spectrum
;are worked out, and an app that shows none of it has no mail 30 times a
;second
;and it goes if it hears nothing of the window for a while, ./app.inc

(enums +select 0
	(enum main timer measure_task measure_reply))

(defq +ahead 11025 +frame_time (/ 1000000 30) +fft 1024
	;how long the measurer may take over a song before it is taken to have
	;gone, to start with, lib/task/jobs.inc
	+measure_patience 30000000
	+recent_max (+ +ahead +fft 4096)
	;the mailboxes it reads, the song's name, the mailbox of the window it
	;is told of on, the song and its player
	*select* :nil *song_id* :nil *app* :nil *song* :nil *player* :nil *handle* :nil *written* 0
	*marks* (list) *recent* "" *recent_at* 0 *state* "s" *ended* :nil *quiet* :nil
	*spectrum* (spectrum-make +fft +rate +bars) *heard* 0 *told* :nil
	;the farm of one that measures the song, while it does
	*measurer* :nil
	;the last seek asked for, (mailbox seconds), done when no more is waiting
	*seek_to* :nil
	;the song's length once its child has found it, and where its orders
	;start, (tracker-start) each, which a seek goes from
	*length* :nil *starts* (list))

(defun sound-stop ()
	(when *handle*
		(audio-stream-close-rpc *handle*)
		(setq *handle* :nil)))

(defun rewind ()
	;the song from its start, nothing written yet
	(sound-stop)
	(setq *player* (tracker-player *song* +rate) *written* 0 *marks* (list)
		*recent* "" *recent_at* 0 *ended* :nil *heard* 0))

(defun song-ended ()
	; (song-ended) -> :t | :nil
	;at its end: a player knows it by a row come to again, or once it has
	;played as long as the song is, as one from where an order starts must
	(or (. *player* :find :ended) (and *length* (>= *written* *length*))))

(defun seek (seconds)
	;from so many seconds in, its rows played to there and its voices moved
	;on, nothing mixed: on from where it has got to if no order starts
	;between, else from where the last order before it starts, as libxmp
	;seeks, (tracker-at), or from its start if those are not known yet
	(defq target (* seconds +rate) start :nil)
	;none known till its child has found them, a song resumed seeks first
	(each (# (if (<= (first %0) target) (setq start %0))) *starts*)
	(cond
		((and (>= target *written*) (not *ended*) (or (not start) (<= (first start) *written*)))
			(sound-stop)
			(setq *written* (tracker-seek *player* target *written*)))
		(start (rewind)
			(setq *player* (tracker-at *song* +rate start)
				*written* (tracker-seek *player* target (first start))))
		(:t (rewind)
			(setq *written* (tracker-seek *player* target))))
	(setq *marks* (list) *recent* "" *recent_at* *written* *heard* *written*
		*ended* (song-ended)))

(defun feed ()
	; (feed) -> heard
	;ticks of the song written till the stream is far enough ahead of what is
	;heard, and each tick's place kept, to say it as it is heard. A stream
	;not written to for a while, a pause of 5 seconds, is closed by the
	;Audio service: it is opened again, nothing of it still to be heard,
	;where written to one that is gone the song ran on unheard
	(defq left (and *handle* (audio-stream-left-rpc *handle*)))
	(unless left (setq *handle* (audio-stream-open-rpc +rate 2) left 0))
	(cond
		(*handle*
			(when (eql *state* "p")
				(while (and (< left +ahead) (not *ended*))
					(unless *quiet* (push *marks* (list *written* (. *player* :find :order) (. *player* :find :row))))
					(defq pcm (tracker-next *player*) n (/ (length pcm) 4))
					(audio-stream-write-rpc *handle* pcm)
					(setq *written* (+ *written* n) left (+ left n) *ended* (song-ended))
					;what was heard last, for the spectrum, not for a quiet song
					(unless *quiet*
						(setq *recent* (cat *recent* pcm))
						(when (> (defq over (- (/ (length *recent*) 4) +recent_max)) 0)
							(setq *recent* (slice *recent* (* 4 over) -1) *recent_at* (+ *recent_at* over))))
					(task-slice)))
			(setq *heard* (- *written* left)))
		(*heard*)))

(defun report ()
	;what is heard told to the window: where, how, which samples sound and
	;the bars of its spectrum; none of it of a quiet song
	(unless *quiet*
		(defq heard *heard*)
		(while (and (> (length *marks*) 1) (<= (first (second *marks*)) heard))
			(setq *marks* (slice *marks* 1 -1)))
		(bind '(& order row) (ifn (first *marks*) (list 0 (. *player* :find :order) (. *player* :find :row))))
		(defq playing (tracker-playing *player*)
			from (- heard +fft *recent_at*)
			bands (if (and (eql *state* "p") (>= from 0) (<= (+ from +fft) (/ (length *recent*) 4)))
				(map (const n2i) (spectrum-bands *spectrum* *recent* from))
				(map! (lambda () 0) '() 0 +bars)))
		(mail-send *app* (cat "T" (join (map (const str) (list heard order row)) " ")
			" " *state* " " (join (map (const str) playing) " ") " | " (join (map (const str) bands) " ")))))

(defun measure-end ()
	;the farm that measures let go of: its child is told to go, and
	;nothing late of it comes in, lib/task/jobs.inc
	(setq *measurer* (jobs-end *measurer* *select* +select_measure_task +select_measure_reply)))

(defun measure (data)
	;how long the song is found by a child, a farm of one where the kernel
	;puts it, as a long song takes seconds: the song is its job. The farm of
	;the song before is let go of. A child that goes, or takes too long, is
	;started again by the farm, (:refresh)
	(measure-end)
	(setq *measurer* (Jobs (cat *app_root* "measure.lisp")
		(elem-get *select* +select_measure_task) (elem-get *select* +select_measure_reply) (list 1)))
	(. *measurer* :add (list (cat (pad "" +job_size "\x00") data))))

(defun measured (msg)
	;what the measurer found: the song's length, told the window, and where
	;its orders start, which a seek goes from. Its farm has no more to do
	(when (and *measurer* (. *measurer* :answered msg))
		(defq vals (map (const str-as-num) (split (slice msg +job_reply_size -1) " ")))
		(setq *length* (first vals) *starts* (partition (rest vals) 7))
		(unless *quiet* (mail-send *app* (cat "I" (str *length*))))
		(measure-end)))

(defun load-song (id state seconds quiet data)
	;a module played, held or stopped at so many seconds, at once, how long
	;it is found as it plays. The mailbox it came with is its name, and
	;where the window is told of it, of a quiet song only its end. A seek
	;asked of the song before is not this one's
	(sound-stop)
	(setq *song_id* id *app* id *song* (module-read data) *told* :nil *length* :nil *starts* (list)
		*seek_to* :nil *quiet* quiet)
	(cond
		(*song*
			(measure data)
			(rewind)
			(setq *state* state)
			(if (> seconds 0) (seek seconds))
			(unless (eql state "s") (setq *handle* (audio-stream-open-rpc +rate 2)))
			(report))
		(:t (setq *state* "s")
			(mail-send *app* "!"))))

(defun command (msg)
	; (command msg) -> :nil if to go
	;a mailbox, a letter, and what the letter is of
	(defq id (slice msg 0 +net_id_size) at (inc +net_id_size)
		kind (if (> (length msg) +net_id_size) (elem-get msg +net_id_size) "K"))
	(cond
		((eql kind "Q") :nil)
		((eql kind "K"))
		((eql kind "M")
			;its line ends after the mailbox, which is bytes, a newline
			;among them now and then
			(defq end (ifn (find "\n" msg at) (length msg)) words (split (slice msg at end) " "))
			(load-song id (first words) (str-as-num (second words)) (eql (third words) "q")
				(slice msg (min (length msg) (inc end)) -1)))
		;of a song that is not the one played, not heard
		((or (not *song*) (nql id *song_id*)))
		((eql kind "P")
			(cond
				((eql *state* "s") (rewind) (setq *state* "p" *handle* (audio-stream-open-rpc +rate 2)))
				((setq *state* "p")))
			(report))
		((eql kind "H")
			(if (find *state* '("p" "h")) (setq *state* (if (eql *state* "p") "h" "p")))
			(report))
		((eql kind "S")
			(rewind)
			(setq *state* "s" *told* :nil)
			(report))
		((eql kind "G")
			;only the last of a drag is done, (seek-now)
			(setq *seek_to* (list (slice msg at (+ at +net_id_size)) (str-as-num (slice msg (+ at +net_id_size) -1)))))))

(defun seek-now ()
	;the last seek asked for, now no more is waiting: the window is told of
	;the song on the seek's mailbox from now, its length again with it
	(bind '(reply seconds) *seek_to*)
	(defq state *state*)
	(setq *seek_to* :nil *app* reply)
	(seek seconds)
	(setq *state* (if (eql state "s") "p" state) *told* :nil
		*handle* (audio-stream-open-rpc +rate 2))
	(and *length* (not *quiet*) (mail-send *app* (cat "I" (str *length*))))
	(report))

(defun main ()
	(setq *select* (task-mboxes +select_size))
	(defq running :t heard_at (pii-time) refreshed (pii-time))
	(mail-timeout (elem-get *select* +select_timer) +frame_time 0)
	(while running
		(defq idx (mail-select *select*) msg (mail-read (elem-get *select* idx)))
		(cond
			((= idx +select_timer)
				(mail-timeout (elem-get *select* +select_timer) +frame_time 0)
				;the window gone, so is the player
				(if (> (- (pii-time) heard_at) +orphan) (setq running :nil))
				;the measurer's patience, each second
				(when (and *measurer* (> (- (pii-time) refreshed) 1000000))
					(setq refreshed (pii-time))
					(. *measurer* :refresh +measure_patience))
				(when (and *song* (eql *state* "p"))
					(feed)
					(report)
					;all of it heard
					(when (and *ended* (>= *heard* *written*) (not *told*))
						(setq *told* :t)
						(mail-send *app* "E"))))
			((= idx +select_measure_task) (if *measurer* (. *measurer* :launched msg)))
			((= idx +select_measure_reply) (measured msg))
			(:t (setq heard_at (pii-time) running (command msg))))
		;a seek when no more is waiting
		(and *seek_to* *song* (not (mail-waiting? (elem-get *select* +select_main)))
			(seek-now)))
	(measure-end)
	(sound-stop))
