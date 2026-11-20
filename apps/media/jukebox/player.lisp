(import "service/audio/app.inc")
(import "lib/media/module.inc")
(import "lib/media/tracker.inc")
(import "lib/media/spectrum.inc")

;The player of the Jukebox, a task of its own on a node not the window's,
;so a heavy song takes none of the time of what draws the screen. It plays
;a song a tick at a time into a stream of the Audio service, a little
;ahead of what is heard, and tells the window what is heard.
;
;Told by the window, each with the number of the song it is of, as mail
;is unordered and what is of another song is not heard:
;	M serial state seconds \n bytes	a module, played, held or stopped there
;	P serial		played, from its start if stopped
;	H serial		held, or played on if held
;	S serial		stopped, to play from its start
;	G serial seconds	played from there
;	K			the window is there
;	Q			go
;It tells the window:
;	I serial frames		the song is so long
;	! serial		not a module it can read
;	T serial heard order row state samples... | bands...
;	E serial		all of the song heard
;and it goes if it hears nothing of the window for a while

(enums +select 0
	(enum main timer))

(defq +rate 44100 +ahead 11025 +frame_time (/ 1000000 30) +fft 1024 +bars 32
	+recent_max (+ +ahead +fft 4096) +orphan 5000000
	*app* :nil *serial* -1 *song* :nil *player* :nil *handle* :nil *written* 0
	*marks* (list) *recent* "" *recent_at* 0 *length* 0 *state* "s" *ended* :nil
	*spectrum* (spectrum-make +fft +rate +bars) *heard* 0 *told* :nil)

(defun sound-stop ()
	(when *handle*
		(audio-stream-close-rpc *handle*)
		(setq *handle* :nil)))

(defun rewind ()
	;the song from its start, nothing written yet
	(sound-stop)
	(setq *player* (tracker-player *song* +rate) *written* 0 *marks* (list)
		*recent* "" *recent_at* 0 *ended* :nil *heard* 0))

(defun seek (seconds)
	;from so many seconds in, its rows played to there and its voices moved
	;on, nothing mixed
	(rewind)
	(setq *written* (tracker-seek *player* (* seconds +rate)) *recent_at* *written*
		*heard* *written* *ended* (. *player* :find :ended)))

(defun feed ()
	; (feed) -> heard
	;ticks of the song written till the stream is far enough ahead of what is
	;heard, and each tick's place kept, to say it as it is heard
	(cond
		(*handle*
			(defq left (ifn (audio-stream-left-rpc *handle*) 0))
			(when (eql *state* "p")
				(while (and (< left +ahead) (not *ended*))
					(push *marks* (list *written* (. *player* :find :order) (. *player* :find :row)))
					(defq pcm (tracker-next *player*) n (/ (length pcm) 4))
					(audio-stream-write-rpc *handle* pcm)
					(setq *recent* (cat *recent* pcm) *written* (+ *written* n) left (+ left n)
						*ended* (. *player* :find :ended))
					(when (> (defq over (- (/ (length *recent*) 4) +recent_max)) 0)
						(setq *recent* (slice *recent* (* 4 over) -1) *recent_at* (+ *recent_at* over)))
					(task-slice)))
			(setq *heard* (- *written* left)))
		(*heard*)))

(defun report ()
	;what is heard told to the window: where, how, which samples sound and
	;the bars of its spectrum
	(defq heard *heard*)
	(while (and (> (length *marks*) 1) (<= (first (second *marks*)) heard))
		(setq *marks* (slice *marks* 1 -1)))
	(bind '(& order row) (ifn (first *marks*) (list 0 (. *player* :find :order) (. *player* :find :row))))
	(defq voices (. *player* :find :voices) playing (list)
		from (- heard +fft *recent_at*)
		bands (if (and (eql *state* "p") (>= from 0) (<= (+ from +fft) (/ (length *recent*) 4)))
			(map (# (n2i %0)) (spectrum-bands *spectrum* *recent* from))
			(map! (lambda () 0) '() 0 +bars)))
	(each! (lambda ()
			(defq s (elem-get voices (* (!) +mix_size)))
			(and (>= s 0) (> (elem-get voices (+ (* (!) +mix_size) 3)) 0) (not (find s playing))
				(push playing s)))
		'() 0 (/ (length voices) +mix_size))
	(mail-send *app* (cat "T" (join (map (const str) (cat (list *serial* heard order row) )) " ")
		" " *state* " " (join (map (const str) playing) " ") " | " (join (map (const str) bands) " "))))

(defun load-song (serial state seconds data)
	;a module played, held or stopped at so many seconds, and how long it is
	(sound-stop)
	(setq *serial* serial *song* (module-read data) *told* :nil)
	(cond
		(*song*
			(setq *length* (tracker-length *song* +rate))
			(mail-send *app* (cat "I" (str serial) " " (str *length*)))
			(rewind)
			(setq *state* state)
			(if (> seconds 0) (seek seconds))
			(unless (eql state "s") (setq *handle* (audio-stream-open-rpc +rate 2)))
			(report))
		(:t (setq *state* "s")
			(mail-send *app* (cat "!" (str serial))))))

(defun command (msg)
	; (command msg) -> :nil if to go
	;the words of the line, a module's bytes after it
	(defq kind (first msg) end (ifn (find "\n" msg) (length msg))
		words (split (slice msg 1 end) " ") serial (if (nempty? words) (str-as-num (first words)) -1))
	(cond
		((eql kind "Q") :nil)
		((eql kind "K"))
		((eql kind "M")
			(load-song serial (second words) (str-as-num (third words)) (slice msg (inc end) -1)))
		((or (/= serial *serial*) (not *song*)))
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
			(defq state *state* seconds (str-as-num (second words)))
			(seek seconds)
			(setq *state* (if (eql state "s") "p" state) *told* :nil
				*handle* (audio-stream-open-rpc +rate 2))
			(report))))

(defun main ()
	(defq select (task-mboxes +select_size) running :t heard_at (pii-time))
	;the first word is the window's mailbox
	(when (defq msg (mail-read-timeout (elem-get select +select_main) +orphan))
		(setq *app* msg)
		(mail-timeout (elem-get select +select_timer) +frame_time 0)
		(while running
			(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
			(cond
				((= idx +select_timer)
					(mail-timeout (elem-get select +select_timer) +frame_time 0)
					;the window gone, so is the player
					(if (> (- (pii-time) heard_at) +orphan) (setq running :nil))
					(when (and *song* (eql *state* "p"))
						(feed)
						(report)
						;all of it heard
						(when (and *ended* (>= *heard* *written*) (not *told*))
							(setq *told* :t)
							(mail-send *app* (cat "E" (str *serial*))))))
				(:t (setq heard_at (pii-time) running (command msg))))))
	(sound-stop))
