(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "service/audio/app.inc")
(import "apps/system/files/picker.inc")
(import "lib/media/mod.inc")
(import "lib/media/tracker.inc")
(import "lib/media/spectrum.inc")
(import "./library.inc")
(import "./widgets.inc")

;A player of the modules of the Amiga's trackers, and a jukebox of the
;user's favourites. A song is played by lib/media/tracker.inc a tick at a
;time into a stream of the Audio service, a little ahead of what is heard;
;what is shown is of what is heard: the row, the samples, and the bars of
;its spectrum, lib/media/spectrum.inc. The modules come from a file, the
;favourites, kept to play offline, or Modland, ./library.inc

(enums +select 0
	(enum main timer tip fetch picker))

;what is kept of the frames written covers what is heard, which is as far
;behind as the stream is ahead, and the FFT's frames before it
(defq +rate 44100 +ahead 11025 +frame_time (/ 1000000 30) +fft 1024 +recent_max (+ +ahead +fft 4096)
	+names ''("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-")
	;the favourites, Player or Jukebox, shuffle, which list and the folder
	;of Modland, as they were when last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "jukebox.tre")
	*favourites* (list) *jukebox* :nil *shuffled* :nil *list_at* 1 *browse* (list)
	;the song, its bytes, what plays it, the stream, the frames written and
	;the place in the song each tick's frames began at, and the frames of
	;late, for the bars
	*song* :nil *data* :nil *player* :nil *handle* :nil *written* 0 *marks* (list)
	*recent* "" *recent_at* 0 *length* 0 *paused* :nil *ended* :nil *shown* :nil
	;what plays now, (title url), and the list it is of, and in what order
	*current* :nil *queue* (list) *order* (list) *queue_at* 0
	;the entries of the list shown, and what is being fetched
	*entries* (list) *fetching* :nil *picker* :nil *select* :nil
	*spectrum* (spectrum-make +fft +rate +bars)
	*levels* (map! (lambda () 0) '() 0 +bars) *peaks* (map! (lambda () 0) '() 0 +bars))

(defun hex (n width)
	; (hex n width) -> str
	(defq out "")
	(times width (setq out (cat (elem-get "0123456789ABCDEF" (logand n 15)) out) n (>> n 4)))
	out)

(defun clock (frames)
	; (clock frames) -> str
	(defq s (/ frames +rate))
	(cat (str (/ s 60)) ":" (if (< (% s 60) 10) "0" "") (str (% s 60))))

(defun status (text)
	(. *status* :set_text text))

(defun local? (url)
	(not (starts-with "https://" url)))

(defun favourite? (url)
	(some (# (eql (second %0) url)) *favourites*))

(defun find-text (needle hay)
	; (find-text needle hay) -> :nil | :t
	;is needle somewhere in hay
	(defq n (length needle) end (- (length hay) n) at 0 found :nil)
	(while (and (not found) (<= at end))
		(if (eql (slice hay at (+ at n)) needle) (setq found :t) (setq at (inc at))))
	found)

;;;;;;;;;;;;;;;;;
; what is shown
;;;;;;;;;;;;;;;;;

(defun note-name (period)
	(if (= period 0) "---"
		(cat (elem-get +names (% (defq n (tracker-note period)) 12)) (str (inc (/ n 12))))))

(defun cell-text (period sample effect param)
	(cat (note-name period) " " (if (> sample 0) (hex sample 2) "..") " "
		(if (or (> effect 0) (> param 0)) (cat (hex effect 1) (hex param 2)) "...")))

(defun rows-show (order row)
	;the rows of the pattern about the one heard, it in the middle
	(defq channels (. *song* :find :channels) half (/ +rows_shown 2)
		cells (elem-get (. *song* :find :patterns) (elem-get (. *song* :find :orders) order))
		lines (list))
	(each! (lambda ()
			(defq r (+ row (- (!) half)))
			(push lines (if (<= 0 r 63)
				(apply (const cat) (cat (list (hex r 2) (if (= r row) "> " "  "))
					(map! (lambda ()
							(defq c (* (+ (* r channels) (!)) 4))
							(cat (cell-text (elem-get cells c) (elem-get cells (+ c 1))
								(elem-get cells (+ c 2)) (elem-get cells (+ c 3))) " "))
						'() 0 channels)))
				"")))
		'() 0 +rows_shown)
	(. *rows* :load lines 0 0 -1 -1))

(defun samples-show ()
	;the samples of the song, a line each, those a channel plays now marked
	(defq voices (. *player* :find :voices) playing (list))
	(each! (lambda ()
			(defq s (elem-get voices (* (!) +mix_size)))
			(and (>= s 0) (> (elem-get voices (+ (* (!) +mix_size) 3)) 0) (push playing s)))
		'() 0 (. *song* :find :channels))
	(. *samples* :load (map (lambda (sample)
			(cat (if (find (!) playing) "*" " ") (hex (inc (!)) 2) " " (. sample :find :name)))
		(. *song* :find :samples)) 0 0 -1 -1))

(defun bars-show (heard)
	;the bars of what was heard last, each falling back as it goes quiet,
	;and its peak falling slower: green, yellow over two thirds, red over
	;five sixths
	(defq from (- heard +fft *recent_at*) w (/ +bars_width +bars) h +bars_height
		bands (if (and (>= from 0) (<= (+ from +fft) (/ (length *recent*) 4)))
			(map (# (n2i %0)) (spectrum-bands *spectrum* *recent* from))
			(map! (lambda () 0) '() 0 +bars)))
	(.-> *bars* (:fill +argb_black))
	(each! (lambda (db)
			;the top 46 dB shown, a full scale tone about 86: music lives in
			;its top 25 or so, all of 80 had the bars stand still
			(defq i (!) v (max 0 (min h (/ (* (- db 40) h) 46)))
				level (max v (- (elem-get *levels* i) 4))
				peak (max level (- (elem-get *peaks* i) 1))
				x (* i w) top (- h level) third (/ h 3) sixth (/ h 6))
			(elem-set *levels* i level)
			(elem-set *peaks* i peak)
			(.-> *bars* (:set_color +argb_green) (:fbox x (max top third) (- w 2) (- h (max top third))))
			(if (< top third) (.-> *bars* (:set_color +argb_yellow) (:fbox x (max top sixth) (- w 2) (- third (max top sixth)))))
			(if (< top sixth) (.-> *bars* (:set_color +argb_red) (:fbox x top (- w 2) (- sixth top))))
			(.-> *bars* (:set_color +argb_white) (:fbox x (- h peak 1) (- w 2) 2)))
		(list bands))
	(. *bars* :swap +swap_write))

(defun favourite-show ()
	;the pin lit while what plays is a favourite, the other not
	(defq on (and *current* (favourite? (second *current*))))
	(each (lambda (btn lit)
			(if lit (def (. btn :dirty) :paper (get :paper_highlight *env*)) (undef (. btn :dirty) :paper)))
		(slice (. *favourite* :children) 0 2) (list on (not on))))

;;;;;;;;;;;;;;;;;
; the sound of it
;;;;;;;;;;;;;;;;;

(defun sound-stop ()
	(when *handle*
		(audio-stream-close-rpc *handle*)
		(setq *handle* :nil)))

(defun song-rewind ()
	;the song from its start, nothing written yet
	(sound-stop)
	(setq *player* (tracker-player *song* +rate) *written* 0 *marks* (list)
		*recent* "" *recent_at* 0 *ended* :nil *shown* :nil *paused* :nil))

(defun song-start (data title url)
	; (song-start data title url) -> :nil | :t
	;a module's bytes played from its start, if they are one
	(when (defq song (mod-read data))
		(setq *song* song *data* data *current* (list title url) *length* (tracker-length song +rate))
		(song-rewind)
		(setq *handle* (audio-stream-open-rpc +rate 2))
		;as many columns as it has channels
		(def *rows* :vdu_width (+ 4 (* 11 (. song :find :channels))))
		(samples-show)
		(rows-show 0 0)
		(. *title* :set_text (cat "Jukebox -> " title))
		(favourite-show)
		(. *window* :fit)
		:t))

(defun song-stop ()
	;stopped, to play from its start
	(if *song* (song-rewind)))

(defun feed ()
	; (feed) -> :nil | heard
	;ticks of the song written till the stream is far enough ahead of what is
	;heard, and each tick's place kept, to show it as it is heard
	(when *handle*
		(defq left (ifn (audio-stream-left-rpc *handle*) 0))
		(unless *paused*
			(while (and (< left +ahead) (not *ended*))
				(push *marks* (list *written* (. *player* :find :order) (. *player* :find :row)))
				(defq pcm (tracker-next *player*) n (/ (length pcm) 4))
				(audio-stream-write-rpc *handle* pcm)
				(setq *recent* (cat *recent* pcm) *written* (+ *written* n) left (+ left n)
					*ended* (. *player* :find :ended))
				(when (> (defq over (- (/ (length *recent*) 4) +recent_max)) 0)
					(setq *recent* (slice *recent* (* 4 over) -1) *recent_at* (+ *recent_at* over)))))
		(- *written* left)))

;;;;;;;;;;;;;;;;
; what is played
;;;;;;;;;;;;;;;;

(defun fetch-start (kind url &optional title)
	;a module or a listing fetched by a task of its own, the window drawn
	;meanwhile. A new mailbox each time, what was asked before is not heard
	(elem-set *select* +select_fetch (defq reply (mail-mbox)))
	(setq *fetching* (list kind url title))
	(status (cat "Fetching " (ifn title "the folder") " ..."))
	(mail-send (open-child (cat *app_root* "fetch.lisp") +kn_call_pin) (cat reply url)))

(defun entry-play (entry)
	;an entry played, (title url): a file, a favourite kept, or fetched
	(bind '(title url) entry)
	(cond
		((defq data (load (if (local? url) url (kept-path url))))
			(unless (song-start data title url) (status (cat title ": not a module it can read"))))
		((local? url) (status (cat url ": not there")))
		(:t ;what played stops, and the window says what is coming, till it
			;has come
			(sound-stop)
			(setq *song* :nil *current* :nil)
			(. *title* :set_text (cat "Jukebox -> " title))
			(. *samples* :load (list "" "  fetching ...") 0 0 -1 -1)
			(. *rows* :load (list) 0 0 -1 -1)
			(favourite-show)
			(fetch-start :module url title))))

(defun queue-start (entries at)
	;a list played from one of it, in its order, or shuffled with that first
	(setq *queue* entries *order* (map! (lambda () (!)) '() 0 (length entries)))
	(if *shuffled* (setq *order* (cat (list at) (shuffle (filter (# (/= %0 at)) *order*)))))
	(setq *queue_at* (ifn (find at *order*) 0))
	(if (nempty? *queue*) (entry-play (elem-get *queue* (elem-get *order* *queue_at*)))))

(defun queue-step (by)
	;the next or the previous of the list, round from the end
	(when (nempty? *queue*)
		(setq *queue_at* (% (+ *queue_at* by (length *order*)) (length *order*)))
		(entry-play (elem-get *queue* (elem-get *order* *queue_at*)))))

(defun frame ()
	;what is heard now, shown: its row, its samples, its bars and where it is
	(when *song*
		(defq heard (ifn (feed) 0))
		;the place of the tick heard, those before it let go
		(while (and (> (length *marks*) 1) (<= (first (second *marks*)) heard))
			(setq *marks* (slice *marks* 1 -1)))
		(when (defq mark (first *marks*))
			(bind '(& order row) mark)
			(unless (eql *shown* (list order row))
				(setq *shown* (list order row))
				(rows-show order row)
				(samples-show)))
		(bars-show heard)
		(status (cat (. *song* :find :title) ", " (. *song* :find :kind) " of "
			(str (. *song* :find :channels)) " channels, "
			(if *shown* (cat "order " (str (inc (first *shown*))) " of " (str (length (. *song* :find :orders))) ", ") "")
			(clock (min heard *length*)) " of " (clock *length*)
			(cond (*paused* ", paused") ((not *handle*) ", stopped") (""))))
		;all of it heard: the next of the jukebox, or stopped
		(and *handle* *ended* (>= heard *written*)
			(if *jukebox* (queue-step 1) (song-stop)))))

;;;;;;;;;
; config
;;;;;;;;;

(defun config-load ()
	;as it was when last closed, or as it is the first time: no favourites,
	;the player, Modland's top
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version :favourites (list)
			:jukebox :nil :shuffled :nil :list_at 1 :browse (list))))
	(setq *favourites* (. *config* :find :favourites) *jukebox* (. *config* :find :jukebox)
		*shuffled* (. *config* :find :shuffled) *list_at* (. *config* :find :list_at)
		*browse* (. *config* :find :browse))
	(. *mode* :set_selected (if *jukebox* 1 0))
	(. *shuffle* :set_states (list *shuffled*))
	(. *tab* :set_selected *list_at*))

(defun config-save ()
	(scatter *config* :favourites *favourites* :jukebox *jukebox* :shuffled *shuffled*
		:list_at *list_at* :browse *browse*)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; the list, and fetching
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun browse-url ()
	(apply (const cat) (cat (list +modland) (map (# (cat (url-encode %0) "/")) *browse*))))

(defun list-entries ()
	; (list-entries) -> ((title url folder?) ...)
	;the favourites, or the folder of Modland shown, its listing kept once
	;fetched, the trackers' folders at its top
	(cond
		((= *list_at* 0) (map (# (list (first %0) (second %0) :nil)) *favourites*))
		((empty? *browse*) (map (# (list %0 (cat +modland (url-encode %0) "/") :t)) +modland_kinds))
		((defq html (load (kept-path (defq url (browse-url)))))
			(map (# (list (entry-title (first %0)) (cat url (first %0)) (second %0))) (listing-read html)))
		(:t (fetch-start :listing url) (list))))

(defun list-show ()
	;a button a row, as many as the filter leaves, to so many
	(defq filter_text (to-lower (get :clear_text *filter*)))
	(setq *entries* (filter (# (or (eql filter_text "") (find-text filter_text (to-lower (first %0)))))
		(list-entries)))
	(each (# (. %0 :sub)) (. *list* :children))
	(each! (lambda ((title url folder))
			(def (defq button (Button)) :text (if folder (cat title "/") title) :border 0
				:paper +argb_black :ink +argb_white
				:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
			(view-variant button :font :font_button)
			(. button :connect (+ +event_item (!)))
			(. *list* :add_child button))
		(list *entries*) 0 (min +list_max (length *entries*)))
	(. *where* :set_text (cond
		((= *list_at* 0) (cat (str (length *favourites*)) " favourites"))
		((empty? *browse*) "Modland, the trackers played here")
		((cat "Modland / " (join *browse* " / ") ", " (str (length *entries*)) " here"))))
	(.-> *list_scroll* :layout :dirty_all))

(defun fetch-done (msg)
	(bind '(kind url title) *fetching*)
	(setq *fetching* :nil)
	(cond
		((starts-with "!" msg) (status (cat "Could not fetch it: " (slice msg 1 -1))))
		((eql kind :listing)
			(save msg (kept-path url))
			(unless *song* (status "Nothing playing"))
			(list-show))
		((not (song-start msg title url)) (status (cat title ": not a module it can read")))))

(defun item-pressed (i)
	;a favourite played, a folder opened, or a module of Modland played,
	;the modules of the list what next and prev go through
	(bind '(title url folder) (elem-get *entries* i))
	(cond
		(folder (push *browse* title) (config-save) (list-show))
		(:t (defq modules (filter (# (not (third %0))) *entries*))
			(queue-start (map (# (list (first %0) (second %0))) modules)
				(ifn (find title (map (const first) modules)) 0)))))

(defun favourite-add ()
	;what plays now a favourite, its bytes kept to play offline
	(when (and *current* (not (favourite? (second *current*))))
		(push *favourites* *current*)
		(unless (local? (second *current*)) (save *data* (kept-path (second *current*))))
		(config-save)
		(favourite-show)
		(if (= *list_at* 0) (list-show))))

(defun favourite-remove ()
	(when (and *current* (favourite? (defq url (second *current*))))
		(setq *favourites* (filter (# (nql (second %0) url)) *favourites*))
		(unless (local? url) (pii-remove (kept-path url)))
		(config-save)
		(favourite-show)
		(if (= *list_at* 0) (list-show))))

(defun main ()
	(setq *select* (task-mboxes +select_size))
	(def *window* :tip_mbox (elem-get *select* +select_tip))
	(config-load)
	(.-> *bars* (:fill +argb_black) (:swap +swap_write))
	(. *samples* :load (list "" "  open a module, or" "  pick one below") 0 0 -1 -1)
	(. *rows* :load (list) 0 0 -1 -1)
	(list-show)
	(. *window* :open)
	(mail-timeout (elem-get *select* +select_timer) +frame_time 0)
	(defq id :t)
	(while id
		(defq msg (mail-read (elem-get *select* (defq idx (mail-select *select*)))))
		(cond
			((= idx +select_timer)
				(mail-timeout (elem-get *select* +select_timer) +frame_time 0)
				(frame))
			((= idx +select_tip) (. *window* :tip msg))
			((= idx +select_fetch) (fetch-done msg))
			((= idx +select_picker)
				(setq *picker* :nil)
				(unless (eql (defq file (trim msg)) "")
					(entry-play (list (last (split file "/")) file))))
			((= (setq id (getf msg +ev_msg_target_id)) +event_close) (setq id :nil))
			((= id +event_play)
				;from its start after a stop, on again after a pause
				(cond
					((and *song* (not *handle*))
						(song-rewind)
						(setq *handle* (audio-stream-open-rpc +rate 2)))
					((setq *paused* :nil))))
			((= id +event_pause) (if *handle* (setq *paused* (not *paused*))))
			((= id +event_stop) (song-stop))
			((= id +event_next) (queue-step 1))
			((= id +event_prev) (queue-step -1))
			((= id +event_pin) (favourite-add))
			((= id +event_unpin) (favourite-remove))
			((= id +event_open)
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get *select* +select_picker) "Open module..." "." ".mod" :nil "mods" :t (list))))
			((= id +event_mode)
				;the jukebox plays the favourites
				(setq *jukebox* (= (. *mode* :get_selected) 1))
				(config-save)
				(and *jukebox* (nempty? *favourites*) (queue-start *favourites* 0)))
			((= id +event_shuffle)
				(setq *shuffled* (first (. *shuffle* :get_states)))
				(config-save))
			((= id +event_tab)
				(setq *list_at* (. *tab* :get_selected))
				(config-save)
				(list-show))
			((= id +event_up)
				(when (nempty? *browse*) (pop *browse*) (config-save) (list-show)))
			((= id +event_filter) (list-show))
			((and (= (getf msg +ev_msg_type) +ev_type_action)
					(<= +event_item id (+ +event_item (dec (length *entries*)))))
				(item-pressed (- id +event_item)))
			((. *window* :event msg))))
	(config-save)
	(if *picker* (mail-send *picker* ""))
	(sound-stop)
	(. *window* :close))
