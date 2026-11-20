(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "apps/system/files/picker.inc")
(import "lib/files/names.inc")
(import "lib/media/module.inc")
(import "lib/text/searching.inc")
(import "./library.inc")
(import "./widgets.inc")

;A player of the modules of the trackers, the Amiga's MODs, Scream
;Tracker's S3Ms and FastTracker's XMs, and a jukebox of the user's favourites. A song is played
;by ./player.lisp, a task on another node, so what draws the screen is not
;slowed by it; it says what is heard, and this shows it: the row, the
;samples, and the bars of its spectrum. The modules come from a file, a
;store, the favourites, kept to play offline, or Modland, ./library.inc

(enums +select 0
	(enum main timer tip fetch picker engine))

(defq +rate 44100 +alive 1000000 +rows_wide 136
	+names ''("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-")
	;the favourites, Player or Jukebox, shuffle, which list and the folder
	;of Modland, as they were when last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "jukebox.tre")
	*favourites* (list) *jukebox* :nil *shuffled* :nil *list_at* 1 *browse* (list)
	;the player, the song shown and its number, its bytes, how long it is,
	;and what the player last said: heard, where, played, held or stopped,
	;and the samples that sound
	*engine* :nil *song* :nil *serial* 0 *seek_id* 0 *seek_at* 0 *data* :nil *length* 0
	*heard* 0 *place* :nil *state* "s" *playing* (list) *shown* :nil
	;what plays now, (title url), and the list it is of, and in what order,
	;and what was playing when last closed, (title url seconds state), to go on
	*current* :nil *queue* (list) *order* (list) *queue_at* 0 *resume* :nil
	;the entries of the list shown, the url of the one played or fetched,
	;marked, and the title of the folder gone up from, and what is being
	;fetched
	*entries* (list) *top* 0 *marked* :nil *left* :nil *fetching* :nil *picker* :nil *select* :nil
	;the fetcher, ./fetch.lisp, and the number of the last fetch asked of it
	*fetcher* :nil *fetch_id* 0
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

(defun module-load (name)
	; (module-load name) -> :nil | str
	;the bytes of a module of a file, or of an object of a store, store:path,
	;lib/files/names.inc
	(if (name-in-store name)
		(and (defq stream (name-whole-stream name)) (read-blk stream (ifn (name-size name) 0)))
		(load name)))

(defun favourite? (url)
	(some (# (eql (second %0) url)) *favourites*))

(defun engine-send (&rest words)
	;the player told, of this song
	(if *engine* (mail-send *engine* (apply (const cat) (map (const str) words)))))

;;;;;;;;;;;;;;;;;
; what is shown
;;;;;;;;;;;;;;;;;

(defun note-name (note)
	(cond
		((= note 0) "---")
		((= note +note_cut) "^^^")
		((= note +note_off) "===")
		((cat (elem-get +names (% (dec note) 12)) (str (/ (dec note) 12))))))

(defun cell-text (note ins vol effect param)
	;a cell as its tracker shows it: an S3M's volume and its effects by
	;letter, A 1; an XM's volume column, a volume or an effect of a sign
	;and a digit, and its effects by number then letter, G 16; a MOD's
	;effects by number
	(defq mode (. *song* :find :mode))
	(cat (note-name note) " " (if (> ins 0) (hex ins 2) "..")
		(case mode
			(:st3 (if (= vol +no_volume) " .." (cat " " (hex vol 2))))
			(:ft2 (cond
				((< vol 0x10) " ..")
				((<= vol 0x50) (cat " " (hex (- vol 0x10) 2)))
				((cat " " (elem-get "------+DUSVPLRM" (>> vol 4)) (hex vol 1)))))
			(:t ""))
		" " (cond
			((and (= effect 0) (= param 0)) "...")
			((eql mode :st3) (cat (char (+ 64 effect)) (hex param 2)))
			((< effect 16) (cat (hex effect 1) (hex param 2)))
			((cat (char (+ 55 effect)) (hex param 2))))))

(defun cell-width ()
	(if (eql (. *song* :find :mode) :mod) 11 14))

(defun rows-show (order row)
	;the rows of the pattern about the one heard, it in the middle, as many
	;channels as fit
	(defq channels (. *song* :find :channels) half (/ +rows_shown 2)
		pattern (elem-get (. *song* :find :orders) order)
		cells (elem-get (. *song* :find :patterns) pattern) rows (elem-get (. *song* :find :rows) pattern)
		shown (min channels (/ (- +rows_wide 4) (cell-width))) lines (list))
	(each! (lambda ()
			(defq r (+ row (- (!) half)))
			(push lines (if (<= 0 r (dec rows))
				(apply (const cat) (cat (list (hex r 2) (if (= r row) "> " "  "))
					(map! (lambda ()
							(defq c (* (+ (* r channels) (!)) +cell_size))
							(cat (apply (const cell-text) (slice cells c (+ c +cell_size))) " "))
						'() 0 shown)))
				"")))
		'() 0 +rows_shown)
	(. *rows* :load lines 0 0 -1 -1))

(defun samples-show ()
	;the samples of the song, a line each, those a channel plays now marked
	(. *samples* :load (map (lambda (sample)
			(cat (if (find (str (!)) *playing*) "*" " ") (hex (inc (!)) 2) " " (. sample :find :name)))
		(. *song* :find :samples)) 0 0 -1 -1))

(defun bars-show (bands)
	;the bars of what was heard last, each falling back as it goes quiet,
	;and its peak falling slower: green, yellow over two thirds, red over
	;five sixths
	(defq w (/ +bars_width +bars) h +bars_height)
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
	;the pin lit while what plays is a favourite
	(defq btn (first (. *favourite* :children)))
	(if (and *current* (favourite? (second *current*)))
		(def (. btn :dirty) :paper (get :paper_highlight *env*))
		(undef (. btn :dirty) :paper)))

(defun song-status ()
	;a song with no title of its own by the name of its file
	(defq title (trim (. *song* :find :title)))
	(status (cat (if (eql title "") (first *current*) title) ", " (. *song* :find :kind) " of "
		(str (. *song* :find :channels)) " channels, "
		(if *place* (cat "order " (str (inc (first *place*))) " of " (str (length (. *song* :find :orders))) ", ") "")
		(clock (min *heard* *length*)) " of " (if (> *length* 0) (clock *length*) "?")
		(cond ((eql *state* "h") ", paused") ((eql *state* "s") ", stopped") ("")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the player, ./player.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun away-task (file mbox)
	; (away-task file mbox) -> mailbox
	;a task of the app's on a node of this machine not this one, if there is
	;another, told the mailbox it tells the window by
	(defq me (task-nodeid) others (filter (# (not (eql %0 me))) (lisp-nodes :t))
		task (open-remote (cat *app_root* file)
			(if (nempty? others) (elem-get others (random (length others))) me) +kn_call_pin))
	(mail-send task mbox)
	task)

(defun engine-start ()
	;the player and the fetcher
	(setq *engine* (away-task "player.lisp" (elem-get *select* +select_engine))
		*fetcher* (away-task "fetch.lisp" (elem-get *select* +select_fetch))))

(defun song-start (data title url)
	; (song-start data title url) -> :nil | :t
	;a module's bytes played, if they are one: from its start, or as it was
	;when last closed if it is that one
	(when (defq song (module-read data :t))
		(defq state "p" seconds 0)
		(when (and *resume* (eql (second *resume*) url))
			(bind '(& & at was) *resume*)
			(setq *resume* :nil seconds at state (cond ((eql was "paused") "h") ((eql was "stopped") "s") ("p"))))
		(setq *song* song *data* data *current* (list title url) *length* 0 *heard* (* seconds +rate)
			*place* :nil *shown* :nil *playing* (list) *state* state *serial* (inc *serial*))
		(def (. *seek* :dirty) :maximum 0 :value seconds)
		(engine-send "M" *serial* " " state " " seconds "\n" data)
		;as many columns as it has channels, as fit
		(def *rows* :vdu_width (+ 4 (* (cell-width) (min (. song :find :channels)
			(/ (- +rows_wide 4) (cell-width))))))
		(samples-show)
		(rows-show 0 0)
		(. *title* :set_text (cat "Jukebox -> " title))
		(favourite-show)
		(song-status)
		(. *window* :fit)
		:t))

;;;;;;;;;;;;;;;;
; what is played
;;;;;;;;;;;;;;;;

(defun loading (part)
	; (loading part)
	;how much of what is fetched has come, out of 1000, the bar in front of
	;the seek bar while it comes, :nil it has come
	(cond
		(part (def (. *loading* :dirty) :value part)
			(. *loading* :to_front))
		((. (. *seek* :dirty) :to_front))))

(defun fetch-start (kind url &optional title)
	;a module or a listing fetched by the fetcher, the window drawn
	;meanwhile. Each has a number, what is said of one before is not heard
	(setq *fetching* (list kind url title) *fetch_id* (inc *fetch_id*))
	(loading 0)
	(status (cat "Fetching " (ifn title "the folder") " ..."))
	(mail-send *fetcher* (cat "F" (str *fetch_id*) " " url)))

(defun list-mark ()
	; (list-mark)
	;the rows seen, from the entry at the top: what is played or fetched,
	;and the folder gone up from, on dark grey, the rest on black
	(each (lambda (button)
			;a row past the list's end a space, so it keeps its height
			(defq paper +argb_black text " " at (+ *top* (!)))
			(when (< at (length *entries*))
				(bind '(title url folder) (elem-get *entries* at))
				(setq text (if folder (cat title "/") title))
				(if (or (eql url *marked*) (and folder (eql title *left*))) (setq paper +argb_grey4)))
			(. button :set_text text)
			(unless (= (get :paper button) paper) (def (. button :dirty) :paper paper)))
		(. *list* :children)))

(defun list-at (top)
	; (list-at top)
	;the list seen from an entry, the slider with it
	(setq *top* (max 0 (min top (- (length *entries*) +list_rows))))
	(def (. *list_slider* :dirty) :value *top* :maximum (max 0 (- (length *entries*) +list_rows)))
	(list-mark))

(defun entry-play (entry)
	;an entry played, (title url): a file, a favourite kept, or fetched
	(bind '(title url) entry)
	(setq *marked* url)
	(list-mark)
	(cond
		((defq data (module-load (if (local? url) url (kept-path url))))
			(unless (song-start data title url) (status (cat title ": not a module it can read"))))
		((local? url) (status (cat url ": not there")))
		(:t ;what played stops, and the window says what is coming, till it
			;has come
			(engine-send "S" *serial*)
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

(defun engine-said (msg)
	;what the player said, of this song only
	(defq kind (first msg) words (split (slice msg 1 (ifn (find "|" msg) (length msg))) " ")
		serial (str-as-num (first words)))
	(and *song* (= serial *serial*)
		(cond
			((eql kind "I")
				(setq *length* (str-as-num (second words)))
				(def (. *seek* :dirty) :maximum (/ *length* +rate))
				(song-status))
			((eql kind "!")
				(setq *song* :nil)
				(status (cat (first *current*) ": not a module it can read")))
			((eql kind "T")
				;the row and the samples shown again only when they change
				;of a seek before the last asked for, not shown
				(bind '(& id heard order row state &rest playing) words)
				(when (= (str-as-num id) *seek_id*)
					(setq *heard* (str-as-num heard) *state* state)
					(unless (eql *shown* (defq place (cat order " " row)))
						(setq *shown* place *place* (list (str-as-num order) (str-as-num row)))
						(rows-show (first *place*) (second *place*)))
					(unless (eql (join *playing* " ") (join playing " "))
						(setq *playing* playing)
						(samples-show))
					(bars-show (map (const str-as-num) (split (slice msg (inc (find "|" msg)) -1) " ")))
					;the bar left to the hand that drags it, a second after the last
					(and (> (- (pii-time) *seek_at*) 1000000)
						(/= (get :value *seek*) (defq at (/ (min *heard* *length*) +rate)))
						(def (. *seek* :dirty) :value at))
					(song-status)))
			((eql kind "E")
				;all of it heard: the next of the jukebox, or stopped
				(if *jukebox* (queue-step 1) (engine-send "S" *serial*))))))

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
		*browse* (. *config* :find :browse)
		;what was playing, none in a config kept before it was
		*queue* (ifn (. *config* :find :queue) (list)) *order* (ifn (. *config* :find :order) (list))
		*queue_at* (ifn (. *config* :find :queue_at) 0))
	(setq *resume* (. *config* :find :playing))
	(. *mode* :set_selected (if *jukebox* 1 0))
	(. *shuffle* :set_states (list *shuffled*))
	(. *tab* :set_selected *list_at*))

(defun config-save ()
	;and what plays, or is fetched, where it is and how
	(defq entry (cond
		(*current*)
		((and *fetching* (eql (first *fetching*) :module)) (list (third *fetching*) (second *fetching*))))
		state (cond ((not *current*) "playing") ((eql *state* "h") "paused") ((eql *state* "s") "stopped") ("playing")))
	(scatter *config* :favourites *favourites* :jukebox *jukebox* :shuffled *shuffled*
		:list_at *list_at* :browse *browse* :queue *queue* :order *order* :queue_at *queue_at*
		:playing (if entry (list (first entry) (second entry) (if *current* (/ *heard* +rate) 0) state)))
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
			;Startrekker AM's instruments, kept beside its modules, are not shown
			(map (# (list (entry-title (first %0)) (cat url (first %0)) (second %0)))
				(filter (# (not (ends-with ".nt" (first %0)))) (listing-read html))))
		(:t (fetch-start :listing url) (list))))

(defun list-show ()
	;a button a row, as many as the filter leaves, to so many
	;the filter a regexp, ignoring case, or plain text if it is not one
	(defq pattern (get :clear_text *filter*) found (query pattern :nil :t :t))
	(unless (second found) (setq found (query pattern :nil :nil :t)))
	(bind '(engine meta &ignore) found)
	(setq *entries* (filter (# (or (eql pattern "") (. engine :match? (to-lower (first %0)) meta)))
		(list-entries)))
	(. *where* :set_text (cond
		((= *list_at* 0) (cat (str (length *favourites*)) " favourites"))
		((empty? *browse*) "Modland, the trackers played here")
		((cat "Modland / " (join *browse* " / ") ", " (str (length *entries*)) " here"))))
	;from the top, or with the folder gone up from in the middle
	(list-at (if (defq at (and *left* (some (# (and (third %0) (eql (first %0) *left*) (!))) *entries*)))
		(- at (/ +list_rows 2)) 0)))

(defun list-rows ()
	; (list-rows)
	;the buttons of the rows seen, made the once, an event each
	(def *list* :slider *list_slider*)
	(times +list_rows
		(def (defq button (Button)) :text " " :border 0 :paper +argb_black :ink +argb_white
			:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
		;the letters of the tracker's rows, a size up
		(view-variant button :font :font_medium_terminal)
		(. button :connect (+ +event_item (length (. *list* :children))))
		(. *list* :add_child button)))

(defun fetch-ended (msg)
	;= and its bytes, or ! and why not
	(bind '(kind url title) *fetching*)
	(defq ok (starts-with "=" msg))
	(setq *fetching* :nil msg (slice msg 1 -1))
	(cond
		((not ok) (status (cat "Could not fetch it: " msg)))
		((eql kind :listing)
			(save msg (kept-path url))
			(unless *song* (status "Nothing playing"))
			(list-show))
		((not (song-start msg title url)) (status (cat title ": not a module it can read")))))

(defun fetch-done (msg)
	;what the fetcher said, of the last fetch asked of it only
	(defq kind (first msg) gap (ifn (find " " msg) (length msg)) rest (slice msg (min (length msg) (inc gap)) -1))
	(cond
		;mail is unordered, how far it got may come after it has come
		((or (not *fetching*) (/= (str-as-num (slice msg 1 gap)) *fetch_id*)))
		((eql kind "%")
			;how far it has got, of how many, or of a megabyte if it is not
			;said
			(bind '(got total) (map (const str-as-num) (split rest " ")))
			(loading (min 1000 (/ (* got 1000) (if (> total 0) total (max got 1048576))))))
		(:t (loading :nil)
			(fetch-ended (cat kind rest)))))

(defun player-mode (url)
	; (player-mode url)
	;a module picked that is not a favourite is played by hand, so Player
	;mode; a favourite picked, the jukebox goes on from it
	(when (and *jukebox* (not (favourite? url)))
		(setq *jukebox* :nil)
		(. *mode* :set_selected 0)
		(config-save)))

(defun item-pressed (i)
	;a favourite played, a folder opened, or a module of Modland played,
	;the modules of the list what next and prev go through
	(bind '(title url folder) (elem-get *entries* (+ *top* i)))
	(cond
		(folder (setq *left* :nil) (push *browse* title) (config-save) (list-show))
		(:t (player-mode url)
			(defq modules (filter (# (not (third %0))) *entries*))
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
	;one of it on this machine, ./app.lisp: what is sent to it is not read
	(defq service (mail-declare (mail-mbox) "@Jukebox" "Jukebox Service 1.0"))
	(def *window* :tip_mbox (elem-get *select* +select_tip))
	(config-load)
	(engine-start)
	(.-> *bars* (:fill +argb_black) (:swap +swap_write))
	(. *samples* :load (list "" "  open a module, or" "  pick one below") 0 0 -1 -1)
	(. *rows* :load (list) 0 0 -1 -1)
	(list-rows)
	(list-show)
	(. *window* :open)
	;what was playing when last closed, on again
	(if *resume* (entry-play (slice *resume* 0 2)))
	(mail-timeout (elem-get *select* +select_timer) +alive 0)
	(defq id :t)
	(while id
		(defq msg (mail-read (elem-get *select* (defq idx (mail-select *select*)))))
		(cond
			((= idx +select_timer)
				;the player and the fetcher told the window is still here
				(mail-timeout (elem-get *select* +select_timer) +alive 0)
				(engine-send "K")
				(mail-send *fetcher* "K"))
			((= idx +select_engine) (engine-said msg))
			((= idx +select_tip) (. *window* :tip msg))
			((= idx +select_fetch) (fetch-done msg))
			((= idx +select_picker)
				(setq *picker* :nil)
				(unless (eql (defq file (trim msg)) "")
					(player-mode file)
					(entry-play (list (last (split file "/")) file))))
			((= (setq id (getf msg +ev_msg_target_id)) +event_close) (setq id :nil))
			((= id +event_play) (engine-send "P" *serial*))
			((= id +event_pause) (engine-send "H" *serial*))
			((= id +event_stop) (engine-send "S" *serial*))
			((= id +event_next) (queue-step 1))
			((= id +event_prev) (queue-step -1))
			((= id +event_pin)
				;a favourite no longer, or one now
				(if (and *current* (favourite? (second *current*))) (favourite-remove) (favourite-add)))
			((= id +event_open)
				(if *picker* (mail-send *picker* ""))
				(setq *picker* (file-pick (elem-get *select* +select_picker) "Open module..." "." ".mod .s3m .xm" :nil "mods" :t (list))))
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
				;the folder gone up from marked, and in view
				(when (nempty? *browse*)
					(setq *left* (pop *browse*))
					(config-save) (list-show)))
			((= id +event_filter) (list-show))
			((= id +event_list) (list-at (get :value *list_slider*)))
			((= id +event_seek)
				(setq *seek_at* (pii-time))
				;a drag sends many, only the last is played from: more of the
				;window's waiting, this is not it
				(and *song* (not (mail-poll (list (elem-get *select* +select_main))))
					(engine-send "G" *serial* " " (setq *seek_id* (inc *seek_id*)) " " (get :value *seek*))))
			((and (= (getf msg +ev_msg_type) +ev_type_action)
					(<= +event_item id (+ +event_item +list_rows -1))
					(< (+ *top* (- id +event_item)) (length *entries*)))
				(item-pressed (- id +event_item)))
			((. *window* :event msg))))
	(config-save)
	(if *picker* (mail-send *picker* ""))
	(engine-send "Q")
	(mail-send *fetcher* "Q")
	(setq service :nil)
	(. *window* :close))
