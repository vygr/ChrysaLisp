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

;A player of the modules of the trackers, the Amiga's MODs, MEDs, AHXs and HVLs, Scream
;Tracker's S3Ms, FastTracker's XMs and Impulse Tracker's ITs, and a jukebox
;of the user's favourites. A song is played by ./player.lisp, a task on
;another node, so what draws the screen is not slowed by it; it says what
;is heard, and this shows it: the row, the samples, and the bars of its
;spectrum. The modules come from a file, a store, the favourites, kept to
;play offline, or Modland, ./library.inc

(enums +select 0
	(enum main timer tip fetch picker engine))

;the lists, by their tabs
(enums +tab 0
	(enum favourites charts all search))

(defq +rate 44100 +alive 1000000 +rows_wide 136
	+names ''("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-")
	;the favourites, Player or Jukebox, shuffle, which list and the folder
	;of Modland and of the charts, as they were when last closed,
	;(config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "jukebox.tre")
	*favourites* (list) *jukebox* :nil *shuffled* :nil *list_at* +tab_all *browse* (list) *charts* (list)
	;the player, the song shown and its number, its bytes, how long it is,
	;and what the player last said: heard, where, played, held or stopped,
	;and the samples that sound
	*engine* :nil *song* :nil *serial* 0 *seek_id* 0 *seek_at* 0 *data* :nil *length* 0
	*heard* 0 *place* :nil *state* "s" *playing* (list) *shown* :nil
	;what plays now, (title url), and the list it is of, and in what order,
	;and what was playing when last closed, (title url seconds state), to go on
	*current* :nil *queue* (list) *order* (list) *queue_at* 0 *resume* :nil
	;which way through the queue what plays was come to, :nil if it was
	;picked, how many have failed in a row since one last played, and is
	;the next to be played for one that failed
	*skip_by* :nil *failed* 0 *skip_due* :nil
	;the entries of the list shown, the url of the one played or fetched,
	;marked, and the title of the folder gone up from, and what is being
	;fetched
	*entries* (list) *marked* :nil *left* :nil *fetching* :nil *picker* :nil *select* :nil
	;the fetcher, ./fetch.lisp, and the number of the last fetch asked of it,
	;and the listings fetched again since it was opened
	*fetcher* :nil *fetch_id* 0 *fresh* (list)
	;the index of where Modland has each module now, ./index.lisp, and of
	;each playlist where its modules are now, (url now ...), and those asked
	*indexer* :nil *resolved* (Fmap 31) *resolving* (list)
	;the playlists and folders of them with nothing played here, of the index
	*empty* (list)
	;the search of all of Modland, ./index.lisp: the number of the last
	;asked, what it found, and how many there are of which those are the first
	*search_id* 0 *found* (list) *found_total* 0
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
		((= note +note_fade) "~~~")
		((cat (elem-get +names (% (dec note) 12)) (str (/ (dec note) 12))))))

(defun cell-text (note ins vol effect param)
	;a cell as its tracker shows it: an S3M's volume and its effects by
	;letter, A 1; an XM's volume column, a volume or an effect of a sign
	;and a digit, and its effects by number then letter, G 16; an IT's
	;volume column, a volume or an effect of a letter and a digit, and
	;its effects by letter; a MOD's and an AHX's effects by number, and a MED's by two
	(defq mode (. *song* :find :mode))
	(cat (note-name note) " " (if (> ins 0) (hex ins 2) "..")
		(case mode
			(:st3 (if (= vol +no_volume) " .." (cat " " (hex vol 2))))
			(:ft2 (cond
				((< vol 0x10) " ..")
				((<= vol 0x50) (cat " " (hex (- vol 0x10) 2)))
				((cat " " (elem-get "-------+DUSVPLRM" (>> vol 4)) (hex vol 1)))))
			(:it (cond
				((<= vol 64) (cat " " (hex vol 2)))
				((< vol 125) (defq at (/ (- vol 65) 10)) (cat " " (elem-get "ABCDEF" at) (str (- vol 65 (* at 10)))))
				((<= 128 vol 192) (cat " P" (hex (min 15 (/ (- vol 128) 4)) 1)))
				((<= 193 vol 212) (defq at (/ (- vol 193) 10)) (cat " " (elem-get "GH" at) (str (- vol 193 (* at 10)))))
				(" ..")))
			(:t ""))
		" " (cond
			((and (= effect 0) (= param 0)) (if (eql mode :med) "...." "..."))
			((find mode '(:st3 :it)) (cat (char (+ 64 effect)) (hex param 2)))
			((eql mode :med) (cat (hex effect 2) (hex param 2)))
			((< effect 16) (cat (hex effect 1) (hex param 2)))
			((cat (char (+ 55 effect)) (hex param 2))))))

(defun cell-width ()
	(case (. *song* :find :mode) ((:mod :ahx) 11) (:med 12) (:t 14)))

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
		*fetcher* (away-task "fetch.lisp" (elem-get *select* +select_fetch))
		;where Modland has each module now, it answers as the fetcher does
		*indexer* (away-task "index.lisp" (elem-get *select* +select_fetch))))

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

(defun fetch-start (kind url &optional title missing)
	;a module or a listing fetched by the fetcher, the window drawn
	;meanwhile, and of a folder looked in for a module not found, its
	;name. Each has a number, what is said of one before is not heard
	(setq *fetching* (list kind url title missing) *fetch_id* (inc *fetch_id*))
	(loading 0)
	(status (cat "Fetching " (ifn title "the folder") " ..."))
	(mail-send *fetcher* (cat "F" (str *fetch_id*) " " url)))

(defun list-button (text paper)
	; (list-button text paper) -> button
	;a row of the list, the letters of the tracker's rows a size up
	(def (defq button (Button)) :text text :border 0 :paper paper :ink +argb_white
		:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))
	(view-variant button :font :font_medium_terminal)
	button)

(defun list-item (at)
	; (list-item index) -> button
	;the row of an entry, asked for by the list as it comes into sight:
	;what is played or fetched, and the folder gone up from, on dark grey,
	;the rest on black
	(bind '(title url folder) (elem-get *entries* at))
	(def (defq button (list-button (if folder (cat title "/") title)
			(if (or (eql url *marked*) (and folder (eql folder *left*))) +argb_grey4 +argb_black)))
		:entry_at at)
	(. button :connect +event_item))

(defun list-init ()
	; (list-init)
	;the list's rows asked for of (list-item), each the height of one, and
	;the list so many of them high
	(def *list* :item_fn (const list-item) :item_h (second (. (list-button " " +argb_black) :pref_size)))
	(def *list_scroll* :min_height (* +list_rows (get :item_h *list*))))

(defun list-visible ()
	; (list-visible)
	;the rows seen made again, and what is marked brought into sight: by
	;the scroll's :visible if its row is made, else the list moved to the
	;edge it is nearer
	(. *list* :refresh)
	(when (defq at (some (# (if (eql (second %0) *marked*) (!))) *entries*))
		(cond
			((defq button (some (# (if (eql (get :entry_at %0) at) %0)) (. *list* :children)))
				(. *list_scroll* :visible button))
			(:t (defq slider (get :vslider *list_scroll*) item_h (get :item_h *list*) y (* at item_h))
				(bind '(& h) (. *list_scroll* :view_size))
				(def slider :value (if (< y (get :value slider)) y (- (+ y item_h) h)))
				(.-> *list_scroll* :place :dirty_all)))))

(defun entry-play (entry)
	;an entry played, (title url): a file, a favourite kept, or fetched
	(bind '(title url) entry)
	(setq *marked* url)
	(list-visible)
	(cond
		((defq data (module-load (if (local? url) url (kept-path url))))
			(unless (song-start data title url) (song-failed (cat title ": not a module it can read"))))
		((local? url) (song-failed (cat url ": not there")))
		(:t ;what played stops, and the window says what is coming, till it
			;has come
			(engine-send "S" *serial*)
			(setq *song* :nil *current* :nil)
			(. *title* :set_text (cat "Jukebox -> " title))
			(. *samples* :load (list "" "  fetching ...") 0 0 -1 -1)
			(. *rows* :load (list) 0 0 -1 -1)
			(favourite-show)
			(fetch-start :module url title))))

(defun queue-set (entries at)
	;the list next and prev go through, in its order, or shuffled with the
	;one at at first, there now
	(setq *queue* entries *order* (map! (lambda () (!)) '() 0 (length entries)))
	(if *shuffled* (setq *order* (cat (list at) (shuffle (filter (# (/= %0 at)) *order*)))))
	(setq *queue_at* (ifn (find at *order*) 0)))

(defun queue-play ()
	;the queue's entry it is at played
	(if (nempty? *queue*) (entry-play (elem-get *queue* (elem-get *order* *queue_at*)))))

(defun queue-start (entries at)
	;a list played from one of it
	(queue-set entries at)
	(queue-play))

(defun queue-move (by)
	;the next or the previous of the queue, round from the end
	(when (nempty? *queue*)
		(setq *queue_at* (% (+ *queue_at* by (length *order*)) (length *order*)))
		(entry-play (elem-get *queue* (elem-get *order* *queue_at*)))))

(defun queue-modules ()
	; (queue-modules) -> ((title url) ...)
	;the modules of the list shown
	(map (# (list (first %0) (second %0))) (filter (# (not (third %0))) *entries*)))

(defun queue-align (by)
	; (queue-align by) -> :nil | :t
	;the queue next and prev go through: in Jukebox mode the favourites, as
	;it is, else the modules of the list shown, if any, made it if it is not
	;already. :t if what plays is in it, else :nil and it is at its first,
	;or its last going back
	(defq modules (queue-modules))
	(cond
		((or *jukebox* (empty? modules)
			(and (= (length modules) (length *queue*))
				(not (some (# (nql (second %0) (second %1))) modules *queue*)))))
		((defq at (some (# (if (eql (second %0) *marked*) (!))) modules))
			(queue-set modules at) :t)
		(:t (queue-set modules (if (> by 0) 0 (dec (length modules)))) :nil)))

(defun queue-step (by)
	;the next or the previous, round from the end, of the queue, on from what
	;plays, or its first or last if what plays is not in it
	(setq *skip_by* by)
	(if (queue-align by) (queue-move by) (queue-play)))

(defun song-failed (why)
	;a song that could not be played, and why. Come to by going through
	;the queue, the next that way is played, as far as once round it, and
	;in Player mode not past its last, as at a song's end; picked, it
	;stops there
	(status why)
	(setq *failed* (inc *failed*))
	(setq *skip_due* (cond
		((not (and *skip_by* (nempty? *queue*))) :nil)
		((>= *failed* (length *queue*)) :nil)
		((and (not *jukebox*) (> *skip_by* 0) (= *queue_at* (dec (length *order*)))) :nil)
		(:t))))

(defun skip-on ()
	;the next of the queue after one that failed, from the main loop: one
	;that fails at once has the next played by this loop, not by a call
	;in a call
	(while *skip_due*
		(setq *skip_due* :nil)
		(queue-move *skip_by*)))

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
				(song-failed (cat (first *current*) ": not a module it can read")))
			((eql kind "T")
				;the row and the samples shown again only when they change
				;of a seek before the last asked for, not shown
				(bind '(& id heard order row state &rest playing) words)
				(setq *failed* 0)
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
				;all of it heard: the next of the favourites in Jukebox mode,
				;round them, else of the list shown, stopped after its last
				(setq *skip_by* 1)
				(cond
					(*jukebox* (queue-step 1))
					((empty? (queue-modules)) (engine-send "S" *serial*))
					((not (queue-align 1)) (queue-play))
					((= *queue_at* (dec (length *order*))) (engine-send "S" *serial*))
					((queue-move 1)))))))

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
			:jukebox :nil :shuffled :nil :list_at +tab_all :browse (list) :charts (list))))
	(setq *favourites* (. *config* :find :favourites) *jukebox* (. *config* :find :jukebox)
		*shuffled* (. *config* :find :shuffled) *list_at* (. *config* :find :list_at)
		*browse* (. *config* :find :browse)
		;the charts' folder, and what was playing, none in a config kept
		;before they were
		*charts* (ifn (. *config* :find :charts) (list))
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
		:list_at *list_at* :browse *browse* :charts *charts* :queue *queue* :order *order* :queue_at *queue_at*
		:playing (if entry (list (first entry) (second entry) (if *current* (/ *heard* +rate) 0) state)))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; the list, and fetching
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-path ()
	; (list-path) -> path
	;the folders gone into of the list shown, Modland's or the charts'
	(case *list_at* (+tab_charts *charts*) (+tab_all *browse*) (:t (list))))

(defun list-base ()
	(if (= *list_at* +tab_charts) +modland_pub +modland))

(defun path-url (path)
	; (path-url path) -> url
	;the address of the folder of the list shown, or of its playlist, which
	;has no / after it
	(defq url (apply (const cat) (cat (list (list-base)) (map (# (cat (url-encode %0) "/")) path))))
	(if (and (nempty? path) (ends-with ".m3u" (last path))) (slice url 0 -2) url))

(defun listing-entries (html url path)
	; (listing-entries html url path) -> ((title url folder) ...)
	;the entries of a folder's listing, a folder's the name it is gone into
	;by: Startrekker AM's instruments, kept beside its modules, not shown,
	;the charts' favourites only of the trackers played here, and a
	;playlist a folder
	(reduce (lambda (out (name folder))
			(defq title (entry-title name))
			(cond
				((ends-with ".nt" name))
				;a playlist, or a folder of them, of nothing played here
				((find (cat url name) *empty*))
				((and (= (length path) 1) (eql (first path) "favourites")
					(not (find title +modland_kinds))))
				((ends-with ".m3u" name) (push out (list (slice title 0 -5) (cat url name) title)))
				((push out (list title (cat url name) (if folder title)))))
			out)
		(listing-read html) (list)))

(defun list-entries ()
	; (list-entries) -> ((title url folder) ...)
	;the favourites, or the folder of Modland or of the charts shown, its
	;listing or playlist kept once fetched, the trackers' folders or the
	;charts' at the top. One kept is shown, and fetched again behind it the
	;first time since the app was opened, if nothing else is coming
	(cond
		((= *list_at* +tab_favourites) (map (# (list (first %0) (second %0) :nil)) *favourites*))
		((= *list_at* +tab_search) *found*)
		((empty? (defq path (list-path)))
			(map (# (list %0 (cat (list-base) (url-encode %0) "/") %0))
				(if (= *list_at* +tab_all) +modland_kinds +chart_places)))
		((defq text (load (kept-path (defq url (path-url path)))))
			(unless (or *fetching* (find url *fresh*)) (fetch-start :refresh url "the folder again"))
			(if (ends-with ".m3u" url)
				(playlist-entries url (playlist-read text))
				(listing-entries text url path)))
		(:t (fetch-start :listing url) (list))))

(defun list-show (&optional keep)
	;a button a row, as many as the filter leaves, to so many
	;the filter a regexp, ignoring case, or plain text if it is not one.
	;Kept, where the list was seen from stays
	(defq pattern (get :clear_text *filter*) found (query pattern :nil :t :t))
	(unless (second found) (setq found (query pattern :nil :nil :t)))
	(bind '(engine meta &ignore) found)
	(setq *entries* (filter (# (or (eql pattern "") (. engine :match? (to-lower (first %0)) meta)))
		(list-entries)))
	(. *where* :set_text (cond
		((= *list_at* +tab_favourites) (cat (str (length *favourites*)) " favourites"))
		((= *list_at* +tab_search)
			(cat "Search of all of Modland, " (str (length *entries*))
				(if (> *found_total* (length *found*)) (cat " shown of " (str *found_total*) " found") " found")))
		((empty? (defq path (list-path)))
			(if (= *list_at* +tab_all) "All of Modland played here, by tracker" "Charts, Modland's favourites and playlists"))
		((cat (if (= *list_at* +tab_all) "All / " "Charts / ")
			(join (map (# (if (ends-with ".m3u" %0) (slice %0 0 -5) %0)) path) " / ") ", "
			(str (length *entries*)) " here"))))
	;from the top, or with the folder gone up from in the middle
	(. *list* :set_count (length *entries*))
	(unless keep
		(def (get :vslider *list_scroll*) :value
			(if (defq at (and *left* (some (# (and (third %0) (eql (third %0) *left*) (!))) *entries*)))
				(* (max 0 (- at (/ +list_rows 2))) (get :item_h *list*)) 0)))
	(.-> *list_scroll* :layout :dirty_all))

(defun fetch-ended (msg)
	;= and its bytes, or ! and why not
	(bind '(kind url title missing) *fetching*)
	(defq ok (starts-with "=" msg))
	(setq *fetching* :nil msg (slice msg 1 -1))
	(cond
		((eql kind :refresh)
			;a listing fetched again: shown again only if it changed and is
			;still what is shown, and not tried again till the app is
			;opened again, if it could not be had
			(push *fresh* url)
			(unless *song* (status "Nothing playing"))
			(when (and ok (nql msg (load (kept-path url))))
				(save msg (kept-path url))
				(and (/= *list_at* +tab_favourites) (eql url (path-url (list-path))) (list-show :t))))
		((and (not ok) (eql kind :module) (starts-with "404" msg) (starts-with +modland url))
			;not at Modland where it was: the index asked where it is now
			(index-ask url title))
		((eql kind :index)
			;where the index has it now, or its folder looked in for it
			;under another name, if it has none or that is where it was
			(defq at (rfind "/" url))
			(if (and (nempty? msg) (nql (defq now (modland-url msg)) url))
				(fetch-start :module now title)
				(fetch-start :renamed (slice url 0 at) title (slice url at -1))))
		;its folder gone too, the artist's renamed say, tanker!!!.mod of RAW #1
		((and (not ok) (eql kind :renamed) (starts-with "404" msg))
			(song-failed (cat title ": Modland has it no more")))
		((and (not ok) (find kind '(:module :renamed))) (song-failed (cat "Could not fetch it: " msg)))
		((not ok) (status (cat "Could not fetch it: " msg)))
		((eql kind :renamed)
			(save msg (kept-path url))
			(cond
				((defq name (entry-renamed msg missing))
					(fetch-start :module (cat url name) title))
				;not in a folder of work with another, looked for in the
				;artist's own, the folder it is in
				((defq up (coop-parent url))
					(fetch-start :renamed up title missing))
				((song-failed (cat title ": Modland has it no more")))))
		((eql kind :listing)
			(save msg (kept-path url))
			(push *fresh* url)
			(unless *song* (status "Nothing playing"))
			(list-show))
		((not (song-start msg title url)) (song-failed (cat title ": not a module it can read")))))

(defun playlist-entries (url entries)
	; (playlist-entries url entries) -> ((title url folder) ...)
	;the modules of a playlist Modland has, where it has them now, as the
	;index, ./index.lisp, says: all of them, as the playlist has them, till
	;it has said, which it is asked once
	(defq now (. *resolved* :find url))
	(cond
		((and now (= (length now) (length entries)))
			(defq out (list))
			(each (lambda (& path) (if (nql path "-") (push out (list (path-name path) (modland-url path) :nil))))
				entries now)
			out)
		(:t (unless (find url *resolving*)
				(push *resolving* url)
				(mail-send *indexer* (cat "R" url "\n" (join (map (# (modland-path (second %0))) entries) "\n"))))
			(map (# (list (first %0) (second %0) :nil)) entries))))

(defun playlist-resolved (text)
	;where the modules of a playlist are now, as the index has said, kept,
	;and shown if that playlist is
	(defq lines (split text "\n") url (first lines))
	(. *resolved* :insert url (rest lines))
	(setq *resolving* (filter (# (nql %0 url)) *resolving*))
	(and (= *list_at* +tab_charts) (eql url (path-url (list-path))) (list-show :t)))

(defun search-ask ()
	;all of Modland searched by the index, ./index.lisp, the names that
	;start with the search's text, as the filter leaves them: of those
	;asked, only the last is answered
	(setq *search_id* (inc *search_id*))
	(mail-send *indexer* (cat "S" (str *search_id*) "\n:" (get :clear_text *search*) "\n:" (get :clear_text *filter*))))

(defun search-found (msg)
	;what the last search found, shown if the search is
	(defq lines (split msg "\n") head (split (first lines) " "))
	(when (= (str-as-num (slice (first head) 1 -1)) *search_id*)
		(setq *found_total* (str-as-num (second head))
			*found* (map! (# (list %0 (modland-url %0) :nil)) (list lines) 1 (length lines)))
		(if (= *list_at* +tab_search) (list-show))))

(defun index-ask (url title)
	;where a module not where it was is now, asked of the index, ./index.lisp,
	;which answers on the fetcher's mailbox, as a fetch of its own
	(setq *fetching* (list :index url title :nil) *fetch_id* (inc *fetch_id*))
	(status (cat "Looking for " title " ..."))
	(mail-send *indexer* (cat "L" (str *fetch_id*) " " (modland-path url))))

(defun fetch-done (msg)
	;what the fetcher said, of the last fetch asked of it only
	(defq kind (first msg) gap (ifn (find " " msg) (length msg)) rest (slice msg (min (length msg) (inc gap)) -1))
	(cond
		;where a playlist's modules are now, of the index, not a fetch
		((eql kind "R") (playlist-resolved (slice msg 1 -1)))
		;what a search of all of Modland found, of the index
		((eql kind "S") (search-found msg))
		;the playlists and their folders with nothing to play, of the index
		((eql kind "E")
			(setq *empty* (rest (split msg "\n")))
			(and (= *list_at* +tab_charts) (nempty? (list-path)) (list-show :t)))
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

(defun item-pressed (at)
	;a favourite played, a folder opened, or a module of Modland played,
	;the modules of the list what next and prev go through
	(bind '(title url folder) (elem-get *entries* at))
	(cond
		(folder (setq *left* :nil) (push (list-path) folder) (config-save) (list-show))
		(:t (player-mode url)
			(setq *skip_by* :nil)
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
		(if (= *list_at* +tab_favourites) (list-show))))

(defun favourite-remove ()
	(when (and *current* (favourite? (defq url (second *current*))))
		(setq *favourites* (filter (# (nql (second %0) url)) *favourites*))
		(unless (local? url) (pii-remove (kept-path url)))
		(config-save)
		(favourite-show)
		(if (= *list_at* +tab_favourites) (list-show))))

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
	(list-init)
	(if (= *list_at* +tab_search) (search-ask))
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
				(mail-send *fetcher* "K")
				(mail-send *indexer* "K"))
			((= idx +select_engine) (engine-said msg))
			((= idx +select_tip) (. *window* :tip msg))
			((= idx +select_fetch) (fetch-done msg))
			((= idx +select_picker)
				(setq *picker* :nil)
				(unless (eql (defq file (trim msg)) "")
					(player-mode file)
					(setq *skip_by* :nil)
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
				(setq *picker* (file-pick (elem-get *select* +select_picker) "Open module..." "." ".mod .s3m .xm .it .med .mmd0 .mmd1 .mmd2 .mmd3 .mmdc .ahx .hvl" :nil "mods" :t (list))))
			((= id +event_mode)
				;the jukebox plays the favourites
				(setq *jukebox* (= (. *mode* :get_selected) 1))
				(config-save)
				(setq *skip_by* 1)
				(and *jukebox* (nempty? *favourites*) (queue-start *favourites* 0)))
			((= id +event_shuffle)
				(setq *shuffled* (first (. *shuffle* :get_states)))
				(config-save))
			((= id +event_tab)
				(setq *list_at* (. *tab* :get_selected))
				(config-save)
				(if (= *list_at* +tab_search) (search-ask))
				(list-show))
			((= id +event_search)
				;what is typed searched, on the search's tab
				(unless (= *list_at* +tab_search)
					(setq *list_at* +tab_search)
					(. *tab* :set_selected *list_at*)
					(config-save))
				(search-ask)
				(list-show))
			((= id +event_up)
				;the folder gone up from marked, and in view
				(when (nempty? (defq path (list-path)))
					(setq *left* (pop path))
					(config-save) (list-show)))
			;on the search's tab what it found is filtered by the index, of all
			;there are, not only the first shown
			((= id +event_filter)
				(if (= *list_at* +tab_search) (search-ask))
				(list-show))
			((= id +event_seek)
				(setq *seek_at* (pii-time))
				;a drag sends many, only the last is played from: more of the
				;window's waiting, this is not it
				(and *song* (not (mail-poll (list (elem-get *select* +select_main))))
					(engine-send "G" *serial* " " (setq *seek_id* (inc *seek_id*)) " " (get :value *seek*))))
			((and (= id +event_item) (= (getf msg +ev_msg_type) +ev_type_action)
					(defq button (. *window* :find_id (getf msg +ev_msg_action_source_id))))
				(item-pressed (get :entry_at button)))
			((. *window* :event msg)))
		(skip-on))
	(config-save)
	(if *picker* (mail-send *picker* ""))
	(engine-send "Q")
	(mail-send *fetcher* "Q")
	(mail-send *indexer* "Q")
	(setq service :nil)
	(. *window* :close))
