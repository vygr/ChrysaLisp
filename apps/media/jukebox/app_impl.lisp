(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "apps/system/files/picker.inc")
(import "lib/files/names.inc")
(import "lib/media/module.inc")
(import "lib/text/searching.inc")
(import "./app.inc")
(import "./library.inc")
(import "./widgets.inc")

;A player of the modules of the trackers, the Amiga's MODs, MEDs, AHXs and HVLs, Scream
;Tracker's S3Ms, FastTracker's XMs and Impulse Tracker's ITs, and a jukebox
;of the user's favourites. A song is played by ./player.lisp, a task on
;another node, so what draws the screen is not slowed by it; it says what
;is heard, and this shows it: the row, the samples, and the bars of its
;spectrum. The modules come from a file, a store, the favourites, kept to
;play offline, or Modland, ./library.inc

;the mailboxes it reads: the window's, a timer, a tip, the file picker's,
;the word that a task of its own has started, and what its tasks say:
;the player of the song, the fetcher of what is being fetched, the index
;of a search, and of what it was not asked. The player's, the fetcher's
;and the search's are new ones for each song and seek, each fetch and
;each search, so what is said of one no longer wanted goes nowhere
(enums +select 0
	(enum main timer tip picker task engine fetch search index))

;its tasks, each on a node of this machine not this one, if there is
;another: the player, ./player.lisp, the fetcher, ./fetch.lisp, and the
;index, ./index.lisp
(enums +task 0
	(enum engine fetcher indexer))

;the lists, by their tabs
(enums +tab 0
	(enum favourites charts all search))

(defq +alive 1000000 +rows_wide 136
	+task_files ''("player.lisp" "fetch.lisp" "index.lisp")
	;how long a task may say nothing of what it is owed before it is taken
	;to have gone, to start with: the player says 30 times a second, a fetch
	;as it comes, the index makes itself of all of Modland's names first.
	;Doubled for each time it is started again and has still said nothing
	+task_patience ''(8000000 20000000 60000000)
	;each task's mailbox, :nil till it says it has started, what is for it
	;kept till then, since when it has owed an answer, :nil if it owes
	;none, its patience now, and when the window last said it was there
	*tasks* (list :nil :nil :nil) *waiting* (list (list) (list) (list)) *owed* (list :nil :nil :nil)
	*patience* (list 8000000 20000000 60000000) *alive_at* 0
	;the favourites, Player or Jukebox, shuffle, which list and the folder
	;of Modland and of the charts, as they were when last closed,
	;(config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "jukebox.tre")
	*favourites* (list) *jukebox* :nil *shuffled* :nil *list_at* +tab_all *browse* (list) *charts* (list)
	;the song shown, the mailbox that is its name to the player, when the
	;seek bar was last dragged, its bytes, how long it is, and what the
	;player last said: heard, where, played, held or stopped, and the
	;samples that sound
	*song* :nil *song_mbox* :nil *seek_at* 0 *data* :nil *length* 0
	*heard* 0 *place* :nil *state* "s" *playing* (list) *shown* :nil
	;till when what went wrong is left in the status line, (notice)
	*notice_until* 0
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
	;the list shown is to be shown again once what is being fetched has come
	*list_due* :nil
	;how many times what is being fetched has been asked for again, its
	;fetcher gone, and the listings fetched again since the app was opened
	*fetch_tries* 0 *fresh* (list)
	;of each playlist where its modules are now, as the index says,
	;./index.lisp, (url now ...), and those asked
	*resolved* (Fmap 31) *resolving* (list)
	;the playlists and folders of them with nothing played here, of the index
	*empty* (list)
	;the search of all of Modland, ./index.lisp: what it found, and how many
	;there are of which those are the first
	*found* (list) *found_total* 0
	*levels* (map! (lambda () 0) '() 0 +bars) *peaks* (map! (lambda () 0) '() 0 +bars))

(defun status (text)
	(. *status* :set_text text))

(defun notice (text)
	; (notice text)
	;what went wrong, shown for a few seconds: a song that plays says where
	;it is 30 times a second, and would be over it at once
	(status text)
	(setq *notice_until* (+ (pii-time) 4000000)))

(defun local? (url)
	(not (starts-with "https://" url)))

(defun favourite? (url)
	(some (# (eql (second %0) url)) *favourites*))

(defun task-start (which)
	; (task-start which)
	;a task of the app's started, on a node of this machine not this one if
	;there is another. It is started by a message: the word that it has
	;comes to the select list, (task-launched), and what is for it is kept
	;till then, (task-send)
	(defq me (task-nodeid) others (filter (# (not (eql %0 me))) (lisp-nodes :t)))
	(elem-set *tasks* which :nil)
	(open-task (cat *app_root* (elem-get +task_files which))
		(if (nempty? others) (elem-get others (random (length others))) me)
		+kn_call_pin which (elem-get *select* +select_task)))

(defun task-launched (msg)
	; (task-launched msg)
	;a task has said it has started: what was kept for it is sent. One that
	;a node could not start is asked of this one
	(defq which (getf msg +kn_msg_key) id (getf msg +kn_msg_reply_id))
	(cond
		((/= (get-long id 0) 0)
			(elem-set *tasks* which id)
			(each (# (mail-send id %0)) (elem-get *waiting* which))
			(elem-set *waiting* which (list)))
		((not (and (>= (length msg) +kn_msg_reply_size) (eql (getf msg +kn_msg_reply_node) (task-nodeid))))
			(open-task (cat *app_root* (elem-get +task_files which)) (task-nodeid)
				+kn_call_pin which (elem-get *select* +select_task)))))

(defun task-send (which reply &rest words)
	; (task-send which reply word ...)
	;a task told: the mailbox it is to answer on, then the words, sent, or
	;kept till it says it has started
	(defq msg (apply (const cat) (cat (list reply) (map (# (if (num? %0) (str %0) %0)) words))))
	(cond
		((defq id (elem-get *tasks* which)) (mail-send id msg))
		;that the window is there is not kept for one not yet started
		((nql (first words) "K") (push (elem-get *waiting* which) msg))))

(defun task-asked (which)
	; (task-asked which)
	;a task owes the window an answer from now
	(elem-set *owed* which (pii-time)))

(defun task-heard (which &optional more)
	; (task-heard which [more])
	;a task has been heard from: its patience is as it was to start with,
	;and it owes nothing, or more from now
	(elem-set *owed* which (if more (pii-time)))
	(elem-set *patience* which (elem-get +task_patience which)))

(defun engine-send (kind)
	; (engine-send kind)
	;the player told, of the song shown
	(if *song_mbox* (task-send +task_engine *song_mbox* kind)))

;;;;;;;;;;;;;;;;;
; what is shown
;;;;;;;;;;;;;;;;;

(defun rows-channels ()
	; (rows-channels) -> num
	;as many of the song's channels as fit across its rows, each a cell as
	;its tracker shows one, lib/media/module.inc
	(min (. *song* :find :channels) (/ (- +rows_wide 4) (module-cell-width *song*))))

(defun rows-show (order row)
	;the rows of the pattern about the one heard, it in the middle, as many
	;channels as fit. An order that is no pattern, a marker a song starts
	;with or none at all, has no rows
	(defq channels (. *song* :find :channels) half (/ +rows_shown 2)
		orders (. *song* :find :orders) patterns (. *song* :find :patterns)
		pattern (if (< order (length orders)) (elem-get orders order) -1)
		there (<= 0 pattern (dec (length patterns)))
		cells (if there (elem-get patterns pattern))
		rows (if there (elem-get (. *song* :find :rows) pattern) 0)
		shown (rows-channels) lines (list))
	(each! (lambda ()
			(defq r (+ row (- (!) half)))
			(push lines (if (<= 0 r (dec rows))
				(apply (const cat) (cat (list (byte-to-hex-str r) (if (= r row) "> " "  "))
					(map! (lambda ()
							(defq c (* (+ (* r channels) (!)) +cell_size))
							(cat (module-cell-text *song* (slice cells c (+ c +cell_size))) " "))
						'() 0 shown)))
				"")))
		'() 0 +rows_shown)
	(. *rows* :load lines 0 0 -1 -1))

(defun samples-show ()
	;the samples of the song, a line each, those a channel plays now marked
	(. *samples* :load (map (lambda (sample)
			(cat (if (find (str (!)) *playing*) "*" " ") (byte-to-hex-str (inc (!))) " " (. sample :find :name)))
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
	(. (view-variant btn :paper (if (and *current* (favourite? (second *current*))) :paper_highlight :paper_toolbar))
		:dirty))

(defun song-heard ()
	; (song-heard) -> frames
	;where the song is heard, no further than its length once that is known
	(if (> *length* 0) (min *heard* *length*) *heard*))

(defun song-status ()
	;a song with no title of its own by the name of its file. Not over what
	;went wrong, till that has been shown a while
	(defq title (trim (. *song* :find :title)))
	(if (>= (pii-time) *notice_until*)
		(status (cat (if (eql title "") (first *current*) title) ", " (. *song* :find :kind) " of "
			(str (. *song* :find :channels)) " channels, "
			(if *place* (cat "order " (str (inc (first *place*))) " of " (str (length (. *song* :find :orders))) ", ") "")
			(time-as-clock (/ (song-heard) +rate)) " of " (if (> *length* 0) (time-as-clock (/ *length* +rate)) "?")
			(cond ((eql *state* "h") ", paused") ((eql *state* "s") ", stopped") (""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the player, ./player.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun song-send (state seconds)
	; (song-send state seconds)
	;the song shown given the player, to play, hold or stop at so many
	;seconds, under a mailbox new for it: its name to the player, and where
	;the player tells of it, so what it says of the song before goes nowhere
	(elem-set *select* +select_engine (setq *song_mbox* (mail-mbox)))
	(if (eql state "p") (task-asked +task_engine) (task-heard +task_engine))
	(task-send +task_engine *song_mbox* "M" state " " seconds " m\n" *data*))

(defun song-seek (seconds)
	; (song-seek seconds)
	;the song played from there, the player to tell of it on a new mailbox
	;from then: what it said before the seek goes nowhere, and is not taken
	;for where the song is after it
	(elem-set *select* +select_engine (defq reply (mail-mbox)))
	(task-asked +task_engine)
	;where it is heard from now, as the window has it till the player says:
	;a player started again plays from there
	(setq *heard* (* seconds +rate) *state* "p")
	(task-send +task_engine *song_mbox* "G" reply seconds))

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
			*place* :nil *shown* :nil *playing* (list) *state* state)
		(def (. *seek* :dirty) :maximum 0 :value seconds)
		(song-send state seconds)
		;as many columns as it has channels, as fit
		(def *rows* :vdu_width (+ 4 (* (module-cell-width song) (rows-channels))))
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

(defun fetch-send ()
	; (fetch-send)
	;what is being fetched asked of the fetcher, or of the index where a
	;module is now, to answer on a mailbox new for it: what is said of a
	;fetch before goes nowhere
	(elem-set *select* +select_fetch (defq reply (mail-mbox)))
	(bind '(kind url &ignore) *fetching*)
	(cond
		((eql kind :index)
			(task-asked +task_indexer)
			(task-send +task_indexer reply "L" (modland-path url)))
		(:t (task-asked +task_fetcher)
			(task-send +task_fetcher reply "F" url))))

(defun fetch-start (kind url &optional title missing)
	;a module or a listing fetched by the fetcher, the window drawn
	;meanwhile, and of a folder looked in for a module not found, its
	;name. Each has a number, what is said of one before is not heard
	;a listing still coming is put off by it, and is asked for again after, (list-due)
	(and *fetching* (eql (first *fetching*) :listing) (nql (second *fetching*) url) (setq *list_due* :t))
	(setq *fetching* (list kind url title missing) *fetch_tries* 0)
	(loading 0)
	(status (cat "Fetching " (ifn title "the folder") " ..."))
	(fetch-send))

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

(defun song-failed (why)
	;a song that could not be played, and why. Come to by going through
	;the queue, the next that way is played, as far as once round it, and
	;in Player mode not past its last, as at a song's end; picked, it
	;stops there
	(notice why)
	(setq *failed* (inc *failed*))
	(setq *skip_due* (cond
		((not (and *skip_by* (nempty? *queue*))) :nil)
		((>= *failed* (length *queue*)) :nil)
		((and (not *jukebox*) (> *skip_by* 0) (= *queue_at* (dec (length *order*)))) :nil)
		(:t))))

(defun entry-play (entry)
	;an entry played, (title url): a file, a favourite kept, or fetched
	(bind '(title url) entry)
	(setq *marked* url)
	(list-visible)
	(cond
		;a file, or an object of a store, store:path, lib/files/names.inc
		((defq data (name-load (if (local? url) url (kept-path url))))
			(unless (song-start data title url) (song-failed (cat title ": not a module it can read"))))
		((local? url) (song-failed (cat url ": not there")))
		(:t ;what played stops, and the window says what is coming, till it
			;has come
			(engine-send "S")
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

(defun skip-on ()
	;the next of the queue after one that failed, from the main loop: one
	;that fails at once has the next played by this loop, not by a call
	;in a call
	(while *skip_due*
		(setq *skip_due* :nil)
		(queue-move *skip_by*)))

(defun engine-said (msg)
	;what the player said of the song, on the song's mailbox or that of its
	;last seek: of no other song, and of no time before that seek
	(defq kind (first msg) words (split (slice msg 1 (ifn (find "|" msg) (length msg))) " "))
	(when *song*
		(cond
			((eql kind "I")
				(setq *length* (str-as-num (first words)))
				(def (. *seek* :dirty) :maximum (/ *length* +rate))
				(song-status))
			((eql kind "!")
				(task-heard +task_engine)
				(setq *song* :nil)
				(song-failed (cat (first *current*) ": not a module it can read")))
			((eql kind "T")
				;the row and the samples shown again only when they change
				(bind '(heard order row state &rest playing) words)
				(setq *failed* 0 *heard* (str-as-num heard) *state* state)
				;while it plays it is owed the next of these
				(task-heard +task_engine (eql state "p"))
				(unless (eql *shown* (defq place (cat order " " row)))
					(setq *shown* place *place* (list (str-as-num order) (str-as-num row)))
					(rows-show (first *place*) (second *place*)))
				(unless (eql (join *playing* " ") (join playing " "))
					(setq *playing* playing)
					(samples-show))
				(bars-show (map (const str-as-num) (split (slice msg (inc (find "|" msg)) -1) " ")))
				;the bar left to the hand that drags it, a second after the last
				(and (> (- (pii-time) *seek_at*) 1000000)
					(/= (get :value *seek*) (defq at (/ (song-heard) +rate)))
					(def (. *seek* :dirty) :value at))
				(song-status))
			((eql kind "E")
				;all of it heard: the next of the favourites in Jukebox mode,
				;round them, else of the list shown, stopped after its last
				(task-heard +task_engine)
				(setq *skip_by* 1)
				(cond
					(*jukebox* (queue-step 1))
					((empty? (queue-modules)) (engine-send "S"))
					((not (queue-align 1)) (queue-play))
					((= *queue_at* (dec (length *order*))) (engine-send "S"))
					((queue-move 1)))))))

;;;;;;;;;
; config
;;;;;;;;;

(defun config-default ()
	;the first time: no favourites, the player, the top of All, nothing in
	;the queue and nothing playing
	(scatter (Emap) :version +config_version :favourites (list) :jukebox :nil :shuffled :nil
		:list_at +tab_all :browse (list) :charts (list) :queue (list) :order (list) :queue_at 0
		:playing :nil))

(defun config-load ()
	;the defaults with what was kept laid over them, so a key added since it
	;was kept has its default, and one no longer used is passed over. A file
	;of a later version than this, or none, gives the defaults,
	;docs/ai_digest/app_configuration.md
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val)))))
	(setq *favourites* (. *config* :find :favourites) *jukebox* (. *config* :find :jukebox)
		*shuffled* (. *config* :find :shuffled) *list_at* (. *config* :find :list_at)
		*browse* (. *config* :find :browse) *charts* (. *config* :find :charts)
		*queue* (. *config* :find :queue) *order* (. *config* :find :order)
		*queue_at* (. *config* :find :queue_at) *resume* (. *config* :find :playing))
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
				;it answers on the mailbox of what it is not asked a fetch or a
				;search of, with the playlist's address: nothing of it goes stale
				(task-send +task_indexer (elem-get *select* +select_index)
					"R" url "\n" (join (map (# (modland-path (second %0))) entries) "\n")))
			(map (# (list (first %0) (second %0) :nil)) entries))))

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
		;its listing fetched: one already coming is not asked for again, and
		;behind a module that is coming it waits its turn, (list-due)
		(:t (cond
				((or (not *fetching*) (eql (first *fetching*) :refresh)) (fetch-start :listing url))
				((not (and (eql (first *fetching*) :listing) (eql (second *fetching*) url))) (setq *list_due* :t)))
			(list))))

(defun list-show (&optional keep)
	;a button a row, as many as the filter leaves, to so many
	;the filter a regexp, ignoring case, or plain text if it is not one.
	;Kept, where the list was seen from stays
	(defq leaves (name-filter (get :clear_text *filter*)))
	(setq *entries* (filter (# (name-filter? leaves (first %0))) (list-entries)))
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

(defun list-due ()
	;the list shown again, and so its listing fetched, once what was coming
	;has come, if it had to wait for it or was put off by it
	(when (and *list_due* (not *fetching*))
		(setq *list_due* :nil)
		(list-show :t)))

(defun index-ask (url title)
	;where a module not where it was is now, asked of the index, ./index.lisp,
	;as a fetch of its own
	(setq *fetching* (list :index url title :nil) *fetch_tries* 0)
	(status (cat "Looking for " title " ..."))
	(fetch-send))

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
			(if *song* (song-status) (status "Nothing playing"))
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
		((not ok) (notice (cat "Could not fetch it: " msg)))
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
			(if *song* (song-status) (status "Nothing playing"))
			(list-show))
		((not (song-start msg title url)) (song-failed (cat title ": not a module it can read")))))

(defun playlist-resolved (text)
	;where the modules of a playlist are now, as the index has said, kept,
	;and shown if that playlist is
	(defq lines (split text "\n") url (first lines))
	(. *resolved* :insert url (rest lines))
	(setq *resolving* (filter (# (nql %0 url)) *resolving*))
	(and (= *list_at* +tab_charts) (eql url (path-url (list-path))) (list-show :t)))

(defun search-ask ()
	;all of Modland searched by the index, ./index.lisp, the names that
	;start with the search's text, as the filter leaves them, to answer on
	;a mailbox new for it: what an earlier search found goes nowhere
	(elem-set *select* +select_search (defq reply (mail-mbox)))
	(task-asked +task_indexer)
	(task-send +task_indexer reply "S" "\n:" (get :clear_text *search*) "\n:" (get :clear_text *filter*)))

(defun search-found (msg)
	;what the search found, S and how many, then a path a line, shown if
	;the search is
	(defq lines (split msg "\n"))
	(task-heard +task_indexer)
	(setq *found_total* (str-as-num (slice (first lines) 1 -1))
		*found* (map! (# (list %0 (modland-url %0) :nil)) (list lines) 1 (length lines)))
	(if (= *list_at* +tab_search) (list-show)))

(defun index-said (msg)
	;what the index says that is of no fetch or search: where a playlist's
	;modules are now, R, and the playlists and their folders with nothing
	;to play, E
	(case (first msg)
		("R" (playlist-resolved (slice msg 1 -1)))
		("E" (setq *empty* (rest (split msg "\n")))
			(and (= *list_at* +tab_charts) (nempty? (list-path)) (list-show :t)))))

(defun fetch-said (msg)
	;what the fetcher said of what is being fetched, or the index of where
	;a module is now: how far it has got, %, its bytes, =, why not, !, or
	;where, L
	(defq kind (first msg) said (slice msg 1 -1) which (if (eql kind "L") +task_indexer +task_fetcher))
	(when *fetching*
		(cond
			((eql kind "%")
				;how far it has got, of how many, or of a megabyte if it is
				;not said: and it is owed the rest from now
				(task-heard which :t)
				(bind '(got total) (map (const str-as-num) (split said " ")))
				(loading (min 1000 (/ (* got 1000) (if (> total 0) total (max got 1048576))))))
			(:t (task-heard which)
				(loading :nil)
				(fetch-ended (cat kind said))))))

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
			;the row picked by its address: two of a list may have one name
			(defq modules (queue-modules))
			(queue-start modules (ifn (some (# (if (eql (second %0) url) (!))) modules) 0)))))

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

;;;;;;;;;;;;
; its tasks
;;;;;;;;;;;;

(defun task-again (which)
	; (task-again which)
	;a task taken to have gone: it is started again, twice as patient, and
	;given again what it owed. A courtesy to the one before, if it is there
	;after all: it is told to go
	(if (defq id (elem-get *tasks* which)) (mail-send id (cat (elem-get *select* +select_main) "Q")))
	(elem-set *waiting* which (list))
	(elem-set *owed* which :nil)
	(elem-set *patience* which (* 2 (elem-get *patience* which)))
	(task-start which)
	(case which
		(+task_engine
			;the song from where it was heard to
			(if *song* (song-send *state* (/ *heard* +rate))))
		(+task_fetcher
			;what was being fetched asked for again, three times at most
			(and *fetching* (nql (first *fetching*) :index)
				(cond
					((< (setq *fetch_tries* (inc *fetch_tries*)) 3) (fetch-send))
					(:t (loading :nil)
						(fetch-ended "!no answer")))))
		(+task_indexer
			(and *fetching* (eql (first *fetching*) :index) (fetch-send))
			(if (= *list_at* +tab_search) (search-ask)))))

(defun tasks-alive ()
	; (tasks-alive)
	;each second the tasks are told the window is there, a task that hears
	;nothing for a while goes, ./app.inc. One that has said nothing of what
	;it owes for its patience is taken to have gone, and is started again;
	;and all of them if the window itself was not here to say so in time,
	;as they will have gone
	(defq now (pii-time) late (and (> *alive_at* 0) (> (- now *alive_at*) +orphan)))
	(each (lambda (since patience)
			(if (or late (and since (> (- now since) patience))) (task-again (!))))
		*owed* *patience*)
	(setq *alive_at* now)
	;and where to tell it what it did not ask for
	(each! (# (task-send (!) (elem-get *select* +select_index) "K")) '() 0 +task_size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the actions, ./actions.inc has which event and key each is of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-play ()
	;played, as the window has it till the player says: a player that has
	;gone is started again to play it
	(when *song*
		(setq *state* "p")
		(task-asked +task_engine))
	(engine-send "P"))

(defun action-pause ()
	(engine-send "H"))

(defun action-toggle ()
	;played if stopped, else held, or played on if held
	(if (eql *state* "s") (action-play) (action-pause)))

(defun action-stop ()
	(engine-send "S"))

(defun action-next ()
	(queue-step 1))

(defun action-prev ()
	(queue-step -1))

(defun action-pin ()
	;a favourite no longer, or one now
	(if (and *current* (favourite? (second *current*))) (favourite-remove) (favourite-add)))

(defun action-open ()
	(if *picker* (mail-send *picker* ""))
	(setq *picker* (file-pick (elem-get *select* +select_picker) "Open module..." "." +module_exts :nil "mods" :t (list))))

(defun action-picked (file)
	;a file picked, or none: played by hand
	(setq *picker* :nil)
	(unless (eql (setq file (trim file)) "")
		(player-mode file)
		(setq *skip_by* :nil)
		(entry-play (list (path-name file) file))))

(defun action-mode ()
	;the jukebox plays the favourites
	(setq *jukebox* (= (. *mode* :get_selected) 1))
	(config-save)
	(setq *skip_by* 1)
	(and *jukebox* (nempty? *favourites*) (queue-start *favourites* 0)))

(defun action-shuffle ()
	(setq *shuffled* (first (. *shuffle* :get_states)))
	(config-save))

(defun action-tab ()
	(setq *list_at* (. *tab* :get_selected))
	(config-save)
	(if (= *list_at* +tab_search) (search-ask))
	(list-show))

(defun action-search ()
	;what is typed searched, on the search's tab
	(unless (= *list_at* +tab_search)
		(setq *list_at* +tab_search)
		(. *tab* :set_selected *list_at*)
		(config-save))
	(search-ask)
	(list-show))

(defun action-up ()
	;the folder gone up from marked, and in view
	(when (nempty? (defq path (list-path)))
		(setq *left* (pop path))
		(config-save)
		(list-show)))

(defun action-filter ()
	;on the search's tab what it found is filtered by the index, of all
	;there are, not only the first shown
	(if (= *list_at* +tab_search) (search-ask))
	(list-show))

(defun action-seek ()
	;a drag sends many, only the last is played from: more of the window's
	;waiting, this is not it
	(setq *seek_at* (pii-time))
	(and *song* (not (mail-waiting? (elem-get *select* +select_main)))
		(song-seek (get :value *seek*))))

(defun action-item ()
	;a row of the list pressed
	(and (= (getf *msg* +ev_msg_type) +ev_type_action)
		(defq button (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		(item-pressed (get :entry_at button))))

(import "./actions.inc")

(defun main ()
	(setq *select* (task-mboxes +select_size))
	;one of it on this machine, ./app.lisp: what is sent to it is not read
	(defq service (mail-declare (mail-mbox) "@Jukebox" "Jukebox Service 1.0") *running* :t)
	(def *window* :tip_mbox (elem-get *select* +select_tip))
	(config-load)
	;the player, the fetcher and the index, each by a message
	(each! (# (task-start (!))) '() 0 +task_size)
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
	(while *running*
		(defq *msg* (mail-read (elem-get *select* (defq idx (mail-select *select*)))))
		(cond
			((= idx +select_timer)
				(mail-timeout (elem-get *select* +select_timer) +alive 0)
				(tasks-alive))
			((= idx +select_task) (task-launched *msg*))
			((= idx +select_engine) (engine-said *msg*))
			((= idx +select_tip) (. *window* :tip *msg*))
			((= idx +select_fetch) (fetch-said *msg*))
			((= idx +select_search) (search-found *msg*))
			((= idx +select_index) (index-said *msg*))
			((= idx +select_picker) (action-picked *msg*))
			;must be +select_main
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*)))
		(skip-on)
		(list-due))
	(config-save)
	(if *picker* (mail-send *picker* ""))
	;a courtesy, each would go of itself
	(each! (# (task-send (!) (elem-get *select* +select_main) "Q")) '() 0 +task_size)
	(setq service :nil)
	(. *window* :close))
