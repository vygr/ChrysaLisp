;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Music behind an app with no Jukebox window: a demo's or a game's, the
; user's Jukebox favourites, (music-favourites), or songs of the app's own.
;
; It is the Jukebox's own player, ./player.lisp, a task on another node of
; this machine, played quiet: it says only that a song has ended or is not
; a module, and the next is played. An app gives it three mailboxes of its
; select list, slot and the two after it, and hands it what comes to them,
; (:mail), the one line in its loop; it asks nothing else of the app, and
; keeps its player told that the app is there itself, as it must be or the
; player goes.
;
;	(enums +select 0
;		(enum main music music_song music_tick timer))
;	(defq music (Music select +select_music))
;	(. music :start)			;the user's favourites
;	(. music :start (list (list "Tune" "apps/mygame/data/tune.mod")))
;	...
;	(cond
;		((. music :mail idx msg))	;the music's
;		((= idx +select_main) ...))
;	...
;	(. music :stop)
;
; The mailbox a song is played under is made new for each song, in place of
; the one in that slot, so what the player says of the song before goes
; nowhere: the app reads its select list afresh each time, as it does.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/files/names.inc")
(import "lib/net/url.inc")
(import "service/lock/app.inc")
(import "apps/media/jukebox/app.inc")

(defq +music_player "apps/media/jukebox/player.lisp"
	+music_favourites "jukebox.tre"
	;how often the player is told the app is there, well within its
	;+orphan, ./app.inc
	+music_tick 1000000)

(defun music-path (url)
	; (music-path url) -> path
	;where a song is: a file's own path, or where the Jukebox keeps an
	;address's, as its (kept-path) has it
	(if (starts-with "https://" url) (url-kept-path "downloads/mods" url :t) url))

(defun music-favourites ()
	; (music-favourites) -> ((title url) ...)
	;the user's Jukebox favourites, those kept, which the Jukebox does when
	;one is pinned, so they play offline, shuffled
	(defq file (cat (get :home *env*) +music_favourites)
		config (with-read-lock file (and (defq stream (file-stream file)) (tree-load stream))))
	(shuffle (filter (# (> (age (music-path (second %0))) 0))
		(if config (ifn (. config :find :favourites) (list)) (list)))))

(defclass Music (select slot) :nil
	; (Music select slot) -> music
	;slot is the first of the three mailboxes of select it has, the player's
	;word that it has started, the song's and its own timer
	(def this :select select :slot slot :player :nil :launching :nil :waiting (list)
		:songs (list) :at 0 :on :nil :title :nil :unread 0)

	(defmethod :start (&optional songs)
		; (. music :start [songs]) -> :t | :nil
		;the music on, songs a list of (title path_or_url), else the user's
		;favourites; :nil if there are none. The player is started on a node
		;of this machine not this one if there is another
		(unless (get :on this)
			(def this :songs (ifn songs (music-favourites)) :at 0 :unread 0)
			(when (nempty? (get :songs this))
				(raise :select :slot)
				(def this :on :t)
				(unless (or (get :player this) (get :launching this))
					(def this :launching :t)
					(defq me (task-nodeid) others (filter (# (not (eql %0 me))) (lisp-nodes :t)))
					(open-task +music_player (if (nempty? others) (elem-get others (random (length others))) me)
						+kn_call_pin 0 (elem-get select slot)))
				(mail-timeout (elem-get select (+ slot 2)) +music_tick 0)
				(. this :play)))
		(get :on this))

	(defmethod :stop ()
		; (. music :stop) -> music
		;the music off, its player told to go
		(when (get :on this)
			(raise :select :slot)
			(if (defq id (get :player this)) (mail-send id (cat (elem-get select (inc slot)) "Q")))
			(mail-timeout (elem-get select (+ slot 2)) 0 0)
			(def this :on :nil :player :nil :waiting (list) :title :nil))
		this)

	(defmethod :move (by)
		; (. music :move by) -> music
		;the song after, or before, round from the end
		(when (get :on this)
			(raise :songs)
			(def this :at (% (+ (get :at this) by (length songs)) (length songs)))
			(. this :play))
		this)

	(defmethod :title ()
		; (. music :title) -> :nil | str
		;what is playing, :nil if the music is off
		(if (get :on this) (get :title this)))

	(defmethod :mail (idx msg)
		; (. music :mail idx msg) -> :t | :nil
		;mail of the app's select list, dealt with and :t if it came to one
		;of the music's mailboxes, else :nil and the app's
		(defq at (- idx (get :slot this)))
		(cond
			((= at 0) (. this :launched msg) :t)
			((= at 1) (. this :heard msg) :t)
			((= at 2) (. this :tick) :t)))

	(defmethod :send (word)
		; (. music :send word) -> music
		;the player told, with the song's mailbox, or kept till it is up
		(raise :select :slot)
		(defq msg (cat (elem-get select (inc slot)) word))
		(if (defq id (get :player this)) (mail-send id msg) (push (get :waiting this) msg))
		this)

	(defmethod :play ()
		; (. music :play) -> music
		;the song it is at given the player, quiet, under a mailbox new for
		;it, so what is said of the one before goes nowhere. One that can not
		;be read is passed over, and the music goes off once as many have
		;been, or were not modules, since one was heard to its end
		(raise :songs :select :slot)
		(defq data :nil)
		(while (and (not data) (< (get :unread this) (length songs)))
			(bind '(title url) (elem-get songs (get :at this)))
			(cond
				((setq data (name-load (music-path url)))
					(elem-set select (inc slot) (mail-mbox))
					(def this :title title)
					(. this :send (cat "Mp 0 q\n" data)))
				(:t (def this :at (% (inc (get :at this)) (length songs))
					:unread (inc (get :unread this))))))
		(unless data (. this :stop))
		this)

	(defmethod :launched (msg)
		; (. music :launched msg) -> music
		;the player has started, or could not on that node and is asked of
		;this one; what was kept for it is sent. One started for music
		;since turned off is told to go
		(defq id (getf msg +kn_msg_reply_id))
		(cond
			((= (get-long id 0) 0)
				(if (get :on this)
					(open-task +music_player (task-nodeid) +kn_call_pin 0
						(elem-get (get :select this) (get :slot this)))
					(def this :launching :nil)))
			((get :on this)
				(def this :player id :launching :nil)
				(each (# (mail-send id %0)) (get :waiting this))
				(def this :waiting (list)))
			(:t (def this :launching :nil)
				(mail-send id (cat (elem-get (get :select this) (inc (get :slot this))) "Q"))))
		this)

	(defmethod :heard (msg)
		; (. music :heard msg) -> music
		;the player says the song has all been heard, or is not a module, and
		;the next is played; the music off if none of them is
		(when (get :on this)
			(case (first msg)
				("E" (def this :unread 0) (. this :move 1))
				("!" (def this :unread (inc (get :unread this))) (. this :move 1))))
		this)

	(defmethod :tick ()
		; (. music :tick) -> music
		;its timer: the player told the app is still there, while it plays
		(when (get :on this)
			(mail-timeout (elem-get (get :select this) (+ (get :slot this) 2)) +music_tick 0)
			(if (get :player this) (. this :send "K")))
		this))
