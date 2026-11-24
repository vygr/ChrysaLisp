;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The user's Jukebox favourites, played for another app: music behind a
; demo or a game, the user's own.
;
; It is the Jukebox's own player, ./player.lisp, a task on another node,
; driven with no window, as the Jukebox's window drives it: a song given
; with a mailbox new for it, played, held, and told the app is there, or
; it goes. The favourites are those of the user's jukebox.tre, shuffled,
; and only those kept, which the Jukebox does when one is pinned, so they
; play offline; one that is not, or is not a module, is passed over.
;
; The app gives two of its mailboxes, by their slots in its select list:
; one for the word that the player has started, (:launched), and one the
; song's, which is made new for each song, (:heard). It calls (:tick)
; once a second.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/files/names.inc")
(import "lib/net/url.inc")
(import "service/lock/app.inc")
(import "apps/media/jukebox/app.inc")

(defq +favourites_player "apps/media/jukebox/player.lisp"
	+favourites_config "jukebox.tre")

(defun favourite-kept (url)
	; (favourite-kept url) -> path
	;where the Jukebox keeps a module, a file or an address's, as its
	;(kept-path) has it
	(if (starts-with "https://" url) (url-kept-path "downloads/mods" url :t) url))

(defclass Favourites (select task_slot song_slot) :nil
	; (Favourites select task_slot song_slot) -> favourites
	(def this :select select :task_slot task_slot :song_slot song_slot
		:player :nil :waiting (list) :songs (list) :at 0 :on :nil :title :nil)

	(defmethod :load ()
		; (. favourites :load) -> num
		;the user's favourites that are kept, shuffled; how many
		(defq file (cat (get :home *env*) +favourites_config)
			config (with-read-lock file (and (defq stream (file-stream file)) (tree-load stream)))
			songs (if config (ifn (. config :find :favourites) (list)) (list)))
		(def this :at 0 :songs (shuffle (filter (# (> (age (favourite-kept (second %0))) 0)) songs)))
		(length (get :songs this)))

	(defmethod :send (&rest words)
		; (. favourites :send word ...) -> favourites
		;the player told, with the song's mailbox, or kept till it is up
		(defq msg (apply (const cat) (cat (list (elem-get (get :select this) (get :song_slot this)))
				(map (# (if (num? %0) (str %0) %0)) words))))
		(if (defq id (get :player this)) (mail-send id msg) (push (get :waiting this) msg))
		this)

	(defmethod :start ()
		; (. favourites :start) -> favourites
		;the music on: the player started, on a node of this machine not
		;this one if there is another, and a song given it
		(when (and (not (get :on this)) (nempty? (get :songs this)))
			(def this :on :t)
			(unless (get :player this)
				(defq me (task-nodeid) others (filter (# (not (eql %0 me))) (lisp-nodes :t)))
				(open-task +favourites_player (if (nempty? others) (elem-get others (random (length others))) me)
					+kn_call_pin 0 (elem-get (get :select this) (get :task_slot this))))
			(. this :play))
		this)

	(defmethod :stop ()
		; (. favourites :stop) -> favourites
		;the music off, the player told to go
		(when (get :on this)
			(. this :send "Q")
			(def this :on :nil :player :nil :waiting (list) :title :nil))
		this)

	(defmethod :launched (msg)
		; (. favourites :launched msg) -> favourites
		;the player has started, or could not on that node, and is asked of
		;this one; what was kept for it is sent
		(defq id (getf msg +kn_msg_reply_id))
		(cond
			((not (get :on this)) (if (/= (get-long id 0) 0) (mail-send id (cat (elem-get (get :select this) (get :song_slot this)) "Q"))))
			((/= (get-long id 0) 0)
				(def this :player id)
				(each (# (mail-send id %0)) (get :waiting this))
				(def this :waiting (list)))
			((open-task +favourites_player (task-nodeid) +kn_call_pin 0
				(elem-get (get :select this) (get :task_slot this)))))
		this)

	(defmethod :play ()
		; (. favourites :play) -> favourites
		;the song it is at given the player, under a mailbox new for it, so
		;what is said of the one before goes nowhere; one that can not be
		;read passed over
		(raise :songs :select :song_slot)
		(defq tries (length songs))
		(while (> tries 0)
			(bind '(title url) (elem-get songs (get :at this)))
			(cond
				((defq data (name-load (favourite-kept url)))
					(elem-set select song_slot (mail-mbox))
					(def this :title title)
					(. this :send "M" "p 0\n" data)
					(setq tries 0))
				(:t (def this :at (% (inc (get :at this)) (length songs)))
					(setq tries (dec tries)))))
		this)

	(defmethod :move (by)
		; (. favourites :move by) -> favourites
		;the next song, or the one before, round from the end
		(when (get :on this)
			(raise :songs)
			(def this :at (% (+ (get :at this) by (length songs)) (length songs)))
			(. this :play))
		this)

	(defmethod :heard (msg)
		; (. favourites :heard msg) -> favourites
		;what the player says of the song: all of it heard, or not a module,
		;and the next is played; what it says of where it is, not wanted
		(if (find (first msg) '("E" "!")) (. this :move 1))
		this)

	(defmethod :tick ()
		; (. favourites :tick) -> favourites
		;once a second: the player told the app is still there
		(if (get :on this) (. this :send "K"))
		this)

	(defmethod :title ()
		; (. favourites :title) -> :nil | str
		;what is playing
		(if (get :on this) (get :title this))))
