(import "lib/media/module.inc")
(import "lib/media/tracker.inc")

;The measurer of the Jukebox's player, a child of it the kernel puts where
;it will, that finds how long a song is, and where each of its orders
;starts, which a seek goes from: a long one takes seconds, so the player
;plays it at once and this tells the window and the player when it knows,
;a part at a time with the node's other tasks between.
;
;Told by the player, once: the window's mailbox, the player's, then the
;song's number, a line, and the module's bytes. It tells the window, as
;the player does, and the player:
;	I serial frames				the song is so long
;	W serial frames start...	and its orders start so, seven numbers
;								each, (tracker-start)
;and it goes when told anything more, the player on to another song

(defq +rate 44100 +part 10000 +orphan 5000000)

(defun main ()
	;the line ends after the mailboxes, which are bytes, a newline among
	;them now and then: the first newline of all left a window with no
	;song's length, one in ten or so
	(when (and (defq msg (mail-read-timeout (task-mbox) +orphan))
			(defq end (find "\n" msg (* 2 +net_id_size)))
			(defq song (module-read (slice msg (inc end) -1))))
		(defq app (slice msg 0 +net_id_size) player (slice msg +net_id_size (* 2 +net_id_size))
			serial (slice msg (* 2 +net_id_size) end) measure (tracker-measure song +rate) frames :nil)
		(while (and (not frames) (not (mail-poll (list (task-mbox)))))
			(setq frames (tracker-measure-step measure +part))
			(task-slice))
		(when frames
			(mail-send app (cat "I" serial " " (str frames)))
			(mail-send player (cat "W" serial " " (str frames) " "
				(join (map (# (join (map (const str) %0) " ")) (. measure :find :starts)) " "))))))
