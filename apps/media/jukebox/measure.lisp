(import "lib/media/module.inc")
(import "lib/media/tracker.inc")
(import "./app.inc")

;The measurer of the Jukebox's player, a child of it the kernel puts where
;it will, that finds how long a song is, and where each of its orders
;starts, which a seek goes from: a long one takes seconds, so the player
;plays it at once and this tells it when it knows, a part at a time with
;the node's other tasks between.
;
;Told by the player, once: the mailbox to answer on, one of that song's
;alone, then the module's bytes. It answers:
;	W frames start...	the song is so long, and its orders start so,
;						seven numbers each, (tracker-start)
;and it goes when told anything more, the player on to another song, or
;if it is told nothing at all

(defq +part 10000)

(defun main ()
	(when (and (defq msg (mail-read-timeout (task-mbox) +orphan))
			(> (length msg) +net_id_size)
			(defq song (module-read (slice msg +net_id_size -1))))
		(defq player (slice msg 0 +net_id_size) measure (tracker-measure song +rate) frames :nil)
		(while (and (not frames) (not (mail-poll (list (task-mbox)))))
			(setq frames (tracker-measure-step measure +part))
			(task-slice))
		(if frames
			(mail-send player (cat "W" (str frames) " "
				(join (map (# (join (map (const str) %0) " ")) (. measure :find :starts)) " "))))))
