(import "lib/media/module.inc")
(import "lib/media/tracker.inc")

;The measurer of the Jukebox's player, a child of it the kernel puts where
;it will, that finds how long a song is: a long one takes seconds, so the
;player plays it at once and this tells the window when it knows, a part
;at a time with the node's other tasks between.
;
;Told by the player, once: the window's mailbox, then the song's number, a
;line, and the module's bytes. It tells the window, as the player does:
;	I serial frames		the song is so long
;and it goes when told anything more, the player on to another song

(defq +rate 44100 +part 10000 +orphan 5000000)

(defun main ()
	(when (and (defq msg (mail-read-timeout (task-mbox) +orphan))
			(defq end (find "\n" msg))
			(defq song (module-read (slice msg (inc end) -1))))
		(defq app (slice msg 0 +net_id_size) serial (slice msg +net_id_size end)
			measure (tracker-measure song +rate) frames :nil)
		(while (and (not frames) (not (mail-poll (list (task-mbox)))))
			(setq frames (tracker-measure-step measure +part))
			(task-slice))
		(if frames (mail-send app (cat "I" serial " " (str frames))))))
