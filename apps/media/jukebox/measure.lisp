(import "lib/task/jobs.inc")
(import "lib/media/module.inc")
(import "lib/media/tracker.inc")
(import "./app.inc")

;The measurer of the Jukebox's player, the child of a farm of one of it,
;lib/task/jobs.inc, that the kernel puts on the node of this machine with
;least to do. It finds how long a song is, and where each of its orders
;starts, which a seek goes from: a long one takes seconds, so the player
;plays it at once and this tells it when it knows, a part at a time with
;the node's other tasks between.
;
;A job is the module's bytes. Its answer is the song's length in frames,
;then where its orders start, seven numbers each, (tracker-start), with a
;space between. It is left as soon as anything more comes, the player on
;to another song, and the child goes when told, or given nothing for a
;while

(defq +part 10000)

(defun job (msg mbox)
	(when (defq song (module-read (slice msg +job_size -1)))
		(defq measure (tracker-measure song +rate) frames :nil)
		(while (and (not frames) (not (mail-poll (list mbox))))
			(setq frames (tracker-measure-step measure +part))
			(task-slice))
		(if frames
			(mail-send (getf msg +job_reply)
				(cat (setf-> (str-alloc +job_reply_size) (+job_reply_key (getf msg +job_key)))
					(str frames) " " (join (map (# (join (map (const str) %0) " ")) (. measure :find :starts)) " "))))))

(defun main ()
	(job-child +orphan (const job)))
