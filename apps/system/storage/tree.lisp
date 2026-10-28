;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a folder of the host put in a store and read back, by a farm
;
; A task of its own, started by the Storage app, so that the app goes on
; drawing while it runs. It is mailed, a line each: who to tell, the
; folder, what to put before each path, and the members of the store,
; where each is. Every file under the folder is a job, (Jobs), for a farm
; of workers on the nodes of this machine, ./tree_child.lisp, two for each
; member: the file is put, as a stream, at one of the members, each in
; turn, so every member is written to at once, as a store is by the many
; that use it. When all are put, each is a job again, to be read back as
; a stream, at the member it was put at, and compared with the file. obj,
; deps and folders whose names begin with a dot are not gone in to.
;
; Chris: "Would/Could it work using a Farm of workers each putting in a
; file as jobs ? like any other jobs demo ?" and "farm across ALL the
; members !" A put of one path is nothing to a put of another: a block is
; named by what is in it, and a path is set by itself.
;
; It says how it is going, every few files, "put" or "read", how many of
; how many, and the path, and at the end "done" and: how many were put,
; how many read back the same, of how many, how many bytes, and how long
; each took, ms. Any mail to it and it stops.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "service/storage/app.inc")
(import "lib/files/files.inc")

(enums +select 0
	(enum main launch reply timer))

;how often the farm is looked over, and how long a job may be: a film of
;tens of megabytes is one
(defq +look_rate 1000000 +job_timeout 120000000 +lf "\n")

(defun main ()
	(defq select (task-mboxes +select_size))
	(bind '(reply root prefix members) (split (mail-read (task-mbox)) +lf))
	(defq reply (hex-decode reply) svcs (map (const hex-decode) (split members " "))
		;not what the tests make and take away as they run: a file that was
		;there when the folder was listed and gone when its turn came was the
		;one of 2036 that was not put, the first time this ran over three
		;machines
		files (filter (# (not (or (starts-with "./deps/" %0) (starts-with "./tests/scratch/" %0)
				(starts-with "." (last (split %0 "/"))))))
			(files-all root '("")))
		count (length files) going :t putting :t done 0 put 0 same 0 bytes 0 t0 (pii-time) t1 t0
		want (max 2 (* 2 (length svcs)))
		jobs (Jobs "apps/system/storage/tree_child.lisp" (elem-get select +select_launch)
			(elem-get select +select_reply) (list want want 0)))
	(defun work (what)
		;a job for each file, each at the next of the members
		(. jobs :add (map (lambda (file)
			(cat (str-alloc +job_size) (join (list what (hex-encode (elem-get svcs (% (!) (length svcs))))
				(cat prefix (if (starts-with "./" file) (slice file 2 -1) file)) file) +lf)))
			files)))
	(cond
		((or (empty? svcs) (= count 0))
			(mail-send reply (join (map (const str) (list "done" 0 0 count 0 0 0)) " ")))
		(:t (work "p")
			(mail-timeout (elem-get select +select_timer) +look_rate 0)
			(while going
				(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
				(case idx
					;anything said to it, and it stops
					(+select_main (setq going :nil))
					(+select_launch (. jobs :launched msg))
					(+select_reply
						(defq job (. jobs :job msg) said (slice msg +job_reply_size -1))
						(when (defq out (. jobs :answered msg))
							(++ done)
							(cond
								((starts-with "0" said)
									;which one, and why: it is said, a count does not
									(if job (mail-send reply (cat (if putting "notput " "notread ")
										(third (split (slice job +job_size -1) +lf)) +lf (slice said 2 -1)))))
								(putting (++ put))
								(:t (++ same)
									(setq bytes (+ bytes (str-as-num (slice said 2 -1))))))
							(and job (= 0 (% done 8))
								(mail-send reply (cat (if putting "put " "read ") (str done) " " (str count) " "
									(third (split (slice job +job_size -1) +lf)))))
							(when (= out 0)
								(cond
									(putting (setq putting :nil done 0 t1 (pii-time))
										(work "v"))
									(:t (mail-send reply (join (map (const str)
											(list "done" put same count bytes (/ (- t1 t0) 1000) (/ (- (pii-time) t1) 1000))) " "))
										(setq going :nil))))))
					(:t ;one of the farm that has gone, or been too long, is started again
						(mail-timeout (elem-get select +select_timer) +look_rate 0)
						(. jobs :refresh +job_timeout))))
			(mail-timeout (elem-get select +select_timer) 0 0)))
	(. jobs :close))
