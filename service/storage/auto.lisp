;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, this machine's store started by itself
;
; The Net service starts this when its session becomes the machine's member
; of the mesh, the one that has the port, and tells it to go when it is that
; no longer. If the machine was made a member of a store, (storage-join),
; the store is started here, and stopped when this is told to go. If it
; never was, there is nothing to do but wait to be told.
;
; It looks again now and then: a machine may be made a member while this
; session is up, in a session that is not the member, and then it is this
; one that is to serve the store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(enums +select 0
	(enum main timer))

(defq +auto_look 10000000)

(defun main ()
	(defq select (task-mboxes +select_size) running :t svc :nil image :nil)
	(while running
		;started if it is to be and is not
		(and (not svc) (defq said (storage-member))
			(if (defq had (storage-service))
				;it is up already, in this session: asked for by hand
				(setq svc had image (. said :find :image))
				(when (setq svc (storage-join (. said :find :image) (. said :find :size)
						(. said :find :id) (. said :find :copies)))
					(setq image (. said :find :image)))))
		(mail-timeout (elem-get select +select_timer) +auto_look 0)
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(mail-timeout (elem-get select +select_timer) 0 0)
		(if (= idx +select_main) (setq running :nil)))
	;told to go: the store is another session's to serve now
	(when svc
		(storage-stop svc)
		(storage-unclaim image)))
