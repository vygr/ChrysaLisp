;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The herd's part of a sync service. A tree that is exactly at its
; waypoint offers it to every other machine that takes a sync, as the
; herd would, never as one of the machines they name: one at an older
; waypoint, or with the same tree and none noted, takes it, made exactly
; the same, and offers it on in turn. One at a change of its own, with a
; .git, or at the same or a newer waypoint refuses it, a message each.
;
; Started by the service, and told by it when to offer: as it starts,
; when it has taken a waypoint, when a machine that takes a sync is new
; to it, and when this machine's waypoint is noted. One task does them
; one at a time, so two are never sent to a machine at once, and what
; asks while it is at one is done by the next.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/sync/sync.inc")

(defun main ()
	;the name of the service, and the root of its tree
	(bind '(name root) (split (mail-read (task-mbox)) "\n"))
	(defq kept (sync-kept root)
		me (hex-encode (system-id)) running :t select (list (task-mbox)))
	(while running
		;an empty message is to stop, any other to offer, and all that came
		;while one was being offered are one more
		(defq msg (mail-read (task-mbox)))
		(while (mail-poll select)
			(if (eql (mail-read (task-mbox)) "") (setq msg "")))
		(cond
			((eql msg "") (setq running :nil))
			((sync-clean root kept)
				(defq rules_text (sync-rules-text root))
				(each (lambda ((svc id machine &ignore))
						;a host with no modes for its files, there or here, can not
						;say it is at a waypoint, so it is not one of the herd
						(unless (or (eql id me) (sync-no-modes? machine))
							(sync-push svc root rules_text :nil :t kept :nil :t)))
					(sync-services name))))))
