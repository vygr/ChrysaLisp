;the recorder of tests/system/test_storage_record.lisp, kept as a recorder
;is kept: each member asked again each second, as (storage-record-run)
;does, for a member tells one only so long as it has asked of late. A
;slow machine with a lot to do took longer than that between the test's
;steps, and what was done was told to nobody. It is given the recorder's
;mailbox and the members, and on a mail of stop each is told to tell that
;recorder no more, answered, before it says it has stopped. It stops too
;when they have gone, a test that ended before it was told
(import "service/storage/app.inc")

(defun main ()
	(bind '(said box &rest svcs) (map (const hex-decode) (split (mail-read (task-mbox)) " ")))
	(defq stop :nil)
	(until (or stop (notevery (const storage-there?) svcs))
		(each (# (storage-send %0 +storage_type_record (hex-encode box))) svcs)
		(setq stop (mail-read-timeout (task-mbox) (task-timeout 1))))
	(when stop
		(each (# (storage-record-ask %0 box :t)) svcs)
		(mail-send said "stopped")))
