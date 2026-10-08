(report-header "Audio and clipboard: a task has the service of its own node")
(import "service/audio/app.inc")

;each desktop has an audio service, on its node. An app is given the one on
;the node it is on, so one desktop that quits does not take the sound of
;another. Stood in for here by a service of the name that plays nothing
(defq as_before (audio-service) as_key (mail-declare (task-mbox) "@Audio" "a test"))
(assert-eq "the service on this node is the one had" (task-mbox) (audio-service))
(assert-eq "it is on this node" (slice (task-mbox) +mailbox_id_size -1)
	(slice (audio-service) +mailbox_id_size -1))
(mail-forget as_key)
(assert-eq "and when it has gone, what there was before" as_before (audio-service))

;the clipboard is found the same way
(import "service/clipboard/app.inc")
(defq as_before (clip-service) as_key (mail-declare (task-mbox) "@Clipboard" "a test"))
(assert-eq "the clipboard service on this node is the one had" (task-mbox) (clip-service))
(mail-forget as_key)
(assert-eq "and when it has gone, what there was before" as_before (clip-service))

(undef (env) 'as_before 'as_key)
