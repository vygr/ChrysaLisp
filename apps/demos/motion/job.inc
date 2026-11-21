;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What the Motion app and its child, ./child.lisp, say to each other: a job
; of a frame, or of the length of a scene, and the answer to each. They
; start where the farm's end, lib/task/jobs.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")

(enums +motion_kind 0
	(enum frame length))

;the scene by its number and seed; the step the app last had, and the
;one to seek to, -1 for none; the key of the canvas's pixels and its
;size; where the pointer is in the picture; how long since the last
;frame in us; playing or not; and is it a film, a scene with an end
;that is sought and keeps I-frames
(structure +motion_job +job_size
	(long seed step target shared)
	(fixed px py)
	(uint kind scene dt width height playing film))

;the step it is at, or for a length job how many steps the scene is;
;how many figures; the colour round the picture; how long the tick and
;the draw took in us
(structure +motion_reply +job_reply_size
	(long step)
	(uint figures paper tick draw))
