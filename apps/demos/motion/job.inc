;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What the Motion app and its children, ./child.lisp, say to each other: a
; job of a frame, or of a scout of a scene, the answer to each, and what
; the scout sends the child that draws. They start where the farm's end,
; lib/task/jobs.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")

(enums +motion_kind 0
	(enum frame scout))

;the scene by its number and seed; the step the app last had, and the
;one to seek to, -1 for none; the key of the canvas's pixels and its
;size; where the pointer is in the picture; how long since the last
;frame in us; playing or not; is it a film, a scene with an end that is
;sought and keeps I-frames; and for a scout the mailbox of the child that
;draws, where its I-frames go
(structure +motion_job +job_size
	(long seed step target shared)
	(fixed px py)
	(uint kind scene dt width height playing film)
	(netid iframes))

;the step it is at, or for a scout how many steps the scene is, or how
;many of them it has run so far, more to come; how many figures; the
;colour round the picture; how long the tick and the draw took in us; and
;of the child that draws the mailbox I-frames are to be sent to
(structure +motion_reply +job_reply_size
	(long step)
	(uint figures paper tick draw more)
	(netid iframes))

;what a scout sends the child that draws, at the head of each I-frame, as
;longs on its stream: which scene it is of, by number and seed, which of
;the scout's I-frames it is, 0 the first, as mail comes in any order, and
;the step it is at. Then the I-frame, lib/motion/iframe.inc
