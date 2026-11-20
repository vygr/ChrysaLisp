;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tracker's module of whatever kind read into a song, which
; lib/media/tracker.inc plays: those with a tag of their own first, a MOD
; last, as one of the Soundtracker's has none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")
(import "lib/media/s3m.inc")

(defun module-read (data &optional bare)
	; (module-read data [bare]) -> :nil | song
	;bare, its samples' bytes not had, for what only shows it
	(or (s3m-read data bare) (mod-read data bare)))
