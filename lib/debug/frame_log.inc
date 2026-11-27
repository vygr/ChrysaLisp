;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FrameLog, a frame log for tuning an app that draws frames: the time of
; each part of a frame, counts of what happened, and how the view turned,
; a line a second to a file, read as it grows:
;
;	(defq log (FrameLog "tests/scratch/app/frames.log" '("eye" "draws")))
;	(. log :frame) ... (. log :mark 0) ... (. log :mark 1)
;	(. log :add :stucks)
;	(. log :turn forward up)
;	(. log :line now rate (list "detail" detail) (list "gpu" (. log :ms took)
;		"nods" (. log :got :nods)))
;
; A line: the seconds since the first, the rate, the fields before the
; parts, " |" and each part's milliseconds a frame, then the fields after;
; a field a name and its value, or a str as it is, "|". Its counts are
; let go after each line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")
(import "lib/collections/fmap.inc")

;module
(env-push)

;how far the view turns before a turn back counts, and a second
(defq +frame_log_turn (n2d 0.01) +frame_log_second 1000000)

(defclass FrameLog (file parts) :nil
	; (FrameLog file parts) -> log
	;a log to a file, written from the first line, of the parts of a frame
	;named, in the order they end
	(def this :file file :parts parts :times (fill (nums) (length parts) 0) :at 0 :frames 0
		:stream :nil :began 0 :counts (Fmap 7) :was :nil :nod_way +double_0 :sway_way +double_0)

	(defmethod :frame ()
		; (. log :frame) -> log
		;a frame begun
		(def this :at (pii-time) :frames (inc (get :frames this)))
		this)

	(defmethod :mark (part)
		; (. log :mark part) -> log
		;part of the frame ended, its number in the parts
		(defq now (pii-time) times (get :times this))
		(elem-set times part (+ (elem-get times part) (- now (get :at this))))
		(def this :at now)
		this)

	(defmethod :add (name &optional n)
		; (. log :add name [n]) -> log
		;n more of a count, 1 if not said
		(defq counts (get :counts this))
		(. counts :insert name (+ (ifn (. counts :find name) 0) (ifn n 1)))
		this)

	(defmethod :got (name)
		; (. log :got name) -> num
		;a count so far this second
		(ifn (. (get :counts this) :find name) 0))

	(defmethod :turn (forward up)
		; (. log :turn forward up) -> log
		;how the view turned: :upsides, a frame upside down; :nods, a turn
		;from pitching up to down or back; :sways, from turning one way to the
		;other; each more than a little
		(if (< (second up) +double_0) (. this :add :upsides))
		(defq was (get :was this))
		(cond
			((not was) (def this :was (cat forward)))
			((> (sqrt (nums-dot (defq d (nums-sub forward was)) d)) +frame_log_turn)
				(bind '(fx fy fz) forward)
				(bind '(ux uy uz) up)
				(defq n (nums-dot d up) w (nums-dot d (doubles (- (* fy uz) (* fz uy)) (- (* fz ux) (* fx uz)) (- (* fx uy) (* fy ux)))))
				(if (< (* n (get :nod_way this)) +double_0) (. this :add :nods))
				(if (< (* w (get :sway_way this)) +double_0) (. this :add :sways))
				(def this :nod_way n :sway_way w :was (cat forward))))
		this)

	(defmethod :ms (us)
		; (. log :ms us) -> str
		;microseconds of the frames of this second as milliseconds a frame,
		;to a tenth
		(defq a (/ us (max (get :frames this) 1)))
		(cat (str (/ a 1000)) "." (str (/ (% a 1000) 100))))

	(defmethod :line (now rate head tail)
		; (. log :line now rate head tail) -> seconds
		;a second's line: when, the rate, the fields of head, the parts, the
		;fields of tail; its counts and times let go; the seconds since the
		;first line
		(unless (get :stream this)
			(def this :stream (file-stream (get :file this) +file_open_write) :began now))
		(defq out (list (cat (str (/ (- now (get :began this)) +frame_log_second)) "s " (str rate) " fps")))
		(each (# (push out (if (str? %0) %0 (cat (first %0) " " (str (second %0)))))) head)
		(when (nempty? (get :parts this))
			(push out "|")
			(each (lambda (name us) (push out (cat name " " (. this :ms us)))) (get :parts this) (get :times this)))
		(each (# (push out (if (str? %0) %0 (cat (first %0) " " (str (second %0)))))) tail)
		(write-line (get :stream this) (join out " "))
		(stream-flush (get :stream this))
		(def this :times (fill (nums) (length (get :parts this)) 0) :frames 0 :counts (Fmap 7))
		(/ (- now (get :began this)) +frame_log_second))

	(defmethod :quit (why)
		; (. log :quit why) -> :t
		;the last line, QUIT and why
		(when (defq stream (get :stream this))
			(write-line stream (cat "QUIT " why))
			(stream-flush stream))
		:t)
	)

;module
(export-classes '(FrameLog))
(env-pop)
