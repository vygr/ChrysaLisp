;;;;;;;;;;;;;;;
; file previews
;;;;;;;;;;;;;;;

(import "lib/files/files.inc")
(import "lib/files/names.inc")

;the ways a file can be shown small, each (test make). A previewer,
;apps/system/files/previewers/, says what it is for as it is read,
;(file-previewer), so neither the file picker nor a chat need be changed
;for a new kind: a new file there is enough. They are all read, last of
;all, by this
(defq *file_previewers* (list) *file_previewers_last* (list))

(defun file-previewer (test make &optional last)
	; (file-previewer test make [last])
	;files (test file) is true of are shown by (make file most_w [most_h])
	;-> view | :nil. Those said last, as one for any text, are asked only
	;after all the others, so that one for a kind is asked first
	(push (if last *file_previewers_last* *file_previewers*) (list test make)))

(defun file-preview (file most_w &optional most_h)
	; (file-preview file most_w [most_h]) -> view | :nil
	;the file shown, no wider than most_w nor higher than most_h, by the
	;first previewer for it that can, :nil if none
	(some (lambda ((test make)) (if (test file) (make file most_w most_h)))
		(cat *file_previewers* *file_previewers_last*)))

(defun canvas-fit (canvas most_w &optional most_h)
	; (canvas-fit canvas most_w [most_h]) -> canvas
	;the canvas, or a smaller copy of it, (. canvas :resize), no wider than
	;most_w, nor higher than most_h if given. Its pixels are to be there
	;still, not yet sent on to be drawn, and the copy is not sent on
	(bind '(w h) (. canvas :pref_size))
	(defq nw w nh h)
	(when (> nw most_w) (setq nh (max 1 (/ (* nh most_w) nw)) nw most_w))
	(and most_h (> nh most_h) (setq nw (max 1 (/ (* nw most_h) nh)) nh most_h))
	(if (or (/= nw w) (/= nh h))
		(. (Canvas nw nh 1) :resize canvas)
		canvas))

(defun file-picture (path most_w &optional most_h)
	; (file-picture path most_w [most_h]) -> :nil | canvas
	;the picture of a file, if it is of a kind the canvas reads, made smaller
	;by (. canvas :resize) to be no wider than most_w, nor higher than most_h
	;if given, as a preview or a picture in a chat shows it
	;loaded with its pixels kept, not sent on to be drawn yet, so that a
	;smaller one can be made from them: what is sent on is the one shown
	(and (canvas-loader-of path) (defq canvas (canvas-load path +load_flag_noswap :nil (name-whole-stream path)))
		(. (canvas-fit canvas most_w most_h) :swap 0)))

(defun file-lines (path &optional n)
	; (file-lines path [n]) -> :nil | (line ...)
	;the lines of a file of text, the first n if given, read so far only;
	;:nil if there is no such file, or it is not text, a 0 byte in it
	(when (defq stream (if n (name-stream path) (name-whole-stream path)))
		(defq text (ifn (read-blk stream (if n 16384 (ifn (name-size path) 0))) ""))
		(unless (find (ascii-char 0) text)
			(defq lines (split text (ascii-char 10)))
			(if (and n (> (length lines) n)) (slice lines 0 n) lines))))

;the previewers there are, each says what it is for
(defq _ (env))
(each (# (import %0 _)) (files-all "apps/system/files/previewers" '(".inc")))
(undef (env) '_)
