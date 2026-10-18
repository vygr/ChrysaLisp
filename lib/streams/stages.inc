;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stages, a pipeline of tasks on this node, a stream to a stream each
;
; (stage imports fn [arg ...]) -> stage
; (stages-load stream pixmap type stages) -> :t | :nil
; (stages-save pixmap type stream stages) -> :t
; (stages-run stages [done]) -> stages
;
; How an image is read and written: a CPM's rle and lz4, a PNG's chunks,
; inflate and filters, a JPEG's scan. A stage is a task of its own, made
; from a form, (fn in out arg ...), with what it imports. It reads one
; stream and writes another and knows nothing of what is either side of
; it, so no whole frame is ever held between two of them.
;
; stages-load: the stream is read by the first, and what the last writes
; is read into the pixmap, as type, by a task at the end. stages-save: a
; task at the start writes the pixmap, as type, and the last writes the
; stream.
;
; The tasks are pinned to this node, as the stream and the pixmap are
; objects of this node, passed to the task at an end by a weak reference.
; The chain is made from the end its data goes to: each task, once it has
; an in stream, says where its mailbox is, and that is where the one
; before it writes.
;
; An arg goes into the form as it is, so a number or a string is itself,
; a list is quoted by the caller, and bytes that are not text are given
; as (hex-decode ...) of them, a reader takes no byte as it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun stage (imports fn &rest args)
	; (stage imports fn [arg ...]) -> stage
	;imports a path or a list of them
	(list (if (str? imports) (list imports) imports) fn args))

(defun stage-mbox (mbox)
	;a mailbox as a form
	(list 'hex-decode (hex-encode mbox)))

(defun stage-in (handshake)
	;an in stream whose mailbox the stage gives the one before it
	(list 'progn (list 'mail-send (stage-mbox handshake) '(in-mbox (defq in (in-stream)))) 'in))

(defun stage-form (stage in out &optional done)
	;the source of a stage's task. in and out are each a form, done the
	;mailbox told it has finished, if any. No function of this module is
	;called in a backquote: it is looked for as the form is made, when the
	;module's names are gone
	(bind '(imports fn args) stage)
	(str (cat (list 'progn) (map (lambda (path) (list 'import path)) imports)
		(list (cat (list fn in out) args))
		(if done (list (list 'mail-send (stage-mbox done) :t)) (list)))))

(defun stages-load (stream pixmap type stages)
	; (stages-load stream pixmap type stages) -> :t | :nil
	(defq handshake (mail-mbox) reply (stage-mbox handshake))
	;the end, the pixmap read from what the last stage writes, and if it
	;could be
	(open-child (str `(progn (import "gui/pixmap/lisp.inc")
			(mail-send ,reply (in-mbox (defq pixmap (obj-ref ,(weak-ref pixmap)) in (in-stream))))
			(mail-send ,reply (str (weak-ref (pixmap-read pixmap in ,type))))))
		+kn_call_pin)
	;the stages from the last to the first, the first reading the stream
	(each! (lambda (s)
			(defq out (list 'out-stream (stage-mbox (mail-read handshake))))
			(open-child (stage-form s (if (= (!) 0) (list 'obj-ref (weak-ref stream)) (stage-in handshake)) out)
				+kn_call_pin))
		(list stages) -1 0)
	(nql (mail-read handshake) (const (str :nil))))

(defun stages-save (pixmap type stream stages)
	; (stages-save pixmap type stream stages) -> :t
	(defq handshake (mail-mbox) last_at (dec (length stages)))
	;the stages from the last to the first, the last writing the stream
	;and saying when it is done
	(each! (lambda (s)
			(open-child (if (= (!) last_at)
					(stage-form s (stage-in handshake) (list 'obj-ref (weak-ref stream)) handshake)
					(stage-form s (stage-in handshake) (list 'out-stream (stage-mbox (mail-read handshake)))))
				+kn_call_pin))
		(list stages) -1 0)
	;the start, the pixmap written
	(defq to (stage-mbox (mail-read handshake)))
	(open-child (str `(progn (import "gui/pixmap/lisp.inc")
			(pixmap-write (pixmap-as-argb (obj-ref ,(weak-ref pixmap))) (out-stream ,to) ,type)))
		+kn_call_pin)
	(mail-read handshake)
	:t)

(defun stages-run (stages &optional done)
	; (stages-run stages [done]) -> stages
	;a pipeline whose ends are stages too: the first a source, made with no
	;in stream, :nil, the last a sink, with no out stream, :nil. Sound is
	;so, a microphone at one end, a speaker or a file at the other. It runs
	;by itself, done, if given, is sent :t when the sink has finished
	(defq handshake (mail-mbox) last_at (dec (length stages)))
	(each! (lambda (s)
			(open-child (stage-form s
					(if (= (!) 0) :nil (stage-in handshake))
					(if (= (!) last_at) :nil (list 'out-stream (stage-mbox (mail-read handshake))))
					(if (= (!) last_at) done))
				+kn_call_pin))
		(list stages) -1 0)
	stages)

;module
(export-symbols '(stage stage-mbox stages-load stages-save stages-run))
(env-pop)
