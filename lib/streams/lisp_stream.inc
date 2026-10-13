;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lisp-stream, a stream that is read from Lisp functions, or written to one
;
;	(lisp-stream fill [seek write]) -> stream
;
; fill is a function of no arguments that gives the next of it, a str of
; any length, or :nil, or an empty str, at the end. It is asked when what
; it gave last has been read, and not before. The stream keeps the one str.
;
; seek, if there is one, is a function of (offset whence), whence as
; (stream-seek) has it, 0 from the start, 1 from where it is, 2 from the
; end, that gives where it is from now, a number, and has fill give what
; is there next. From where it is, is from what fill has given: what of
; that has not been read yet is taken from offset before seek is called.
; With no seek, or one that gives no number, (stream-seek) is -1.
;
; So anything Lisp can get a piece at a time can be read by what reads a
; stream, (read-line) (read-blk) (read-char) (lines!) and the rest, and by
; the VP that does: an object of a store, a file of a volume of our own.
;
; A function has only what it is given. What fill and seek need to know is
; put in them as they are made:
;
;	(defq state (list ...))
;	(lisp-stream `(lambda () (,my-fill ',state))
;		`(lambda (offset whence) (,my-seek ',state offset whence)))
;
; write, if there is one, makes it a stream to write to and not to read: a
; function of (data flush). What is written is kept till there is a buffer
; of it, and write is given that, a str, and :nil. When the stream is
; flushed, (stream-flush), it is given what there is, which may be nothing,
; and :t. A stream that is let go of is flushed first, as a file is, so
; write is called then too, from wherever the last of it was let go. fill
; can give :nil, and a seek that is :nil is none:
;
;	(lisp-stream '(lambda () :nil) :nil
;		`(lambda (data flush) (,my-write ',state data flush)))
;
; It is here and not in root.inc so that a tree with this in it can be
; built by a boot image that has not: the binding is made when this is
; imported, by when the function is there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "class/lstream/lisp_create" lisp-stream)
; (lisp-stream fill [seek write]) -> stream
