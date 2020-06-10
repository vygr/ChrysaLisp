(include 'class/obj/class.inc)

(def-class 'stream 'obj)
(dec-method :vtable 'class/stream/vtable)
(dec-method :create 'class/stream/create :static '(r0 r1 r2 r3) '(r0))
(dec-method :init 'class/stream/init :static '(r0 r1 r2 r3 r4 r5))
(dec-method :available 'class/stream/available :static '(r0) '(r0 r1))
(dec-method :read_bits 'class/stream/read_bits :static '(r0 r1 r2 r3) '(r0 r1 r2 r3))
(dec-method :write_bits 'class/stream/write_bits :static '(r0 r1 r2 r3 r4) '(r0 r1 r2))
(dec-method :read_char 'class/stream/read_char :static '(r0) '(r0 r1))
(dec-method :read_line 'class/stream/read_line :static '(r0) '(r0 r1))
(dec-method :read 'class/stream/read :static '(r0 r1 r2) '(r0 r1))
(dec-method :write_char 'class/stream/write_char :static '(r0 r1) '(r0))
(dec-method :write 'class/stream/write :static '(r0 r1 r2) '(r0))
(dec-method :write_cstr 'class/stream/write_cstr :static '(r0 r1) '(r0))
(dec-method :skip 'class/stream/skip :static '(r0 r1) '(r0))
(dec-method :skip_not 'class/stream/skip_not :static '(r0 r1) '(r0))
(dec-method :split 'class/stream/split :static '(r0 r1) '(r0 r1))

(dec-method :deinit 'class/stream/deinit :override)

(dec-method :read_next 'class/stream/read_next :virtual '(r0) '(r0 r1))
(dec-method :write_next 'class/stream/write_flush :virtual '(r0) '(r0))
(dec-method :write_flush 'class/stream/write_flush :virtual '(r0) '(r0))

(dec-method :lisp_filestream 'class/stream/lisp_filestream :static '(r0 r1) '(r0 r1))
(dec-method :lisp_strstream 'class/stream/lisp_strstream :static '(r0 r1) '(r0 r1))
(dec-method :lisp_available 'class/stream/lisp_available :static '(r0 r1) '(r0 r1))
(dec-method :lisp_readchar 'class/stream/lisp_readchar :static '(r0 r1) '(r0 r1))
(dec-method :lisp_readline 'class/stream/lisp_readline :static '(r0 r1) '(r0 r1))
(dec-method :lisp_readavail 'class/stream/lisp_readavail :static '(r0 r1) '(r0 r1))
(dec-method :lisp_writechar 'class/stream/lisp_writechar :static '(r0 r1) '(r0 r1))
(dec-method :lisp_write 'class/stream/lisp_write :static '(r0 r1) '(r0 r1))
(dec-method :lisp_write_flush 'class/stream/lisp_write_flush :static '(r0 r1) '(r0 r1))
(dec-method :lisp_write_next 'class/stream/lisp_write_next :static '(r0 r1) '(r0 r1))

(def-struct 'stream 'obj)
	(ptr 'object 'buffer)
	(pubyte 'bufp 'bufe)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/stream/available ()
	;inputs
	;r0 = stream object (ptr)
	;outputs
	;r0 = stream object (ptr)
	;r1 = available space (bytes)
	;trashes
	;r1-r2
	(assign '((r0 stream_bufp) (r0 stream_bufe)) '(r2 r1))
	(vp-sub-rr r2 r1))

(defcfun class/stream/write_flush ()
	;inputs
	;r0 = stream object (ptr)
	;outputs
	;r0 = stream object (ptr)
	;trashes
	;r1
	(vp-xor-rr r1 r1)
	(assign '(r1 r1) '((r0 stream_bufp) (r0 stream_bufe))))
