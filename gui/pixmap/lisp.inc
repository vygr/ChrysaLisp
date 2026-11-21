(import "./struct.inc")
(import "class/buf/lisp.inc")

(ffi "gui/pixmap/lisp_as_argb" pixmap-as-argb)
; (pixmap-as-argb pixmap) -> pixmap
(ffi "gui/pixmap/lisp_to_argb32" pixmap-to-argb32)
; (pixmap-to-argb32 pixel type) -> argb32
(ffi "gui/pixmap/lisp_from_argb32" pixmap-from-argb32)
; (pixmap-from-argb32 pixel type) -> pixel
(ffi "gui/pixmap/lisp_write" pixmap-write)
; (pixmap-write pixmap stream type) -> pixmap
(ffi "gui/pixmap/lisp_read" pixmap-read)
; (pixmap-read pixmap stream type) -> :nil | pixmap
(ffi "gui/pixmap/lisp_shared" pixmap-shared)
; (pixmap-shared width height key) -> :nil | pixmap
;a premultiplied 32 bit pixmap with its pixels the block of a buf, shared
;memory, class/buf. A key of 0 makes them, zero, under a key of its own,
;(pixmap-key). A pixmap on another node of this machine that is given that
;key, and the same size, finds them, and both draw on the same pixels. The
;key goes when the buf of the pixmap that made it does, the pixels last
;till the last one on them has.

(defun pixmap-buf (pixmap)
	; (pixmap-buf pixmap) -> :nil | buf
	;the buf its pixels are the block of, :nil if they are its own
	(getf pixmap +pixmap_buf 0))

(defun pixmap-key (pixmap)
	; (pixmap-key pixmap) -> 0 | key
	;the key of the buf its pixels are the block of, 0 if they are its own
	(if (defq b (pixmap-buf pixmap)) (buf-key b) 0))

(defun pixmap-clear (pixmap argb x y w h)
	; (pixmap-clear pixmap argb x y w h) -> pixmap
	;a box of it, as much as is on it, made the color: set, not laid over
	;what is there, so 0 makes it clear, to be drawn in again. The function
	;is looked for when this is called, so a boot image from before it was
	;written can still load this, and build it
	((ffi "gui/pixmap/lisp_clear") pixmap argb x y w h))

(defun pixmap-attach (held width height key)
	; (pixmap-attach held width height key) -> :nil | pixmap
	;the pixmap of the shared pixels of a key, (pixmap-shared), kept in
	;held, a list of the caller's, empty at first, and found again only
	;when the key or the size is not the one it was found by. :nil if the
	;key is 0 or the pixels can not be reached, from another machine say
	(unless (eql (first held) (defq want (list key width height)))
		(clear held)
		(push held want (and (/= key 0) (pixmap-shared width height key))))
	(second held))
