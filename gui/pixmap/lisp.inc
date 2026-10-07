(import "./struct.inc")

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
;a premultiplied 32 bit pixmap with its pixels in shared memory. A key of
;0 makes them, zero, under a key of its own, (pixmap-key). A pixmap on
;another node of this machine that is given that key, and the same size,
;finds them, and both draw on the same pixels. The key goes when the
;pixmap that made it does, the pixels last till the last one on them has.

(defun pixmap-key (pixmap)
	; (pixmap-key pixmap) -> 0 | key
	;the key of the shared memory its pixels are in, 0 if they are its own
	(getf pixmap +pixmap_key 0))
