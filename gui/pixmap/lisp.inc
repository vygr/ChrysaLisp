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
; (pixmap-shared width height key create) -> :nil | pixmap
;a premultiplied 32 bit pixmap with its pixels in shared memory, under the
;name key, at most 30 characters. create 1 makes it, the pixels zero, and
;the name goes when that pixmap does. create 0 finds one another node of
;this machine made, of the same size, and both draw on the same pixels.

(defun pixmap-key ()
	; (pixmap-key) -> str
	;a name for a shared pixmap that no other task will come up with
	(cat "clpx-" (str (pii-time)) "-" (str (logand (get-long (task-mbox) 0) 0xffffff))))
