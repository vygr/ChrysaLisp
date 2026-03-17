(import "class/obj/lisp.inc")
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
