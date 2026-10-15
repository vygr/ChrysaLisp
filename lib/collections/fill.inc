;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fill, an array made so long, each element the one given
;
; (fill array length value) -> array
;
; Of a list each element is the object, a reference to it for each, and
; those it had are let go. Of the rest, nums, fixeds, reals, a path, each
; is the number, of the array's kind. A set or a map is not filled.
;
; Bound here, not in class/lisp/root.inc, so a boot image from before it
; can still build the tree; it goes there with the next snapshot.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "class/array/lisp_fill" fill)
; (fill array length value) -> array
