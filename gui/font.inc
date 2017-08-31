(import 'sys/class.inc)
(import 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;
; font static class
;;;;;;;;;;;;;;;;;;;

(def-class 'gui_font)
(def-method 'statics 'gui/font_statics)
(def-method 'init 'gui/font_init)
(def-method 'deinit 'gui/font_deinit)
(def-method 'open 'gui/font_open 'static '(r0 r1) '(r0))
(def-method 'text 'gui/font_text 'static '(r0 r1) '(r0))
(def-method 'bounds 'gui/font_bounds 'static '(r0 r1) '(r9 r10))

;;;;;;;;;;;;;;;;;
; font structures
;;;;;;;;;;;;;;;;;

(def-struct 'ft_font)
	(ptr 'next)
	(ulong 'handle)
	(ulong 'points)
	(ulong 'ascent)
	(ulong 'descent)
	(ulong 'height)
	(offset 'name)
(def-struct-end)

(def-struct 'ft_text)
	(ptr 'next)
	(ulong 'font)
	(ulong 'texture)
	(ulong 'width)
	(ulong 'height)
	(offset 'name)
(def-struct-end)

(defcvar 'ft_num_buckets 61)
(defcvar 'ft_buckets_size (mul ptr_size ft_num_buckets))

(def-struct 'ft_statics)
	(ptr 'font_flist)
	(struct 'text_flists 'ft_buckets)
(def-struct-end)
