(import "class/obj/lisp.inc")
(import "./struct.inc")

(ffi "gui/pixmap/lisp_as_argb" pixmap-as-argb)
; (pixmap-as-argb pixmap) -> pixmap
(ffi "gui/pixmap/lisp_to_argb32" pixmap-to-argb32)
; (pixmap-to-argb32 pixel type) -> argb32
(ffi "gui/pixmap/lisp_from_argb32" pixmap-from-argb32)
; (pixmap-from-argb32 pixel type) -> pixel

(defun pixmap-write (pixmap stream type)
	; (pixmap-write pixmap stream type) -> pixmap
	(defq w (getf pixmap +pixmap_width 0)
        h (getf pixmap +pixmap_height 0)
		num_bits type bit_pool_state (array 0 0)
		pixel_data +pixmap_data
		pixel_data_end (+ +pixmap_data (* w h +int_size)))
	(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
	(until (= pixel_data pixel_data_end)
		(write-bits stream bit_pool_state (pixmap-from-argb32 (obj-get pixmap pixel_data +type_uint) type) num_bits)
		(setq pixel_data (+ pixel_data +int_size)))
	(flush-bits stream bit_pool_state)
	pixmap)

(defun pixmap-read (pixmap stream type trans)
	; (pixmap-read pixmap stream type trans) -> :nil | pixmap
	(defq w (getf pixmap +pixmap_width 0)
        h (getf pixmap +pixmap_height 0)
		num_bits type bit_pool_state (array 0 0)
		pixel_data +pixmap_data
		pixel_data_end (+ +pixmap_data (* w h +int_size)))
	(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
    (while (< pixel_data pixel_data_end)
        (defq col (read-bits stream bit_pool_state num_bits))
        (if (= col -1)
            ; premature EOF
            (setq pixel_data pixel_data_end pixmap :nil)
            (progn
                (if (/= (setq col (pixmap-to-argb32 col type)) trans)
                    (obj-set pixmap pixel_data +type_uint col))
                (setq pixel_data (+ pixel_data +int_size)))))
	pixmap)
