;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/assets.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught graphical assets manager
; preloads, resizes, and caches all image canvases in global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "./enums.inc")

(unless (def? '*app_root*)
	(defq *app_root* (path-to-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; global asset variables for all image canvases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	; non-directional image canvases
	*img_ascii* :nil *img_blocks1* :nil *img_blocks2* :nil *img_blocks3* :nil
	*img_buttons* :nil *img_campain* :nil *img_panel* :nil
	*img_frm_16x16* :nil *img_frm_16x32* :nil *img_frm_16x64* :nil
	*img_frm_32c32* :nil *img_frm_64x64* :nil
	; directional image canvases (_l and _r)
	*img_fanatic_l* :nil *img_fanatic_r* :nil
	*img_frm_16x16_l* :nil *img_frm_16x16_r* :nil
	*img_frm_32c32_l* :nil *img_frm_32c32_r* :nil
	*img_frm_32x16_l* :nil *img_frm_32x16_r* :nil
	*img_frm_32x32_l* :nil *img_frm_32x32_r* :nil
	*img_frm_64x32_l* :nil *img_frm_64x32_r* :nil
	*img_frm_64x64_l* :nil *img_frm_64x64_r* :nil)

(defun flip-canvas (c)
	; (flip-canvas canvas) -> canvas
	; creates a horizontally flipped right-facing copy of a left-facing canvas
	(when c
		(defq
			pix (getf c +canvas_pixmap 0)
			w (getf pix +pixmap_width 0)
			h (getf pix +pixmap_height 0))
		(.-> (Canvas w h 1)
			(:flip_x c)
			(:swap 0))))

(defun load-scaled-image (rel_path &optional scale)
	; (load-scaled-image rel_path [scale]) -> canvas
	; loads an image from disk and resizes to the specified zoom level
	(setd scale *zoom*)
	(defq path (cat *app_root* rel_path)
		c (canvas-load path +load_flag_shared))
	(if (and c (/= scale 1))
		(progn
			(defq
				pix (getf c +canvas_pixmap 0)
				sw (getf pix +pixmap_width 0)
				sh (getf pix +pixmap_height 0)
				tw (* sw scale)
				th (* sh scale))
			(.-> (Canvas tw th 1)
				(:resize c)
				(:swap 0)))
		c))

(defun load-assets (&optional scale)
	; (load-assets [scale]) -> canvas
	; loads and caches all assets at the given zoom level, generating _r counterparts
	(setd scale *zoom*)
	(setq *zoom* scale
		*img_ascii*       (load-scaled-image "image/ascii.cpm" scale)
		*img_blocks1*     (load-scaled-image "image/blocks1.cpm" scale)
		*img_blocks2*     (load-scaled-image "image/blocks2.cpm" scale)
		*img_blocks3*     (load-scaled-image "image/blocks3.cpm" scale)
		*img_buttons*     (load-scaled-image "image/buttons.cpm" scale)
		*img_campain*     (load-scaled-image "image/campain.cpm" scale)
		*img_frm_16x16*   (load-scaled-image "image/frm_16x16.cpm" scale)
		*img_frm_16x32*   (load-scaled-image "image/frm_16x32.cpm" scale)
		*img_frm_16x64*   (load-scaled-image "image/frm_16x64.cpm" scale)
		*img_frm_32c32*   (load-scaled-image "image/frm_32c32.cpm" scale)
		*img_frm_64x64*   (load-scaled-image "image/frm_64x64.cpm" scale)
		*img_panel*       (load-scaled-image "image/panel.cpm" scale)
		*img_fanatic_l*   (load-scaled-image "image/fanatic_l.cpm" scale)
		*img_fanatic_r*   (flip-canvas *img_fanatic_l*)
		*img_frm_16x16_l* (load-scaled-image "image/frm_16x16_l.cpm" scale)
		*img_frm_16x16_r* (flip-canvas *img_frm_16x16_l*)
		*img_frm_32c32_l* (load-scaled-image "image/frm_32c32_l.cpm" scale)
		*img_frm_32c32_r* (flip-canvas *img_frm_32c32_l*)
		*img_frm_32x16_l* (load-scaled-image "image/frm_32x16_l.cpm" scale)
		*img_frm_32x16_r* (flip-canvas *img_frm_32x16_l*)
		*img_frm_32x32_l* (load-scaled-image "image/frm_32x32_l.cpm" scale)
		*img_frm_32x32_r* (flip-canvas *img_frm_32x32_l*)
		*img_frm_64x32_l* (load-scaled-image "image/frm_64x32_l.cpm" scale)
		*img_frm_64x32_r* (flip-canvas *img_frm_64x32_l*)
		*img_frm_64x64_l* (load-scaled-image "image/frm_64x64_l.cpm" scale)
		*img_frm_64x64_r* (flip-canvas *img_frm_64x64_l*)))
