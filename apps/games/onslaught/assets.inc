;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught graphical assets manager
; preloads, resizes, and caches all image canvases in global variables
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
		(defq pix (getf c +canvas_pixmap 0))
		(.-> (Canvas (getf pix +pixmap_width 0) (getf pix +pixmap_height 0) 1)
			(:flip_x c) (:swap 0))))

(defun load-scaled-image (rel_path zoom)
	; (load-scaled-image rel_path zoom) -> canvas
	; loads an image from disk and resizes to the specified zoom level
	(defq c (canvas-load (cat *app_root* rel_path) +load_flag_noswap))
	(ifn (and c (/= zoom 1)) (. c :swap 0)
		(defq pix (getf c +canvas_pixmap 0))
		(.-> (Canvas (* (getf pix +pixmap_width 0) zoom) (* (getf pix +pixmap_height 0) zoom) 1)
			(:resize c) (:swap 0))))

(defun load-cpm-assets (zoom)
	; (load-cpm-assets zoom)
	; loads and caches all assets at the given zoom level, generating _r counterparts
	(setq *img_ascii* (load-scaled-image "image/ascii.cpm" zoom)
		*img_blocks1* (load-scaled-image "image/blocks1.cpm" zoom)
		*img_blocks2* (load-scaled-image "image/blocks2.cpm" zoom)
		*img_blocks3* (load-scaled-image "image/blocks3.cpm" zoom)
		*img_buttons* (load-scaled-image "image/buttons.cpm" zoom)
		*img_campain* (load-scaled-image "image/campain.cpm" zoom)
		*img_frm_16x16* (load-scaled-image "image/frm_16x16.cpm" zoom)
		*img_frm_16x32* (load-scaled-image "image/frm_16x32.cpm" zoom)
		*img_frm_16x64* (load-scaled-image "image/frm_16x64.cpm" zoom)
		*img_frm_32c32* (load-scaled-image "image/frm_32c32.cpm" zoom)
		*img_frm_64x64* (load-scaled-image "image/frm_64x64.cpm" zoom)
		*img_fanatic_l* (load-scaled-image "image/fanatic_l.cpm" zoom)
		*img_frm_16x16_l* (load-scaled-image "image/frm_16x16_l.cpm" zoom)
		*img_frm_32c32_l* (load-scaled-image "image/frm_32c32_l.cpm" zoom)
		*img_frm_32x16_l* (load-scaled-image "image/frm_32x16_l.cpm" zoom)
		*img_frm_32x32_l* (load-scaled-image "image/frm_32x32_l.cpm" zoom)
		*img_frm_64x32_l* (load-scaled-image "image/frm_64x32_l.cpm" zoom)
		*img_frm_64x64_l* (load-scaled-image "image/frm_64x64_l.cpm" zoom)
		*img_panel* (load-scaled-image "image/panel.cpm" zoom)
		*img_fanatic_r* (flip-canvas *img_fanatic_l*)
		*img_frm_16x16_r* (flip-canvas *img_frm_16x16_l*)
		*img_frm_32c32_r* (flip-canvas *img_frm_32c32_l*)
		*img_frm_32x16_r* (flip-canvas *img_frm_32x16_l*)
		*img_frm_32x32_r* (flip-canvas *img_frm_32x32_l*)
		*img_frm_64x64_r* (flip-canvas *img_frm_64x64_l*)))

(defq
	*sfx_boar* :nil *sfx_clash1* :nil *sfx_clash2* :nil *sfx_death1* :nil
	*sfx_death2* :nil *sfx_drip* :nil *sfx_explode* :nil *sfx_hooves* :nil
	*sfx_horse* :nil *sfx_out* :nil *sfx_roar* :nil *sfx_spell* :nil
	*sfx_throw* :nil *sfx_twang1* :nil *sfx_twang2* :nil)

(defun unload-wav-assets ()
	; (uload-wav-assets)
	; unloads and caches all WAV assets
	(if *sfx_boar* (audio-remove-rpc *sfx_boar*))
	(if *sfx_clash1* (audio-remove-rpc *sfx_clash1*))
	(if *sfx_clash2* (audio-remove-rpc *sfx_clash2*))
	(if *sfx_death1* (audio-remove-rpc *sfx_death1*))
	(if *sfx_death2* (audio-remove-rpc *sfx_death2*))
	(if *sfx_drip* (audio-remove-rpc *sfx_drip*))
	(if *sfx_explode* (audio-remove-rpc *sfx_explode*))
	(if *sfx_hooves* (audio-remove-rpc *sfx_hooves*))
	(if *sfx_horse* (audio-remove-rpc *sfx_horse*))
	(if *sfx_out* (audio-remove-rpc *sfx_out*))
	(if *sfx_roar* (audio-remove-rpc *sfx_roar*))
	(if *sfx_spell* (audio-remove-rpc *sfx_spell*))
	(if *sfx_throw* (audio-remove-rpc *sfx_throw*))
	(if *sfx_twang1* (audio-remove-rpc *sfx_twang1*))
	(if *sfx_twang2* (audio-remove-rpc *sfx_twang2*))
	(setq *sfx_boar* :nil *sfx_clash1* :nil *sfx_clash2* :nil
		*sfx_death1* :nil *sfx_death2* :nil *sfx_drip* :nil
		*sfx_explode* :nil *sfx_hooves* :nil *sfx_horse* :nil
		*sfx_out* :nil *sfx_roar* :nil *sfx_spell* :nil
		*sfx_throw* :nil *sfx_twang1* :nil *sfx_twang2* :nil))

(defun load-wav-assets ()
	; (load-wav-assets)
	; loads and caches all WAV assets
	(setq *sfx_boar* (audio-add-rpc (cat *app_root* "sfx/boar.wav"))
		*sfx_clash1* (audio-add-rpc (cat *app_root* "sfx/clash1.wav"))
		*sfx_clash2* (audio-add-rpc (cat *app_root* "sfx/clash2.wav"))
		*sfx_death1* (audio-add-rpc (cat *app_root* "sfx/death1.wav"))
		*sfx_death2* (audio-add-rpc (cat *app_root* "sfx/death2.wav"))
		*sfx_drip* (audio-add-rpc (cat *app_root* "sfx/drip.wav"))
		*sfx_explode* (audio-add-rpc (cat *app_root* "sfx/explode.wav"))
		*sfx_hooves* (audio-add-rpc (cat *app_root* "sfx/hooves.wav"))
		*sfx_horse* (audio-add-rpc (cat *app_root* "sfx/horse.wav"))
		*sfx_out* (audio-add-rpc (cat *app_root* "sfx/out.wav"))
		*sfx_roar* (audio-add-rpc (cat *app_root* "sfx/roar.wav"))
		*sfx_spell* (audio-add-rpc (cat *app_root* "sfx/spell.wav"))
		*sfx_throw* (audio-add-rpc (cat *app_root* "sfx/throw.wav"))
		*sfx_twang1* (audio-add-rpc (cat *app_root* "sfx/twang1.wav"))
		*sfx_twang2* (audio-add-rpc (cat *app_root* "sfx/twang2.wav"))))
