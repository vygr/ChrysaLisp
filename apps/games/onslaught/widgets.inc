;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught window & ui layers hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +event 0
	(enum close max min))

(defclass ItemPanel () (View)
	; (ItemPanel) -> items inventory panel view
	(def this :color 0 :canvas :nil :items :nil :selected 0)

	(defmethod :draw ()
		; (. panel :draw) -> panel
		(when (and (defq c (get :canvas this)) (defq texture (getf c +canvas_texture 0)))
			(bind '(tid tw th) (texture-metrics texture))
			(defq zoom (get :zoom this)
				slot_w (* zoom 16) slot_h (* zoom 16)
				sight_raw_y (* +frm_16x16_sight slot_h)
				sight_col (/ sight_raw_y th)
				sight_sy (% sight_raw_y th)
				sight_sx (* sight_col slot_w)
				selected (get :selected this)
				items (get :items this))
			(when items
				(each (lambda (item_frame)
					(defq idx (!) slot_x (* idx slot_w))
					(when (> item_frame 0)
						(defq raw_y (* item_frame slot_h)
							col (/ raw_y th)
							item_sy (% raw_y th)
							item_sx (* col slot_w))
						(. this :ctx_blit tid +argb_white slot_x 0 slot_w slot_h item_sx item_sy))
					(when (= idx selected)
						(. this :ctx_blit tid +argb_white slot_x 0 slot_w slot_h sight_sx sight_sy)))
					items)))
		this))

(ui-window *window* (:resizable :nil)
	(ui-title-bar *title* "Onslaught" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-view *world_scroll* (:min_width (* *zoom* +screen_width) :min_height (* *zoom* (- +screen_height +panel_height)))
		(ui-view *world_layers* (:color 0)
			(ui-view *layer_fx*)
			(ui-view *layer_missiles*)
			(ui-view *layer_bodies*)
			(ui-view *layer_player*)
			(ui-view *layer_enemies*)
			(ui-view *layer_items*)
			(ui-view *layer_bans*)
			(ui-map *layer_land*)
			(ui-sky *layer_sky*)))
	(ui-view *panel_layers* (:color 0 :min_width (* *zoom* +screen_width) :min_height (* *zoom* +panel_height))
		(ui-view *layer_panel_axes*)
		(ui-element *layer_panel_items* (ItemPanel))
		(ui-view *layer_panel_status* (:color 0)
			(ui-element *panel_flag_l* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_flag))
			(ui-element *panel_flag_r* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_flag))
			(ui-element *panel_power* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_power))
			(ui-element *panel_strength* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_strength)))
		(ui-view *layer_panel_detail*)))

(defq +sprite_layers `'(,*layer_player* ,*layer_bans* ,*layer_items* ,*layer_enemies*
		,*layer_missiles* ,*layer_bodies* ,*layer_fx*)
	+world_layers `'(,*layer_player* ,*layer_bans* ,*layer_items* ,*layer_enemies*
		,*layer_missiles* ,*layer_bodies* ,*layer_fx* ,*layer_land* ,*layer_sky*)
	+panel_layers `'(,*layer_panel_detail* ,*layer_panel_items* ,*layer_panel_status* ,*layer_panel_axes*))
