;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught window & ui layers hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +event 0
	(enum close max min))

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
		(ui-view *layer_panel_status* (:color 0)
			(ui-element *panel_flag_l* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_flag))
			(ui-element *panel_flag_r* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_flag))
			(ui-element *panel_power* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_power))
			(ui-element *panel_strength* (Sprite *img_frm_32c32* :nil 32 32 :32c32) (:sp_frame +frm_32c32_strength)))
		(ui-view *layer_panel_items*)
		(ui-view *layer_panel_detail*)))

(defq +sprite_layers `'(,*layer_fx* ,*layer_missiles* ,*layer_bodies* ,*layer_player* ,*layer_enemies*
		,*layer_items* ,*layer_bans*)
	+world_layers `'(,*layer_fx* ,*layer_missiles* ,*layer_bodies* ,*layer_player* ,*layer_enemies*
		,*layer_items* ,*layer_bans* ,*layer_land* ,*layer_sky*)
	+panel_layers `'(,*layer_panel_detail* ,*layer_panel_items* ,*layer_panel_status* ,*layer_panel_axes*))