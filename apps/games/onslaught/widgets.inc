;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Window & UI Layers Hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +event 0
	(enum close max min))

(defq
	+zoom_1x 1.0
	+zoom_2x 2.0
	*zoom_scale* +zoom_1x)

; Window with stacked transparent View layers:
; Layer order (head to tail = front to back):
; 1. *layer_panel*   - Frontmost bottom status panel
; 2. *layer_sprites* - Game & title sprites (letters, units)
; 3. *layer_fx*      - FX & projectiles (sword, blood drip, spells)
; 4. *layer_land*    - Map / terrain tiles
; 5. *layer_sky*     - Background sky / black backdrop
(ui-window *window* (:min_width +game_width :min_height +game_height :border 0 :shadow 2)
	(ui-title-bar *title* "Onslaught" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow *playfield* (:flow_flags +flow_stack_fill :min_width +game_width :min_height +game_height)
		(ui-backdrop *layer_fx* (:color 0))
		(ui-backdrop *layer_sprites* (:color 0))
		(ui-backdrop *layer_land* (:color 0))
		(ui-backdrop *layer_sky* (:color 0))
		(ui-backdrop *layer_backdrop* (:color +argb_black :style :grid :ink_color +argb_white))))
