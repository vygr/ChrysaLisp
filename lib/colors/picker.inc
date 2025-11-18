;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ColorPicker Widget
; Full-featured HSV/RGB color selection widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/colors/colors.inc")
(import "gui/lisp.inc")

;;;;;;;;;;;;;;;;;;;
; ColorPicker Class
;;;;;;;;;;;;;;;;;;;

(defclass ColorPicker () (Flow)
	; A comprehensive color picker widget with HSV/RGB controls,
	; web color palette, and hex input

	(defmethod :init ()
		; Initialize the color picker
		(bind '(this) (call :super :init))

		; Default state
		(def this
			:current_color 0xffff0000  ; Start with red
			:h 0 :s 100 :v 100          ; HSV values
			:r 255 :g 0 :b 0            ; RGB values
			:updating :nil              ; Flag to prevent feedback loops
			:flow_flags +flow_down_fill)

		; Build the UI
		(. this :build_ui)
		this)

	(defmethod :build_ui ()
		; Build the color picker UI
		(defq preview_height 60
			  slider_height 30
			  palette_button_size 24)

		; Color preview area
		(defq preview_flow (ui-flow _ (:min_height preview_height :flow_flags +flow_right_fill)))
		(. this :add_child preview_flow)

		(defq color_preview (ui-view _ (:min_width 100 :min_height preview_height
										 :color (get :current_color this)
										 :flow_flags (logior +flow_flag_right +flow_flag_fillw))))
		(def this :color_preview color_preview)
		(. preview_flow :add_child color_preview)

		; Hex display and input
		(defq info_flow (ui-flow _ (:flow_flags +flow_down_fill :min_width 120)))
		(. preview_flow :add_child info_flow)
		(. info_flow :add_child (ui-label _ (:text "Hex:")))
		(defq hex_field (ui-textfield _ (:text "#FF0000" :min_width 100)))
		(def this :hex_field hex_field)
		(. hex_field :connect (+ 6 (get :action_event this)))
		(. info_flow :add_child hex_field)

		; HSV Sliders section
		(. this :add_child (ui-label _ (:text "HSV Controls:")))

		(defq h_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child h_flow)
		(. h_flow :add_child (ui-label _ (:text "H:" :min_width 20)))
		(defq h_slider (ui-slider _ (:maximum 360 :value 0 :min_width 200)))
		(def this :h_slider h_slider)
		(. h_slider :connect (get :action_event this))
		(. h_flow :add_child h_slider)
		(defq h_label (ui-label _ (:text "0" :min_width 40)))
		(def this :h_label h_label)
		(. h_flow :add_child h_label)

		(defq s_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child s_flow)
		(. s_flow :add_child (ui-label _ (:text "S:" :min_width 20)))
		(defq s_slider (ui-slider _ (:maximum 100 :value 100 :min_width 200)))
		(def this :s_slider s_slider)
		(. s_slider :connect (inc (get :action_event this)))
		(. s_flow :add_child s_slider)
		(defq s_label (ui-label _ (:text "100" :min_width 40)))
		(def this :s_label s_label)
		(. s_flow :add_child s_label)

		(defq v_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child v_flow)
		(. v_flow :add_child (ui-label _ (:text "V:" :min_width 20)))
		(defq v_slider (ui-slider _ (:maximum 100 :value 100 :min_width 200)))
		(def this :v_slider v_slider)
		(. v_slider :connect (+ 2 (get :action_event this)))
		(. v_flow :add_child v_slider)
		(defq v_label (ui-label _ (:text "100" :min_width 40)))
		(def this :v_label v_label)
		(. v_flow :add_child v_label)

		; RGB Sliders section
		(. this :add_child (ui-label _ (:text "RGB Controls:")))

		(defq r_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child r_flow)
		(. r_flow :add_child (ui-label _ (:text "R:" :min_width 20)))
		(defq r_slider (ui-slider _ (:maximum 255 :value 255 :min_width 200)))
		(def this :r_slider r_slider)
		(. r_slider :connect (+ 3 (get :action_event this)))
		(. r_flow :add_child r_slider)
		(defq r_label (ui-label _ (:text "255" :min_width 40)))
		(def this :r_label r_label)
		(. r_flow :add_child r_label)

		(defq g_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child g_flow)
		(. g_flow :add_child (ui-label _ (:text "G:" :min_width 20)))
		(defq g_slider (ui-slider _ (:maximum 255 :value 0 :min_width 200)))
		(def this :g_slider g_slider)
		(. g_slider :connect (+ 4 (get :action_event this)))
		(. g_flow :add_child g_slider)
		(defq g_label (ui-label _ (:text "0" :min_width 40)))
		(def this :g_label g_label)
		(. g_flow :add_child g_label)

		(defq b_flow (ui-flow _ (:flow_flags +flow_right_fill :min_height slider_height)))
		(. this :add_child b_flow)
		(. b_flow :add_child (ui-label _ (:text "B:" :min_width 20)))
		(defq b_slider (ui-slider _ (:maximum 255 :value 0 :min_width 200)))
		(def this :b_slider b_slider)
		(. b_slider :connect (+ 5 (get :action_event this)))
		(. b_flow :add_child b_slider)
		(defq b_label (ui-label _ (:text "0" :min_width 40)))
		(def this :b_label b_label)
		(. b_flow :add_child b_label)

		; Web colors palette
		(. this :add_child (ui-label _ (:text "Web Colors:")))
		(. this :build_palette)

		this)

	(defmethod :build_palette ()
		; Build web color palette with common colors
		(defq palette_colors '(
			:red :green :blue :yellow :cyan :magenta
			:orange :purple :pink :lime :navy :teal
			:maroon :olive :coral :gold :indigo :violet
			:crimson :forestgreen :royalblue :tomato :chocolate :steelblue
			:firebrick :seagreen :mediumblue :darkorange :saddlebrown :dodgerblue
			:white :lightgray :silver :gray :dimgray :black))

		(defq grid (ui-grid _ (:grid_width 6 :grid_height 0 :color (get :color this))))
		(. this :add_child grid)
		(def this :palette_grid grid)

		(each (lambda (color_name)
			(defq color_val (color-name-to-argb color_name))
			(when color_val
				(defq btn (ui-button _ (:min_width 24 :min_height 24
										:color color_val :text "")))
				(. btn :connect (+ 7 (get :action_event this)))
				(def btn :palette_color color_val)
				(. grid :add_child btn)))
			palette_colors)
		this)

	(defmethod :set_color_from_hsv (h s v)
		; Update color from HSV values
		(when (not (get :updating this))
			(def this :updating :t)
			(bind '(r g b) (hsv-to-rgb h s v))
			(defq new_color (rgb-pack r g b))
			(def this :current_color new_color :h h :s s :v v :r r :g g :b b)
			(. this :update_ui)
			(def this :updating :nil)
			(. this :emit))
		this)

	(defmethod :set_color_from_rgb (r g b)
		; Update color from RGB values
		(when (not (get :updating this))
			(def this :updating :t)
			(bind '(h s v) (rgb-to-hsv r g b))
			(defq new_color (rgb-pack r g b))
			(def this :current_color new_color :h h :s s :v v :r r :g g :b b)
			(. this :update_ui)
			(def this :updating :nil)
			(. this :emit))
		this)

	(defmethod :set_color (argb)
		; Set color from ARGB value
		(bind '(r g b) (rgb-unpack argb))
		(. this :set_color_from_rgb r g b)
		this)

	(defmethod :get_color ()
		; Get current color as ARGB
		(get :current_color this))

	(defmethod :update_ui ()
		; Update all UI elements to reflect current color
		(when (not (get :updating this))
			(def this :updating :t)

			(defq h (get :h this) s (get :s this) v (get :v this)
				  r (get :r this) g (get :g this) b (get :b this)
				  current_color (get :current_color this))

			; Update preview
			(def (. (get :color_preview this) :dirty) :color current_color)

			; Update HSV sliders and labels
			(def (get :h_slider this) :value h)
			(set (get :h_label this) :text (str h))
			(def (get :s_slider this) :value s)
			(set (get :s_label this) :text (str s))
			(def (get :v_slider this) :value v)
			(set (get :v_label this) :text (str v))

			; Update RGB sliders and labels
			(def (get :r_slider this) :value r)
			(set (get :r_label this) :text (str r))
			(def (get :g_slider this) :value g)
			(set (get :g_label this) :text (str g))
			(def (get :b_slider this) :value b)
			(set (get :b_label this) :text (str b))

			; Update hex field
			(set (get :hex_field this) :text (argb-to-hex current_color))

			(def this :updating :nil))
		this)

	(defmethod :action (event)
		; Handle slider and button events
		(when (not (get :updating this))
			(defq id (getf event +ev_msg_target_id)
				  base_id (get :action_event this))

			(cond
				; H slider
				((= id base_id)
					(. this :set_color_from_hsv
						(get :value (get :h_slider this))
						(get :s this)
						(get :v this)))

				; S slider
				((= id (inc base_id))
					(. this :set_color_from_hsv
						(get :h this)
						(get :value (get :s_slider this))
						(get :v this)))

				; V slider
				((= id (+ base_id 2))
					(. this :set_color_from_hsv
						(get :h this)
						(get :s this)
						(get :value (get :v_slider this))))

				; R slider
				((= id (+ base_id 3))
					(. this :set_color_from_rgb
						(get :value (get :r_slider this))
						(get :g this)
						(get :b this)))

				; G slider
				((= id (+ base_id 4))
					(. this :set_color_from_rgb
						(get :r this)
						(get :value (get :g_slider this))
						(get :b this)))

				; B slider
				((= id (+ base_id 5))
					(. this :set_color_from_rgb
						(get :r this)
						(get :g this)
						(get :value (get :b_slider this))))

				; Hex input
				((= id (+ base_id 6))
					(defq hex_text (. (get :hex_field this) :get_text))
					(when (defq new_color (hex-to-argb hex_text))
						(. this :set_color new_color)))

				; Palette button
				((>= id (+ base_id 7))
					(defq source_id (getf event +ev_msg_action_source_id))
					(when (defq btn (. this :find_id source_id))
						(when (defq pal_color (get :palette_color btn))
							(. this :set_color pal_color))))))
		this))

(export-symbols '(ColorPicker))
