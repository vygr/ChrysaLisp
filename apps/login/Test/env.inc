;user home
(defq *env_home* (cat "apps/login/" *env_user* "/"))

;launcher apps
(defq *env_launcher_apps* '(
	"terminal" "services"
	"edit" "viewer"
	"docs" "fonts"
	"netmon" "netspeed"
	"logout")
	*env_launcher_auto_apps* '("terminal"))

;default gui window size/pos
(defq *env_window_x* 0 *env_window_y* 0
	*env_window_width* 1280 *env_window_height* 960)

;launcher placement from xy :center :top :bottom :left :right)
(defq *env_launcher_position* :center)

;wallpaper images
(defq *env_wallpaper_images* '("apps/images/data/logo.cpm"))

;terminal prompt and scroll back
(defq *env_terminal_prompt* (cat *env_user* "> ") *env_terminal_lines* 1000)

;timezones must be locations not abbreviations. See lib/date/date.inc for more info.
(defq *env_clock_timezone* "Etc/GMT" *env_clock_analog* :t *env_clock_digital* :t
	*env_clock_twelve_hour* :t  *env_clock_seconds* :t  *env_clock_pad_hour* :nil *env_clock_dotw* :t)

;widget property defaults
(ffi create-font "gui/font/lisp_create" 0)
(defq *env_window_col* 0xff00c000 *env_title_col* 0xff00e000 *env_slider_col* 0xff00e000
	*env_toolbar_col* 0xff00c000 *env_toolbar2_col* 0xff00a000 *env_ink_col* 0xff000000
	*env_hint_col* 0xff00b000 *env_no_hint_col* *env_ink_col*
	*env_window_border* 1 *env_window_shadow* 5 *env_label_border* 0 *env_button_border* 1
	*env_textfield_border* -1 *env_title_border* 1 *env_title_buttons_border* 1
	*env_window_font* (create-font "fonts/OpenSans-Regular.ctf" 18)
	*env_title_font* (create-font "fonts/OpenSans-Regular.ctf" 20)
	*env_body_font* (create-font "fonts/OpenSans-Regular.ctf" 14)
	*env_title_buttons_font* (create-font "fonts/Entypo.ctf" 22)
	*env_toolbar_font* (create-font "fonts/Entypo.ctf" 28)
	*env_medium_toolbar_font* (create-font "fonts/Entypo.ctf" 22)
	*env_small_toolbar_font* (create-font "fonts/Entypo.ctf" 16)
	*env_tiny_toolbar_font* (create-font "fonts/Entypo.ctf" 10)
	*env_terminal_font* (create-font "fonts/Hack-Regular.ctf" 16)
	*env_medium_terminal_font* (create-font "fonts/Hack-Regular.ctf" 14)
	*env_small_terminal_font* (create-font "fonts/Hack-Regular.ctf" 12)
	*env_editor_font* (create-font "fonts/Hack-Regular.ctf" 20)
	*env_tip_font* (create-font "fonts/OpenSans-Regular.ctf" 14)
	*env_warning_font* (create-font "fonts/Entypo.ctf" 128))

;keyboard map module
(defq
	*env_keyboard_map* "lib/keys/macbook_uk.inc"
	;*env_keyboard_map* "lib/keys/macbook_us.inc"
	;*env_keyboard_map* "lib/keys/microsoft_uk.inc"
	;*env_keyboard_map* "lib/keys/microsoft_us.inc"
)
