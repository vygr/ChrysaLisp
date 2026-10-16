;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the user's environment, *env*, each app task its own
;
; Chris: "one app task global variable with the (env) properties of the
; user". The defaults of the system are here, and a user's env.inc,
; usr/<user>/env.inc, gives only what that user has otherwise, as (def
; *env* key value ...), (get key *env*) for what is there already. The
; properties of views, the plain :font :ink and a variant of one, :font_bold
; :paper_toolbar :border_button, the GUI puts on the screen, where each
; widget finds them, gui/lisp.inc. The rest are the user's settings,
; :home :user :clock_timezone :terminal_prompt and so on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "gui/font/lisp_create" create-font)
; (create-font name pixels) -> font

;the symbol fonts are those of the theme the user has, lib/theme/theme.inc
(import "lib/theme/theme.inc")

(defq *login_root* (path-to-file) *env* :nil
	;the user's properties as a list, and a root view with them, each task
	;its own, (env-props) (env-root) of gui/lisp.inc
	*env_props* :nil *env_root* :nil)

(defun env-load (user)
	; (env-load user) -> env
	;*env* of the user: the defaults, then what the user has otherwise
	(setq *env* (env 101) *env_props* :nil *env_root* :nil)
	(defq home (cat "usr/" user "/") symbols (theme-file (theme-current home)))
	(def *env* :user user :home home :symbol_file symbols
		;the apps the launcher starts, where it is, and the desktop
		:launcher_auto_apps '("system/terminal" "desktop/eyes") :launcher_position :center
		:window_x 0 :window_y 0 :window_width 1280 :window_height 960
		:wallpaper_images '("apps/system/wallpaper/data/wallpaper640x480.tga"
			"apps/system/wallpaper/data/wallpaper1280x1024.tga"
			"apps/system/wallpaper/data/wallpaper1920x1080.tga")
		;which way the mouse wheel scrolls, across and up and down: 1 as the
		;host gives it, -1 the other way
		:wheel_x 1 :wheel_y 1
		;the terminal's prompt and how much it keeps
		:terminal_prompt (cat user "> ") :terminal_lines 500
		;timezones must be locations not abbreviations, lib/date/date.inc
		:clock_timezone "Etc/GMT" :clock_analog :t :clock_digital :t
		:clock_twelve_hour :t :clock_seconds :t :clock_pad_hour :nil :clock_dotw :t
		:keyboard_map "lib/keys/macbook_uk.inc"
		;the look of views, the screen has them
		:paper_window 0xffc0c0c0 :paper_title 0xffe0e0e0 :paper_slider 0xffe0e0e0
		:paper_toolbar 0xffc0c0c0 :paper_toolbar2 0xffa0a0a0 :paper_highlight 0xfffffdaf
		:ink_window 0xff000000 :ink_hint 0xffb0b0b0 :ink_no_hint 0xff000000
		:border_window 1 :shadow_window 5 :border_label 0 :border_button 1
		:border_textfield -1 :border_title 1 :border_title_buttons 1
		:font_display (create-font "fonts/OpenSans-Bold.ctf" 32)
		:font_title (create-font "fonts/OpenSans-Bold.ctf" 20)
		:font_sub_title (create-font "fonts/OpenSans-Bold.ctf" 16)
		:font_window (create-font "fonts/OpenSans-Regular.ctf" 18)
		:font_body (create-font "fonts/OpenSans-Regular.ctf" 14)
		:font_bold (create-font "fonts/OpenSans-Bold.ctf" 14)
		:font_button (create-font "fonts/OpenSans-Regular.ctf" 18)
		:font_small (create-font "fonts/OpenSans-Regular.ctf" 12)
		:font_tiny (create-font "fonts/OpenSans-Regular.ctf" 10)
		:font_title_buttons (create-font symbols 22)
		:font_symbol (create-font symbols 28)
		:font_medium_symbol (create-font symbols 22)
		:font_small_toolbar (create-font symbols 16)
		:font_tiny_symbol (create-font "fonts/Symbols-Bold.ctf" 10)
		:font_terminal (create-font "fonts/Hack-Regular.ctf" 16)
		:font_medium_terminal (create-font "fonts/Hack-Regular.ctf" 14)
		:font_small_terminal (create-font "fonts/Hack-Regular.ctf" 12)
		:font_editor (create-font "fonts/Hack-Regular.ctf" 20)
		:font_tip (create-font "fonts/OpenSans-Regular.ctf" 14)
		:font_warning (create-font symbols 64))
	(import (cat home "env.inc"))
	*env*)

;the user of a session that was started as one, run.sh -u name, every node
;of it is told as it starts, (node-start). Else the user the machine has,
;who last signed on
(env-load (ifn (get '*env_node_user*) (load (cat *login_root* "current"))))

;FOR NOW, till the apps have all been made to use *env*: the names they
;had, from it, in the task that imports this. Each goes as its last use does
(defq *env_user* (get :user *env*) *env_home* (get :home *env*) *env_symbol_file* (get :symbol_file *env*)
		*env_launcher_auto_apps* (get :launcher_auto_apps *env*) *env_launcher_position* (get :launcher_position *env*)
		*env_window_x* (get :window_x *env*) *env_window_y* (get :window_y *env*)
		*env_window_width* (get :window_width *env*) *env_window_height* (get :window_height *env*)
		*env_wallpaper_images* (get :wallpaper_images *env*) *env_wheel_x* (get :wheel_x *env*) *env_wheel_y* (get :wheel_y *env*)
		*env_terminal_prompt* (get :terminal_prompt *env*) *env_terminal_lines* (get :terminal_lines *env*)
		*env_clock_timezone* (get :clock_timezone *env*) *env_clock_analog* (get :clock_analog *env*)
		*env_clock_digital* (get :clock_digital *env*) *env_clock_twelve_hour* (get :clock_twelve_hour *env*)
		*env_clock_seconds* (get :clock_seconds *env*) *env_clock_pad_hour* (get :clock_pad_hour *env*)
		*env_clock_dotw* (get :clock_dotw *env*) *env_keyboard_map* (get :keyboard_map *env*)
		*env_window_col* (get :paper_window *env*) *env_title_col* (get :paper_title *env*)
		*env_slider_col* (get :paper_slider *env*) *env_toolbar_col* (get :paper_toolbar *env*)
		*env_toolbar2_col* (get :paper_toolbar2 *env*) *env_highlight_col* (get :paper_highlight *env*)
		*env_ink_col* (get :ink_window *env*) *env_hint_col* (get :ink_hint *env*) *env_no_hint_col* (get :ink_no_hint *env*)
		*env_window_border* (get :border_window *env*) *env_window_shadow* (get :shadow_window *env*)
		*env_label_border* (get :border_label *env*) *env_button_border* (get :border_button *env*)
		*env_textfield_border* (get :border_textfield *env*) *env_title_border* (get :border_title *env*)
		*env_title_buttons_border* (get :border_title_buttons *env*)
		*env_display_font* (get :font_display *env*) *env_title_font* (get :font_title *env*)
		*env_sub_title_font* (get :font_sub_title *env*) *env_window_font* (get :font_window *env*)
		*env_body_font* (get :font_body *env*) *env_bold_font* (get :font_bold *env*)
		*env_button_font* (get :font_button *env*) *env_small_font* (get :font_small *env*)
		*env_tiny_font* (get :font_tiny *env*) *env_title_buttons_font* (get :font_title_buttons *env*)
		*env_symbol_font* (get :font_symbol *env*) *env_medium_symbol_font* (get :font_medium_symbol *env*)
		*env_small_toolbar_font* (get :font_small_toolbar *env*) *env_tiny_symbol_font* (get :font_tiny_symbol *env*)
		*env_terminal_font* (get :font_terminal *env*) *env_medium_terminal_font* (get :font_medium_terminal *env*)
		*env_small_terminal_font* (get :font_small_terminal *env*) *env_editor_font* (get :font_editor *env*)
		*env_tip_font* (get :font_tip *env*) *env_warning_font* (get :font_warning *env*))
