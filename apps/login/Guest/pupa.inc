;user home
(defq *env_home* (cat "apps/login/" *env_user* "/"))

;launcher apps
(defq *env_launcher_apps* '(
	"terminal" "edit" "help" "docs" "debug" "netmon" "stats"
	"fonts" "whiteboard" "clock" "calculator"
	"boing" "freeball" "bubbles" "images" "films" "canvas" "raymarch" "mandelbrot" "pcb" "chess")
	*env_launcher_auto_apps* '("wallpaper" "clock" "terminal"))

;wallpaper images
(defq *env_wallpaper_images* '("apps/wallpaper/wallpaper640x480.cpm"
	"apps/wallpaper/wallpaper1280x1024.cpm" "apps/wallpaper/wallpaper1920x1080.cpm"))

;terminal prompt and scroll back
(defq *env_terminal_prompt* (cat *env_user* ">") *env_terminal_lines* (* 40 10))

;default text files to open when edit opens
(defq *env_edit_auto* '("apps/edit/message"))

;widget property defaults
(ffi create-font "gui/font/lisp_create" 0)
(defq *env_window_col* 0xffc0c0c0 *env_title_col* 0xffe0e0e0 *env_slider_col* 0xffe0e0e0
	*env_toolbar_col* 0xffc0c0c0 *env_toolbar2_col* 0xffa0a0a0 *env_ink_col* 0xff000000
	*env_backdrop_ink_col* 0xff101010 *env_backdrop_col* 0xff202020
	*env_window_border* 5 *env_label_border* 0 *env_button_border* 1
	*env_textfield_border* -1 *env_title_border* 1 *env_title_buttons_border* 1
	*env_window_font* (create-font "fonts/OpenSans-Regular.ctf" 18)
	*env_title_font* (create-font "fonts/OpenSans-Regular.ctf" 20)
	*env_title_buttons_font* (create-font "fonts/Entypo.ctf" 26)
	*env_toolbar_font* (create-font "fonts/Entypo.ctf" 32)
	*env_medium_toolbar_font* (create-font "fonts/Entypo.ctf" 24)
	*env_small_toolbar_font* (create-font "fonts/Entypo.ctf" 16)
	*env_terminal_font* (create-font "fonts/Hack-Regular.ctf" 16)
	*env_medium_terminal_font* (create-font "fonts/Hack-Regular.ctf" 14)
	*env_small_terminal_font* (create-font "fonts/Hack-Regular.ctf" 12))
