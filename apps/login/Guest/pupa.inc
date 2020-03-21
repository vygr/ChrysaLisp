;user home
(defq *env_home* (cat "apps/login/" *env_user* "/"))

;launcher apps
(defq *env_launcher_apps* '("terminal" "edit" "debug" "help" "docs" "netmon" "stats" "boing" "freeball"
	"images" "films" "canvas" "raymarch" "mandelbrot" "pcb" "calculator" "chess" "clock" "fonts")
	*env_launcher_auto_apps* '("wallpaper" "clock" "terminal"))

;wallpaper images
(defq *env_wallpaper_images* '("apps/wallpaper/wallpaper640x480.cpm"
	"apps/wallpaper/wallpaper1280x1024.cpm" "apps/wallpaper/wallpaper1920x1080.cpm"))

;terminal prompt and scroll back
(defq *env_terminal_prompt* (cat *env_user* ">") *env_terminal_lines* (* 40 10))

;default text files to open when edit opens
(defq *env_edit_auto* '("apps/edit/message"))
