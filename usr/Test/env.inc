;What Test has that the system does not, usr/env.inc has the defaults. As
;(def *env* key value ...), (get key *env*) for one that is there already.

;Test is the user the tests and Claude's desktops run as. Its desktop starts
;no app of its own: what is under test is launched onto an empty screen, and
;nothing opens later in front of it. Its wallpaper is plain, a grab of the
;whole screen is the windows and little else

(def *env*
	:launcher_auto_apps '()
	:wallpaper_images '()
	:terminal_lines 1000
	:paper_window 0xff00c000 :paper_title 0xff00e000 :paper_slider 0xff00e000
	:paper_toolbar 0xff00c000 :paper_toolbar2 0xff00a000 :ink_hint 0xff00b000
	:font_display (create-font "fonts/OpenSans-Regular.ctf" 32)
	:font_title (create-font "fonts/OpenSans-Regular.ctf" 20)
	:font_sub_title (create-font "fonts/OpenSans-Regular.ctf" 16)
	:font_bold (create-font "fonts/OpenSans-Regular.ctf" 14)
	:font_button (create-font "fonts/OpenSans-Regular.ctf" 13)
	:font_warning (create-font (get :symbol_file *env*) 128))
