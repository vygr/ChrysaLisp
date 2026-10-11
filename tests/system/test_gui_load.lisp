(report-header "GUI load: the GUI loads a file at a time, in the order of what each file imports, as the GUI service loads it")

;The GUI service does not (import "gui/lisp.inc") top to bottom. It lists
;every file the GUI stands on, (files-all-depends), and imports each by
;itself, those that are imported by others first, service/gui/app_impl.lisp.
;So a file that needs a name as it loads, a constant say, has to import
;the file that has it. One that does not loads in a test, or an app, where
;the whole of gui/lisp.inc has gone before it, and stops a desktop from
;starting. This loads it the way the service does, in a task of its own,
;into an environment that has none of the GUI in it, then a user's
;environment on top, as the service does next.

(defq gl_said (test-output (cat
	"(import {lib/files/files.inc}) (env-push) (defq gl_env (env))"
	;an error is caught, and said, so that the environment is popped
	;whatever happens. Left pushed, the task does not end
	" (catch (progn (reach (# (import %0 gl_env))"
	" (files-all-depends (list {sys/lisp.inc} {class/lisp.inc} {gui/lisp.inc}) :nil 40))"
	" (defq *env_user* {Guest}) (import {usr/Guest/env.inc})"
	" (print (if (def? (quote Files)) {loaded} {no Files class}))"
	;and what was loaded is used. A name a file wants as it is read, in a
	;(const), that is not there yet, a loader that came before the canvas
	;that imports it, does not stop the load: it stops the first use, the
	;first picture a desktop loads. Seen 2026-10-11, a desktop that did
	;not start with every test passing
	" (each (lambda (file) (print (if (> (first (canvas-info file)) 0) {info} {no info})"
	" (if (canvas-load file +load_flag_noswap) {load} {no load})))"
	" (list {apps/demos/boing/data/taoball_1.cpm} {apps/media/images/data/wallpaper.tga}"
	" {apps/media/images/data/burger.svg} {apps/media/whiteboard/data/palette.cwb})))"
	" (progn (print _) :t))"
	" (env-pop)")))
(assert-eq "every file of the GUI loads by itself, in the order the service has them, and a picture of each kind is loaded by it"
	"loaded\ninfoload\ninfoload\ninfoload\ninfoload\n" gl_said)
(assert-true "and none of them says there is an error" (empty? (substr gl_said "Error")))
