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
	" (print (if (def? (quote Files)) {loaded} {no Files class})))"
	" (progn (print _) :t))"
	" (env-pop)")))
(assert-eq "every file of the GUI loads by itself, in the order the service has them" "loaded\n" gl_said)
(assert-true "and none of them says there is an error" (empty? (substr gl_said "Error")))
