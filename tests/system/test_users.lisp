(report-header "Users: the environment of every user under usr/ loads, as an app loads it")

;A user's env.inc is code, and it is changed when what it calls is, the
;themes say. Guest's is run by every test that makes a window. Another
;user's is run by nothing till somebody signs on as them. usr/env.inc
;loads Guest's and then the user's own on top, so that is what is done
;here, for each, in a task of its own, with a window made after it.

(import "lib/files/files.inc")

(defq us_users (filter (# (and (nql %0 "Guest") (not (starts-with "." %0)) (/= (age (cat "usr/" %0 "/env.inc")) 0)))
	(reduce (lambda (out (name kind)) (if (eql kind "4") (push out name) out))
		(partition (split (pii-dirlist "usr") ",") 2) (list))))
(assert-true "there is a user other than Guest to try" (nempty? us_users))
(each (lambda (user)
	(defq us_said (test-output (cat
		"(defq *env_user* {" user "}) (import {usr/Guest/env.inc}) (import {usr/" user "/env.inc})"
		" (import {gui/lisp.inc})"
		" (catch (progn (ui-window us_win () (ui-title-bar _ {t} (+sym_close) 0)"
		" (ui-tool-bar us_bar () (ui-buttons (+sym_undo) 1)))"
		" (. us_win :theme {Bold})"
		" (print *env_home* { } (first (font-info (get :font us_bar)))))"
		" (progn (print _) :t))")))
	(assert-eq (cat "the environment of " user " loads, and a window of theirs takes a theme")
		(cat "usr/" user "/ fonts/Symbols-Bold.ctf\n") us_said))
	us_users)
