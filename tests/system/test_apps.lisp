(report-header "Apps: what a test can see of an app without a desktop to start it on")

(import "lib/files/files.inc")

;A + constant is put in place of its name as the code is read. One that is
;a list, quoted once, is put there bare, and where it is an argument it is
;run as a call: (f +types) is (f (".md")), not_a_function. The Docs app
;would not start for it. Such a list is quoted twice, ''(...), or given a
;*name*. No test starts an app, so the source of each is looked through
;for a + name given a list quoted once, on a line of a (defq)
(defq ap_found (list))
(each (lambda (file)
	(defq ap_in :nil)
	(lines! (lambda (line)
			(defq text (trim-start line (const (char-class " \t"))))
			(cond
				((starts-with "(defq" text) (setq ap_in :t))
				((not (starts-with "+" text)) (setq ap_in :nil)))
			(if (and ap_in (nempty? (matches text "\\+[a-z_0-9]+ '\\(")) (empty? (matches text "\\+[a-z_0-9]+ ''\\(")))
				(push ap_found (cat file " " text)))
			:nil)
		(file-stream file)))
	(files-all "apps" '(".lisp" ".inc")))
(assert-list-eq "no app has a + constant that is a list quoted once" '() ap_found)
(assert-true "the pattern does find one" (nempty? (matches "(defq +a_b '(1 2))" "\\+[a-z_0-9]+ '\\(")))
(assert-true "and tells it from one quoted twice" (empty? (matches "(defq +a_b ''(1 2))" "\\+[a-z_0-9]+ '\\(")))

;The Docs app runs what a document has between ```lisp and ```, and shows
;what it gives. Code that is only there to be read is between ```vdu and
;```. A document that has the first for the second throws as its page is
;drawn, each time, to the terminal the desktop was started from: an API
;written as (name args) -> result, a line of a shader, a trap shown as it
;is. So every such block of the documents is run here, as the app runs it,
;in an environment of its own. Not those of docs/gui/, which make widgets
;and want a desktop, nor docs/reference/, which the build writes
(import "usr/env.inc")
(import "gui/lisp.inc")
(defq ap_threw (list) ap_blocks 0)
(each (lambda (file)
	(defq ap_src :nil ap_line 0 ap_at 0)
	(lines! (lambda (line)
			(++ ap_line)
			(defq text (trim line (const (char-class " \t\r"))))
			(cond
				((and (not ap_src) (eql text "```lisp")) (setq ap_src (list) ap_at ap_line))
				((and ap_src (starts-with "```" text))
					(++ ap_blocks)
					(defq ap_ss (string-stream (join ap_src "\n")))
					(if (eval (static-qq (progn
							(env-push)
							(defq ap_bad (catch (progn (repl ,ap_ss "block") :nil) :t))
							(export-symbols '(ap_bad))
							(env-pop)
							ap_bad)))
						(push ap_threw (cat file ":" (str ap_at))))
					(setq ap_src :nil))
				(ap_src (push ap_src line)))
			:nil)
		(file-stream file)))
	(filter (# (not (or (starts-with "docs/gui/" %0) (starts-with "docs/reference/" %0))))
		(sort (files-all "docs" '(".md")))))
(assert-true "the documents have blocks that the Docs app runs" (> ap_blocks 40))
(assert-list-eq "and none of them throws" '() ap_threw)

;the Docs app's action for a link that is pressed, as the app has it, with
;a Link of its window to press. Each of these is not to be followed, and
;none is to throw: the tree is asked, and it has none of them. One that is
;followed draws the page, which wants a desktop, and is not tried here.
;The app keeps the environment its ```lisp blocks run in, *handler_env*,
;and lets go of it as its main ends. That environment's parent is the
;app's own, the two hold each other, and a task that loads the app and
;does not run its main has to let go of it or it never ends
(defq ap_said (test-output (cat
	"(catch (import {apps/desktop/docs/app.lisp}) :t)"
	" (. *file_selector* :populate +doc_root +doc_types)"
	" (. *window* :add_child (defq ap_link (Link)))"
	" (defq *current_file* {docs/gui/widgets.md} ap_act (. *event_map* :find +event_link)"
	" *msg* (setf-> (str-alloc +ev_msg_action_size) (+ev_msg_type +ev_type_action)"
	" (+ev_msg_target_id +event_link) (+ev_msg_action_source_id (. ap_link :get_id))))"
	" (print (map (lambda (target) (def ap_link :link target)"
	" (catch (if (ap_act) :yes :no) :threw))"
	" (list {../../README.md} {no_such.md} {http://x.org/a.md} {../history/press/1991-12_byte.pdf} {#a-place-in-no-page}))"
	" { } (if (. *file_selector* :find_node {docs/gui/event_dispatch.md}) :in :out)"
	" { } (list +doc_root +doc_types))"
	;wherever it was put, this string is not run at the top of its task
	" (defq ap_e (env)) (while ap_e (undef ap_e '*handler_env*) (setq ap_e (penv ap_e)))")))
(assert-eq "a link out of the root, to no file, to the web, to a pdf, or to a place with no page up is not followed, one in the tree is there to follow, and the root and kinds are as the app has them"
	"(:no :no :no :no :no) :in (\qdocs\q (\q.md\q))\n" ap_said)
