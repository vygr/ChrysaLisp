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
