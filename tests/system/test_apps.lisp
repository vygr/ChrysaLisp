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
