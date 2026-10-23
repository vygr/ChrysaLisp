(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: launch app ...

    options:
        -h --help: this help info.

    Open each app on the node of the GUI of this system, as the
    Launcher does, desktop/eyes for apps/desktop/eyes/app.lisp,
    or a path to the .lisp of it. An app's windows are views of
    that node, so it is opened there, wherever this runs.")
))

(defun app-file (app)
	(if (ends-with ".lisp" app) app (cat "apps/" app "/app.lisp")))

(defun main ()
	(and (defq stdio (create-stdio)) (defq args (options stdio usage))
		(cond
			((empty? (defq gui (mail-enquire "@Gui,"))) (print "launch: no GUI"))
			(:t ;the node of the GUI, its mailbox less the mailbox's own number
				(defq node (slice (mail-entry-id (first gui)) +long_size -1))
				(each! (lambda (app)
						(if (/= (age (defq file (app-file app))) 0)
							(progn (open-remote file node +kn_call_pin) (print "launch: " app))
							(print "launch: no " file))) (list args) 1 -1)))))
