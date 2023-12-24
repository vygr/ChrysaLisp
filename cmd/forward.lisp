(import "lib/options/options.inc")

;test a file
(defun test-file (file)
	(defq func_map (Fmap 11))
	(each-line (# (defq m (matches %0 "^\(defun (\S+)"))
			(when (nempty? m)
				(bind '((__ (x x1)) &ignore) m)
				(defq func (slice x x1 %0))
				(. func_map :insert func _)))
		(file-stream file))
	(each-line (lambda (line)
			(defq line_num _ m (matches line "\((\S+)"))
			(when (nempty? m)
				(each (# (bind '(_ (x x1)) %0)
					(defq func (slice x x1 line))
					(and (defq line (. func_map :find func))
						(< line_num line)
						(print file " (" (inc line_num) ") " func))) m)))
		(file-stream file)))

(defq usage `(
(("-h" "--help")
"Usage: forward [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then will test files from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;test file from stdin
			(each-line (# (test-file %0)) (io-stream 'stdin))
			;test file from args
			(each (# (test-file %0)) (rest args)))))
