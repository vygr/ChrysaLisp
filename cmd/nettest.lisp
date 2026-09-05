(import "lib/options/options.inc")
(import "service/net/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: nettest [options] [path] ...

    options:
        -h --help: this help info.

    Simple net service tests.")
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f :nil args (options stdio usage)))
		(print "Calling (net-open-rpc example.com 80) via *Net service...")
		(stream-flush (io-stream 'stdout))
		(bind '(in out) (net-open-rpc "example.com" 80))
		(if (and in out)
			(progn
				(print "Connection established! Sending HTTP request...")
				(stream-flush (io-stream 'stdout))
				(write-line out "GET / HTTP/1.1")
				(write-line out "Host: example.com")
				(write-line out "Connection: close")
				(write-line out "")
				(stream-flush out)
				(print "Reading response lines via In stream:\n")
				(stream-flush (io-stream 'stdout))
				(lines! (lambda (line)
					(print line)
					(stream-flush (io-stream 'stdout))) in)
				(print "\nFinished reading response.")
				(stream-flush (io-stream 'stdout)))
			(progn
				(print "Failed to connect!")
				(stream-flush (io-stream 'stdout))))))
