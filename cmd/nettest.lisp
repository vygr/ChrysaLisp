(import "lib/options/options.inc")
(import "service/net/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: nettest [options] [host] [port]

    options:
        -h --help: this help info.

    Simple net service test. Default host: example.com, port: 80.")
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f :nil args (options stdio usage)))
		(defq host (if (> (length args) 1) (second args) "example.com")
			port (if (> (length args) 2) (str-to-num (third args)) 80))
		(print "Calling (net-open-rpc " host " " port ") via *Net service...")
		(if (defq conn (net-open-rpc host port))
			(progn
				(bind '(in out) conn)
				(print "Connection established! Sending HTTP request...")
				(write-line out (cat "GET / HTTP/1.1\r\nHost: "
					host "\r\nConnection: close\r\n\r\n"))
				(stream-flush out)
				(print "Reading response lines via In stream:")
				(lines! (const print) in)
				(print "\nFinished reading response."))
			(progn
				(print "Failed to connect!")))))