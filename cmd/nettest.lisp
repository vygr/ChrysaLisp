(import "lib/options/options.inc")
(import "lib/net/url.inc")
(import "lib/net/http.inc")

(defq usage `(
(("-h" "--help")
"Usage: nettest [options] [url|host] [port]

    options:
        -h --help: this help info.

    Simple HTTP / Net service test.
    Examples:
        nettest http://example.com/
        nettest http://httpbin.org/get?msg=hello+world")
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq stdout (io-stream 'stdout)
			target (if (> (length args) 1) (second args) "http://example.com/")
			u (url-parse target))
		(if (> (length args) 2)
			(pinsert u :port (str-to-num (third args))))
		(print "Connecting to: " (url-format u))
		(if (defq resp (http-get u))
			(progn
				(print "Status:  " (pfind resp :status) " " (pfind resp :reason))
				(print "Proto:   " (pfind resp :proto))
				(print "Headers:")
				(each (lambda ((k v))
					(print "  " k ": " v))
					(partition (pfind resp :headers) 2))
				(defq body (pfind resp :body))
				(print "\nBody (" (stream-avail body) " bytes):")
				(lines! (const print) body))
			(print "HTTP request failed!"))))