(import "lib/options/options.inc")
(import "lib/net/http.inc")
(import "lib/net/url.inc")

(defq usage `(
(("-h" "--help")
"Usage: curl [options] <url>

    options:
        -h --help: this help info.
        -i --include: include protocol response headers in output.
        -I --head: fetch headers only (HTTP HEAD).
        -s --silent: silent mode (suppress error/diagnostic messages).
        -X --request cmd: specify request command to use (GET, POST, HEAD).
        -H --header line: custom header to pass to server.
        -d --data str: HTTP POST data.

    Fetch and display content from an HTTP URL.")
(("-i" "--include") ,(opt-flag 'opt_i))
(("-I" "--head") ,(opt-flag 'opt_head))
(("-s" "--silent") ,(opt-flag 'opt_s))
(("-X" "--request") ,(opt-str 'opt_X))
(("-H" "--header") ,(static-qq (lambda (args arg)
	(push opt_headers (first args)) (rest args))))
(("-d" "--data") ,(opt-str 'opt_d))
))

(defun curl-fetch (url opt_i opt_head opt_s opt_X opt_headers opt_d)
	(defq u (trim url))
	(when (> (length u) 0)
		(unless (or (starts-with "http://" u) (starts-with "https://" u))
			(setq u (cat "http://" u)))
		(defq method (cond
			(opt_head "HEAD")
			(opt_X (to-upper opt_X))
			(opt_d "POST")
			(:t "GET")))
		(defq headers (pmap))
		(each (lambda (hdr)
			(when (defq idx (find ":" hdr))
				(defq k (trim (slice hdr 0 idx))
					v (trim (slice hdr (inc idx) -1)))
				(pinsert headers (sym (cat ":" (to-lower k))) v)))
			opt_headers)
		(defq resp (http-request method u headers opt_d))
		(if resp
			(progn
				(when (or opt_i opt_head)
					(defq status (pfind resp :status)
						reason (pfind resp :reason)
						proto (pfind resp :proto))
					(write-line (io-stream 'stdout) (cat proto " " (str status) " " reason))
					(each (lambda ((k v))
						(defq ks (str k)
							kname (if (starts-with ":" ks) (rest ks) ks))
						(write-line (io-stream 'stdout) (cat kname ": " v)))
						(partition (pfind resp :headers) 2))
					(write-line (io-stream 'stdout) ""))
				(unless opt_head
					(defq body_str (http-body-str resp))
					(when (> (length body_str) 0)
						(write-blk (io-stream 'stdout) body_str)))
				(stream-flush (io-stream 'stdout)))
			(unless opt_s
				(write-line (io-stream 'stdout) (cat "curl: failed to fetch " u))
				(stream-flush (io-stream 'stdout))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_i :nil opt_head :nil opt_s :nil opt_X :nil opt_headers (list) opt_d :nil
				args (options stdio usage)))
		(if (<= (length args) 1)
			;read urls from stdin
			(lines! (lambda (line) (curl-fetch line opt_i opt_head opt_s opt_X opt_headers opt_d))
				(io-stream 'stdin))
			;read urls from args
			(each (lambda (url) (curl-fetch url opt_i opt_head opt_s opt_X opt_headers opt_d))
				(rest args)))))
