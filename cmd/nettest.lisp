(import "lib/options/options.inc")
(import "lib/net/url.inc")
(import "lib/net/http.inc")

(defq usage `(
(("-h" "--help")
"Usage: nettest [options] [url|host] [port]

    options:
        -h --help: this help info.
        -t --test: run url codec test suite.

    Simple HTTP / Net service test.
    Examples:
        nettest http://example.com/
        nettest http://httpbin.org/get?msg=hello+world
        nettest -t")
(("-t" "--test")
	,(opt-flag 'opt_t))
))

(defun test-check (label cond)
	(if cond
		(print "  [PASS] " label)
		(print "  [FAIL] " label)))

(defun run-url-tests ()
	(print "Running lib/net/url.inc test suite:")
	(test-check "url-encode special chars"
		(eql (url-encode "hello world / test") "hello%20world%20%2F%20test"))
	(test-check "url-decode special chars"
		(eql (url-decode "hello%20world%20%2F%20test") "hello world / test"))
	(test-check "url-encode query mode (+ for space)"
		(eql (url-encode "a b c" :t) "a+b+c"))
	(test-check "url-decode query mode (+ to space)"
		(eql (url-decode "a+b+c" :t) "a b c"))
	(test-check "url-decode lowercase hex"
		(eql (url-decode "hello%2fworld") "hello/world"))
	(defq q (url-query-parse "foo=bar&num=42&space=hello+world"))
	(test-check "url-query-parse fields"
		(and (eql (pfind q :foo) "bar")
			(eql (pfind q :num) "42")
			(eql (pfind q :space) "hello world")))
	(test-check "url-query-format roundtrip"
		(eql (url-query-format q) "foo=bar&num=42&space=hello+world"))
	(defq u1 (url-parse "http://example.com/index.html"))
	(test-check "url-parse full http url"
		(and (eql (pfind u1 :scheme) "http")
			(eql (pfind u1 :host) "example.com")
			(= (pfind u1 :port) 80)
			(eql (pfind u1 :path) "/index.html")))
	(defq u2 (url-parse "https://alice@api.example.com:8443/v1/query?sort=desc#top"))
	(test-check "url-parse complex url"
		(and (eql (pfind u2 :scheme) "https")
			(eql (pfind u2 :user) "alice")
			(eql (pfind u2 :host) "api.example.com")
			(= (pfind u2 :port) 8443)
			(eql (pfind u2 :path) "/v1/query")
			(eql (pfind u2 :query) "sort=desc")
			(eql (pfind u2 :fragment) "top")))
	(test-check "url-format roundtrip"
		(eql (url-format u2) "https://alice@api.example.com:8443/v1/query?sort=desc#top"))
	(defq u3 (url-parse "example.com"))
	(test-check "url-parse bare host"
		(and (eql (pfind u3 :host) "example.com")
			(= (pfind u3 :port) 80)
			(eql (pfind u3 :path) "/")))
	(test-check "url-path-query helper"
		(eql (url-path-query u2) "/v1/query?sort=desc"))
	(print "Test suite completed.\n"))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_t :nil args (options stdio usage)))
		(defq stdout (io-stream 'stdout))
		(cond
			(opt_t
				(run-url-tests))
			(:t
				(defq target (if (> (length args) 1) (second args) "http://example.com/")
					u (url-parse target))
				(if (> (length args) 2)
					(pinsert u :port (str-to-num (third args))))
				(print "Connecting to: " (url-format u))
				(stream-flush stdout)
				(if (defq resp (http-get u))
					(progn
						(print "Status:  " (pfind resp :status) " " (pfind resp :reason))
						(stream-flush stdout)
						(print "Proto:   " (pfind resp :proto))
						(stream-flush stdout)
						(print "Headers:")
						(stream-flush stdout)
						(each (lambda ((k v))
							(print "  " k ": " v))
							(partition (pfind resp :headers) 2))
						(print "\nBody (" (length (pfind resp :body)) " bytes):")
						(stream-flush stdout)
						(print (pfind resp :body))
						(stream-flush stdout))
					(progn
						(print "HTTP request failed!")
						(stream-flush stdout)))))))