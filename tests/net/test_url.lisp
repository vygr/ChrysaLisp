(import "lib/net/url.inc")

(report-header "Network: URL Codec")

; --- Encoding & Decoding ---
(assert-eq "url-encode special chars"
	"hello%20world%20%2F%20test"
	(url-encode "hello world / test"))

(assert-eq "url-decode special chars"
	"hello world / test"
	(url-decode "hello%20world%20%2F%20test"))

(assert-eq "url-encode query mode (+ for space)"
	"a+b+c"
	(url-encode "a b c" :t))

(assert-eq "url-decode query mode (+ to space)"
	"a b c"
	(url-decode "a+b+c" :t))

(assert-eq "url-decode lowercase hex"
	"hello/world"
	(url-decode "hello%2fworld"))

(assert-eq "url-encode unreserved fast path"
	"user_name-1.0~"
	(url-encode "user_name-1.0~"))

(assert-eq "url-decode unreserved fast path"
	"plain_text"
	(url-decode "plain_text"))

; --- Query Parsing & Formatting ---
(defq q (url-query-parse "foo=bar&num=42&space=hello+world"))
(assert-eq "url-query-parse foo" "bar" (pfind q :foo))
(assert-eq "url-query-parse num" "42" (pfind q :num))
(assert-eq "url-query-parse space" "hello world" (pfind q :space))

(assert-eq "url-query-format roundtrip"
	"foo=bar&num=42&space=hello+world"
	(url-query-format q))

(defq q2 (url-query-parse "?a=1&b=2"))
(assert-eq "url-query-parse leading question mark a" "1" (pfind q2 :a))
(assert-eq "url-query-parse leading question mark b" "2" (pfind q2 :b))
(assert-eq "url-query-parse empty string" 0 (length (url-query-parse "")))

; --- URL Parsing ---
(defq u1 (url-parse "http://example.com/index.html"))
(assert-eq "url-parse http scheme" "http" (pfind u1 :scheme))
(assert-eq "url-parse http host" "example.com" (pfind u1 :host))
(assert-eq "url-parse http port" 80 (pfind u1 :port))
(assert-eq "url-parse http path" "/index.html" (pfind u1 :path))

(defq u2 (url-parse "https://alice@api.example.com:8443/v1/query?sort=desc#top"))
(assert-eq "url-parse complex scheme" "https" (pfind u2 :scheme))
(assert-eq "url-parse complex user" "alice" (pfind u2 :user))
(assert-eq "url-parse complex host" "api.example.com" (pfind u2 :host))
(assert-eq "url-parse complex port" 8443 (pfind u2 :port))
(assert-eq "url-parse complex path" "/v1/query" (pfind u2 :path))
(assert-eq "url-parse complex query" "sort=desc" (pfind u2 :query))
(assert-eq "url-parse complex fragment" "top" (pfind u2 :fragment))
(assert-eq "url-parse complex params sort" "desc" (pfind (pfind u2 :params) :sort))

(assert-eq "url-format roundtrip"
	"https://alice@api.example.com:8443/v1/query?sort=desc#top"
	(url-format u2))

(defq u3 (url-parse "example.com"))
(assert-eq "url-parse bare host" "example.com" (pfind u3 :host))
(assert-eq "url-parse bare port" 80 (pfind u3 :port))
(assert-eq "url-parse bare path" "/" (pfind u3 :path))

(assert-eq "url-path-query helper"
	"/v1/query?sort=desc"
	(url-path-query u2))

(assert-eq "url-path-query default path"
	"/index.html"
	(url-path-query u1))

; --- Scheme Port Defaults ---
(assert-eq "url-scheme-port http" 80 (url-scheme-port "http"))
(assert-eq "url-scheme-port https" 443 (url-scheme-port "https"))
(assert-eq "url-scheme-port gemini" 1965 (url-scheme-port "gemini"))
(assert-eq "url-scheme-port default" 80 (url-scheme-port "unknown"))