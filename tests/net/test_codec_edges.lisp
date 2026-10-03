(import "lib/net/json.inc")
(import "lib/net/url.inc")

(report-header "JSON & URL Edges: empty input, nesting, escapes, bad percent codes")

; --- json-parse ---
(test-cases
	(json-parse "") :nil
	(json-parse "[]") '()
	(json-parse "[[]]") '(())
	(json-parse "[1,[2,[3]]]") '(1 (2 (3)))
	(json-parse "  42  ") 42
	(json-parse "0") 0
	(json-parse "-0") 0
	(json-parse "1.5") 1.5
	(json-parse "\q\q") ""
	(json-parse "[null,true,false]") '(:nil :t :nil)
	(length (json-parse "{}")) 0)

(assert-true "json-parse {} is a pmap" (pmap? (json-parse "{}")))

; --- json-stringify ---
(test-cases
	(json-stringify (list)) "[]"
	(json-stringify (pmap)) "{}"
	(json-stringify "") "\q\q"
	(json-stringify -1) "-1"
	(json-stringify (list 1 (list 2) "a" :nil :t)) "[1,[2],\qa\q,null,true]"
	(json-stringify "plain") "\qplain\q"
	;a string with the letter q in it, this once gave :nil
	(json-stringify "quick") "\qquick\q"
	(json-stringify "a\qb") "\qa\\\qb\q"
	(json-stringify "a\nb") "\qa\\nb\q"
	(json-stringify "back\\slash") "\qback\\\\slash\q")

; --- round trips ---
(test-cases
	(json-stringify (json-parse "{\qa\q:{\qb\q:[1,2]}}")) "{\qa\q:{\qb\q:[1,2]}}"
	(json-parse (json-stringify "say \qhi\q quickly")) "say \qhi\q quickly"
	(json-parse (json-stringify "back\\slash q")) "back\\slash q"
	(json-parse (json-stringify (list 1 (list) "x"))) '(1 () "x"))

; --- url-encode and url-decode ---
(test-cases
	(url-encode "") ""
	(url-decode "") ""
	(url-encode "%") "%25"
	(url-encode "a/b") "a%2Fb"
	(url-decode "%41") "A"
	(url-decode "a%20") "a "
	;a % that is not a valid code is left as it is
	(url-decode "%") "%"
	(url-decode "%4") "%4"
	(url-decode "%zz") "%zz"
	(url-decode (url-encode "100% & more/less?")) "100% & more/less?")

; --- url-parse fills in what is missing ---
(defun ce-url (url &rest keys)
	(defq u (url-parse url))
	(map (# (pfind u %0)) keys))

(test-cases
	(ce-url "" :scheme :host :port :path :query :fragment) '("" "" 80 "/" "" "")
	(ce-url "http://host" :scheme :host :port :path) '("http" "host" 80 "/")
	(ce-url "http://host/" :host :path) '("host" "/")
	(ce-url "http://host:8080/a/b?x=1&y=2#frag" :host :port :path :query :fragment)
		'("host" 8080 "/a/b" "x=1&y=2" "frag")
	(ce-url "host/path" :scheme :host :path) '("" "host" "/path")
	(ce-url "/just/path" :host :path) '("" "/just/path"))

(defq ce_params (pfind (url-parse "http://host/?x=1&y=2") :params))
(assert-eq "url-parse params x" "1" (pfind ce_params :x))
(assert-eq "url-parse params y" "2" (pfind ce_params :y))

; --- url query strings ---
(defun ce-query (query &rest keys)
	(defq q (url-query-parse query))
	(map (# (pfind q %0)) keys))

(test-cases
	(length (url-query-parse "")) 0
	(length (url-query-parse "&&")) 0
	(ce-query "a=1" :a) '("1")
	;a key with no value has the empty string
	(ce-query "a=1&b" :a :b) '("1" "")
	(ce-query "a=&b=2" :a :b) '("" "2")
	;the last of a repeated key wins
	(ce-query "a=1&a=2" :a) '("2")
	(url-query-format (url-query-parse "a=1&b=2")) "a=1&b=2"
	(url-scheme-port "http") 80
	(url-scheme-port "https") 443
	(url-scheme-port "ftp") 21)
