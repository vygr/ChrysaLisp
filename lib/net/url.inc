;;;;;;;;;;;;;;;;;;;
; lib/net/url.inc
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
; Network URL Codec
;;;;;;;;;;;;;;;;;;;

(import "lib/text/searching.inc")

;module
(env-push)

(defq +url_unreserved (char-class "A-Za-z0-9_.~-")
	+cls_pct (char-class "%")
	+cls_pct_plus (char-class "%+")
	+cls_hex (char-class "0-9A-Fa-f"))

(defun url-scheme-port (scheme)
	; (url-scheme-port scheme) -> num
	(case scheme
		("http" 80)
		("https" 443)
		("ws" 80)
		("wss" 443)
		("gemini" 1965)
		("gopher" 70)
		("ftp" 21)
		(80)))

(defun url-encode (s &optional query_flag)
	; (url-encode str [query_flag]) -> str
	(setd s "")
	(defq out (list) l (length s) i 0)
	(while (< i l)
		(defq j (bskip +url_unreserved s i))
		(when (> j i)
			(push out (slice s i j)))
		(cond
			((>= j l)
				(setq i l))
			((and query_flag (eql (elem-get s j) " "))
				(push out "+")
				(setq i (inc j)))
			(:t
				(push out (cat "%" (hex-encode (elem-get s j))))
				(setq i (inc j)))))
	(apply (const cat) out))

(defun url-decode (s &optional query_flag)
	; (url-decode str [query_flag]) -> str
	(setd s "")
	(defq out (list) l (length s) i 0
		cls (if query_flag +cls_pct_plus +cls_pct))
	(while (< i l)
		(defq j (bskipn cls s i))
		(when (> j i)
			(push out (slice s i j)))
		(cond
			((>= j l)
				(setq i l))
			((and query_flag (eql (elem-get s j) "+"))
				(push out " ")
				(setq i (inc j)))
			((and (<= (+ j 3) l)
					(bfind (elem-get s (inc j)) +cls_hex)
					(bfind (elem-get s (+ j 2)) +cls_hex))
				(push out (hex-decode (slice s (inc j) (+ j 3))))
				(setq i (+ j 3)))
			(:t
				(push out (elem-get s j))
				(setq i (inc j)))))
	(apply (const cat) out))

(defun url-query-parse (q_str)
	; (url-query-parse query_str) -> pmap
	(defq res (pmap))
	(when (and q_str (nql q_str ""))
		(if (starts-with "?" q_str)
			(setq q_str (slice q_str 1 -1)))
		(each (lambda (pair)
			(when (nql pair "")
				(defq eq_idx (find "=" pair))
				(if eq_idx
					(defq k_str (url-decode (slice pair 0 eq_idx) :t)
						val (url-decode (slice pair (inc eq_idx) -1) :t))
					(defq k_str (url-decode pair :t)
						val ""))
				(defq k_sym (sym (if (starts-with ":" k_str) k_str (cat ":" k_str))))
				(pinsert res k_sym val)))
			(split q_str "&")))
	res)

(defun url-query-format (q)
	; (url-query-format q) -> str
	(defq pairs (list)
		items (cond
			((empty? q) '())
			((list?? (first q)) q)
			((list? q) (partition q 2))
			((def? :tolist q) (. q :tolist))
			('())))
	(each (lambda ((k v))
		(defq k_str (str k)
			k_clean (if (starts-with ":" k_str) (rest k_str) k_str))
		(push pairs (cat (url-encode k_clean :t) "=" (url-encode (str v) :t))))
		items)
	(join pairs "&"))

(defun url-parse (url_str)
	; (url-parse url_str) -> pmap
	(defq scheme "" user "" host "" port 0 path "" query "" fragment ""
		s (ifn url_str ""))
	; 1. Fragment (#...)
	(when (defq frag_idx (find "#" s))
		(setq fragment (slice s (inc frag_idx) -1)
			s (slice s 0 frag_idx)))
	; 2. Query (?...)
	(when (defq q_idx (find "?" s))
		(setq query (slice s (inc q_idx) -1)
			s (slice s 0 q_idx)))
	; 3. Scheme (scheme://...) via multi-byte substr search
	(cond
		((defq m (first (substr s "://")))
			(bind '(start end) (first m))
			(setq scheme (to-lower (slice s 0 start))
				s (slice s end -1)))
		((starts-with "//" s)
			(setq s (slice s 2 -1))))
	; 4. Authority (user@host:port) vs path
	(cond
		((starts-with "/" s)
			(setq path s))
		((nql s "")
			(defq slash_idx (find "/" s) authority "")
			(if slash_idx
				(setq authority (slice s 0 slash_idx)
					path (slice s slash_idx -1))
				(setq authority s
					path "/"))
			; rfind returns 1-based index (offset + 1)
			(when (defq at_pos (rfind "@" authority))
				(setq user (url-decode (slice authority 0 (dec at_pos)))
					authority (slice authority at_pos -1)))
			(cond
				((starts-with "[" authority)
					(if (defq rbr_idx (find "]" authority))
						(progn
							(setq host (slice authority 1 rbr_idx))
							(if (defq col_pos (rfind ":" authority))
								(when (> (dec col_pos) rbr_idx)
									(setq port (str-to-num (slice authority col_pos -1))))))
						(setq host authority)))
				((defq col_pos (rfind ":" authority))
					(setq host (slice authority 0 (dec col_pos))
						port (str-to-num (slice authority col_pos -1))))
				(:t
					(setq host authority))))
		(:t
			(setq path "/")))
	; 5. Normalization
	(if (= port 0)
		(setq port (if (nql scheme "") (url-scheme-port scheme) 80)))
	(if (and (eql path "") (nql host ""))
		(setq path "/"))
	(pmap
		:scheme scheme
		:user user
		:host host
		:port port
		:path path
		:query query
		:params (url-query-parse query)
		:fragment fragment))

(defun url-path-query (u)
	; (url-path-query u) -> str
	(defq path (or (pfind u :path) "/")
		query (or (pfind u :query) ""))
	(if (eql path "") (setq path "/"))
	(cond
		((nql query "") (cat path "?" query))
		((and (defq p (pfind u :params)) (nempty? p))
			(cat path "?" (url-query-format p)))
		(:t path)))

(defun url-format (u)
	; (url-format u) -> str
	(defq scheme (or (pfind u :scheme) "")
		user (or (pfind u :user) "")
		host (or (pfind u :host) "")
		port (or (pfind u :port) 0)
		path (or (pfind u :path) "")
		query (or (pfind u :query) "")
		fragment (or (pfind u :fragment) "")
		parts (list))
	(when (nql scheme "")
		(push parts scheme "://"))
	(when (nql user "")
		(push parts (url-encode user) "@"))
	(push parts host)
	(when (and (/= port 0)
			(/= port (if (nql scheme "") (url-scheme-port scheme) 80)))
		(push parts ":" (str port)))
	(if (and (eql path "") (nql host ""))
		(push parts "/")
		(push parts path))
	(cond
		((nql query "")
			(push parts "?" (if (list? query) (url-query-format query) query)))
		((and (defq p (pfind u :params)) (nempty? p))
			(push parts "?" (url-query-format p))))
	(when (nql fragment "")
		(push parts "#" fragment))
	(apply (const cat) parts))

;module
(export-symbols '(url-scheme-port url-encode url-decode url-query-parse
	url-query-format url-parse url-path-query url-format))
(env-pop)