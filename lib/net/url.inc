;;;;;;;;;;;;;;;;;;;
; Network URL Codec
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +url_unreserved (char-class "A-Za-z0-9_.~-")
	+cls_pct (char-class "%")
	+cls_pct_plus (char-class "%+")
	+cls_hex (char-class "0-9A-Fa-f"))

; Precomputed 769-byte table: "+" at 0, followed by all 256 "%XX" tokens
(defq +url_rep_table (cat "+" (apply (const cat)
    (map (# (cat "%" (hex-encode (char %0)))) (range 0 256)))))

; Precomputed 256-byte table containing all single byte values (0 to 255)
(defq +all_chars (apply (const cat) (map (const char) (range 0 256))))

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
		(:t 80)))

(defun url-encode (s &optional query_flag)
	; (url-encode str [query_flag]) -> str
	(setd s "")
	(cond
		; Fast-path: rbskip returns 0 if all chars are unreserved
		((= (rbskip +url_unreserved s -1) 0) s)
		(:t (defq idxs (nums) l (length s) i 0)
			(while (< i l)
				(defq j (bskip +url_unreserved s i))
				; 1. Span from s (src1): unreserved characters
				(push idxs i j)
				; 2. Span from +url_rep_table (src2): "+" or "%XX"
				(cond
					((>= j l)
						(push idxs 0 0)
						(setq i l))
					((and query_flag (eql (elem-get s j) " "))
						(push idxs 0 1)
						(setq i (inc j)))
					(:t
						(defq o (inc (* (code s 1 j) 3)))
						(push idxs o (+ o 3))
						(setq i (inc j)))))
			(splice s +url_rep_table idxs))))

(defun url-decode (s &optional query_flag)
	; (url-decode str [query_flag]) -> str
	(setd s "")
	(defq cls (if query_flag +cls_pct_plus +cls_pct))
	(cond
		; Fast-path: rbskipn returns 0 if no encoded chars are present
		((= (rbskipn cls s -1) 0) s)
		(:t (defq idxs (nums) l (length s) i 0)
			(while (< i l)
				(defq j (bskipn cls s i))
				; 1. Span from s (src1): unescaped characters
				(push idxs i j)
				; 2. Span from +all_chars (src2): decoded byte
				(cond
					((>= j l)
						(push idxs 0 0)
						(setq i l))
					((and query_flag (eql (elem-get s j) "+"))
						; Space (ASCII 32) at index 32..33 of +all_chars
						(push idxs 32 33)
						(setq i (inc j)))
					((and (<= (+ j 3) l)
							(bfind (elem-get s (inc j)) +cls_hex)
							(bfind (elem-get s (+ j 2)) +cls_hex))
						; Use native hex-decode directly on the 2-character hex slice
						(defq b (code (hex-decode (slice s (inc j) (+ j 3))) 1))
						(push idxs b (inc b))
						(setq i (+ j 3)))
					(:t (defq c (code s 1 j))
						(push idxs c (inc c))
						(setq i (inc j)))))
			(splice s +all_chars idxs))))

(defun url-query-parse (q_str)
	; (url-query-parse query_str) -> pmap
	(defq res (pmap))
	(when (and q_str (nql q_str ""))
		(if (starts-with "?" q_str)
			(setq q_str (slice q_str 1 -1)))
		(each (lambda (pair)
			(when (nql pair "")
				(defq eq_idx (find "=" pair)
					k_str (url-decode (if eq_idx (slice pair 0 eq_idx) pair) :t)
					val (if eq_idx (url-decode (slice pair (inc eq_idx) -1) :t) "")
					k_sym (sym (if (starts-with ":" k_str) k_str (cat ":" k_str))))
				(pinsert res k_sym val)))
			(split q_str "&")))
	res)

(defun url-query-format (q)
	; (url-query-format q) -> str
	(defq pairs (list)
		items (cond
			((empty? q) (list))
			((list?? (first q)) q)
			((list? q) (partition q 2))
			((def? :tolist q) (. q :tolist))
			((list))))
	(each (lambda ((k v))
		(defq k_str (str k)
			k_clean (if (starts-with ":" k_str) (rest k_str) k_str))
		(push pairs (cat (url-encode k_clean :t) "=" (url-encode (str v) :t))))
		items)
	(join pairs "&"))

(defun url-parse (url_str)
	; (url-parse url_str) -> pmap
	(defq scheme "" user "" host "" port 0 upath "" query "" fragment ""
		s (ifn url_str ""))
	; 1. Fragment (#...)
	(when (defq frag_idx (find "#" s))
		(setq fragment (slice s (inc frag_idx) -1)
			s (slice s 0 frag_idx)))
	; 2. Query (?...)
	(when (defq q_idx (find "?" s))
		(setq query (slice s (inc q_idx) -1)
			s (slice s 0 q_idx)))
	; 3. Scheme (scheme://...)
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
			(setq upath s))
		((nql s "")
			(defq slash_idx (find "/" s) authority "")
			(cond
				(slash_idx
					(setq authority (slice s 0 slash_idx)
						upath (slice s slash_idx -1)))
				(:t (setq authority s upath "/")))
			(when (defq at_pos (rfind "@" authority))
				(setq user (url-decode (slice authority 0 (dec at_pos)))
					authority (slice authority at_pos -1)))
			(cond
				((starts-with "[" authority)
					(when (defq rbr_idx (find "]" authority))
						(setq host (slice authority 1 rbr_idx))
						(when (defq col_pos (rfind ":" authority))
							(when (> (dec col_pos) rbr_idx)
								(setq port (str-to-num (slice authority col_pos -1)))))))
				((defq col_pos (rfind ":" authority))
					(setq host (slice authority 0 (dec col_pos))
						port (str-to-num (slice authority col_pos -1))))
				(:t (setq host authority))))
		(:t (setq upath "/")))
	; 5. Normalization
	(if (= port 0)
		(setq port (if (nql scheme "") (url-scheme-port scheme) 80)))
	(if (and (eql upath "") (nql host ""))
		(setq upath "/"))
	(pmap
		:scheme scheme
		:user user
		:host host
		:port port
		:path upath
		:query query
		:params (url-query-parse query)
		:fragment fragment))

(defun url-path-query (u)
	; (url-path-query u) -> str
	(defq upath (or (pfind u :path) "/")
		query (or (pfind u :query) ""))
	(if (eql upath "") (setq upath "/"))
	(cond
		((nql query "") (cat upath "?" query))
		((and (defq p (pfind u :params)) (nempty? p))
			(cat upath "?" (url-query-format p)))
		(upath)))

(defun url-format (u)
	; (url-format u) -> str
	(defq scheme (or (pfind u :scheme) "")
		user (or (pfind u :user) "")
		host (or (pfind u :host) "")
		port (or (pfind u :port) 0)
		upath (or (pfind u :path) "")
		uquery (or (pfind u :query) "")
		fragment (or (pfind u :fragment) "")
		parts (list))
	(when (nql scheme "")
		(push parts scheme "://"))
	(when (nql user "")
		(push parts (url-encode user) "@"))
	(cond
		((and (find ":" host) (not (starts-with "[" host)))
			(push parts "[" host "]"))
		(:t (push parts host)))
	(when (and (/= port 0)
			(/= port (if (nql scheme "") (url-scheme-port scheme) 80)))
		(push parts ":" (str port)))
	(if (and (eql upath "") (nql host ""))
		(push parts "/")
		(push parts upath))
	(cond
		((nql uquery "")
			(push parts "?" (if (list? uquery) (url-query-format uquery) uquery)))
		((and (defq p (pfind u :params)) (nempty? p))
			(push parts "?" (url-query-format p))))
	(when (nql fragment "")
		(push parts "#" fragment))
	(apply (const cat) parts))

;module
(export-symbols '(url-scheme-port url-encode url-decode url-query-parse
	url-query-format url-parse url-path-query url-format))
(env-pop)