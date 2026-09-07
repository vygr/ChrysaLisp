;;;;;;;;;;;;;;;;;;;
; lib/net/http.inc
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
; HTTP/1.1 Client
;;;;;;;;;;;;;;;;;;;

(import "lib/net/url.inc")
(import "lib/text/searching.inc")
(import "service/net/app.inc")

;module
(env-push)

(defq +crlf "\r\n"
	+http_buf_size 4096
	; Private connection pool: host:port -> list of idle (in out) connections
	*http_pool* (Fmap 31))

(defun http-pool-checkout (host port)
	; (http-pool-checkout host port) -> (in out) | :nil
	(defq key (cat host ":" (str port))
		pool (const *http_pool*)
		conns (. pool :find key)
		live_conn :nil)
	(while (and conns (nempty? conns) (not live_conn))
		(defq conn (pop conns)
			in (first conn))
		; Check if connection is still alive (started and no unread errors/EOF)
		(cond
			((and (= (in-get-state in) +stream_mail_state_started)
					(mail-validate (in-mbox in))
					(not (mail-poll (list (in-mbox in)))))
				(setq live_conn conn))
			(:t
				; Dead/timed-out socket, discard cleanly
				(net-close-rpc conn))))
	(or live_conn (net-open-rpc host port)))

(defun http-pool-checkin (host port conn headers)
	; (http-pool-checkin host port conn headers)
	(defq key (cat host ":" (str port))
		pool (const *http_pool*)
		conn_hdr (to-lower (or (pfind headers :connection) "")))
	(cond
		((and conn
				(not (found? conn_hdr "close"))
				(= (in-get-state (first conn)) +stream_mail_state_started)
				(or (pfind headers :content-length)
					(found? (or (pfind headers :transfer-encoding) "") "chunked")))
			; Connection is reusable and framed properly; return to pool
			(. pool :update key (# (if %0 (push %0 conn) (list conn)))))
		(conn
			; Connection: close or unframed body; close socket
			(net-close-rpc conn))))

(defun http-pool-clear ()
	; (http-pool-clear) -> :nil
	; Close and flush all idle pooled sockets
	(defq pool (const *http_pool*))
	(. pool :each (lambda (k conns)
		(each (const net-close-rpc) conns)))
	(. pool :empty)
	:nil)

(defun http-read-headers (in)
	; (http-read-headers in) -> pmap
	(defq headers (pmap))
	(while (and (defq line (read-line in))
				(defq clean (trim-end line "\r"))
				(nql clean ""))
		(when (defq col_idx (find ":" clean))
			(defq k_str (to-lower (trim (slice clean 0 col_idx)))
				v_str (trim (slice clean (inc col_idx) -1))
				k_sym (sym (if (starts-with ":" k_str) k_str (cat ":" k_str))))
			(pinsert headers k_sym v_str)))
	headers)

(defun http-read-body (in &optional headers dest_stream)
	; (http-read-body in [headers dest_stream]) -> stream
	(setd headers (pmap))
	(defq out (ifn dest_stream (memory-stream))
		te (or (pfind headers :transfer-encoding) "")
		cl (pfind headers :content-length))
	(cond
		; 1. Chunked Transfer Encoding
		((found? te "chunked")
			(defq running :t)
			(while running
				(defq chunk_line (read-line in))
				(ifn chunk_line
					(setq running :nil)
					(progn
						; Chunk header: hex size [;ext]
						(defq hex_part (first (split (trim (trim-end chunk_line "\r")) ";"))
							chunk_size (if (nql hex_part "") (str-to-num (cat "0x" hex_part)) 0))
						(cond
							((= chunk_size 0)
								; Read trailers until empty line
								(while (and (defq trailer (read-line in))
											(nql (trim-end trailer "\r") ""))
									:nil)
								(setq running :nil))
							(:t
								(defq rem chunk_size)
								(while (> rem 0)
									(defq blk (read-blk in (min rem +http_buf_size)))
									(ifn blk
										(setq rem 0 running :nil)
										(progn
											(write-blk out blk)
											(setq rem (- rem (length blk))))))
								; Consume trailing CRLF after chunk payload
								(read-line in)))))))
		; 2. Content-Length Framing
		(cl
			(defq rem (str-to-num cl))
			(while (> rem 0)
				(defq blk (read-blk in (min rem +http_buf_size)))
				(ifn blk
					(setq rem 0)
					(progn
						(write-blk out blk)
						(setq rem (- rem (length blk)))))))
		; 3. Connection: close (read until stream EOF)
		(:t
			(while (defq blk (read-blk in +http_buf_size))
				(write-blk out blk))))
	(stream-flush out)
	(if dest_stream
		dest_stream
		(progn
			(stream-seek out 0 0)
			out)))

(defun http-body-str (resp)
	; (http-body-str resp) -> str
	(defq body (pfind resp :body))
	(cond
		((not body) "")
		((str? body) body)
		((and (/= (stream-seek body 0 0) -1) (defq parts (list)))
			(while (defq blk (read-blk body +http_buf_size))
				(push parts blk))
			(stream-seek body 0 0)
			(apply (const cat) parts))
		(:t "")))

(defun http-request (method url &optional headers body dest_stream)
	; (http-request method url [headers body dest_stream]) -> pmap | :nil
	(setd headers (pmap))
	(defq u (if (str? url) (url-parse url) url)
		host (pfind u :host)
		port (pfind u :port)
		path_query (url-path-query u)
		res :nil)
	(when (and host (> (length host) 0))
		(when (defq conn (http-pool-checkout host port))
			(bind '(in out) conn)
			; 1. Assemble Request Headers
			(defq req_lines (list
				(cat (to-upper method) " " path_query " HTTP/1.1")
				(if (and (/= port 80) (/= port 443))
					(str "Host: " host ":" port)
					(cat "Host: " host))
				"User-Agent: ChrysaLisp/1.0"
				"Accept: */*"))
			; 2. User Connection Header (defaults to keep-alive)
			(unless (pfind headers :connection)
				(push req_lines "Connection: keep-alive"))
			; 3. Append User Headers
			(each (lambda ((k v))
				(defq k_str (str k)
					k_name (if (starts-with ":" k_str) (rest k_str) k_str))
				(push req_lines (cat k_name ": " v)))
				(partition headers 2))
			; 4. Append Content-Length if body is present (str conversion!)
			(when (and (str? body) (> (length body) 0))
				(unless (pfind headers :content-length)
					(push req_lines (cat "Content-Length: " (str (length body))))))
			; 5. Send header block in single buffer
			(write-blk out (cat (join req_lines +crlf) +crlf +crlf))
			(when (and (str? body) (> (length body) 0))
				(write-blk out body))
			(stream-flush out)
			; 6. Read Status Line
			(defq status_line (read-line in))
			(while (and status_line (eql (trim-end status_line "\r") ""))
				(setq status_line (read-line in)))
			(when status_line
				(defq parts (split (trim-end status_line "\r") " ")
					proto (first parts)
					status (if (> (length parts) 1) (str-to-num (second parts)) 0)
					reason (if (> (length parts) 2) (join (slice parts 2 -1) " ") "")
					resp_headers (http-read-headers in)
					resp_body (http-read-body in resp_headers dest_stream))
				(setq res (pmap
					:status status
					:reason reason
					:proto proto
					:headers resp_headers
					:body resp_body)))
			; 7. Check back into pool or close socket
			(http-pool-checkin host port conn
				(if res (pfind res :headers) (pmap :connection "close")))))
	res)

(defun http-get (url &optional headers dest_stream)
	; (http-get url [headers dest_stream]) -> pmap | :nil
	(http-request "GET" url headers :nil dest_stream))

(defun http-post (url body &optional headers dest_stream)
	; (http-post url body [headers dest_stream]) -> pmap | :nil
	(http-request "POST" url headers body dest_stream))

(defun http-head (url &optional headers)
	; (http-head url [headers]) -> pmap | :nil
	(http-request "HEAD" url headers :nil :nil))

;module
(export-symbols '(http-request http-get http-post http-head
	http-read-headers http-read-body http-body-str http-pool-clear))
(env-pop)