;;;;;;;;;;;;;;;;;;;
; lib/net/http.inc
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
; HTTP/1.1 Client
;;;;;;;;;;;;;;;;;;;

(import "lib/net/url.inc")
(import "service/net/app.inc")

;module
(env-push)

(defq +crlf "\r\n"
	+http_buf_size 4096)

(defun http-write-crlf (stream line)
	(write-blk stream (cat line +crlf)))

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

(defun http-read-body (in headers &optional dest_stream)
	; (http-read-body in headers [dest_stream]) -> body_str | dest_stream
	(defq out (ifn dest_stream (string-stream (cat "")))
		te (or (pfind headers :transfer-encoding) "")
		cl (or (pfind headers :content-length) :nil))
	(cond
		; 1. Chunked Transfer Encoding
		((find "chunked" te)
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
	(if dest_stream dest_stream (str out)))

(defun http-request (method url &optional headers body dest_stream)
	; (http-request method url [headers body dest_stream]) -> pmap | :nil
	(defq u (if (str? url) (url-parse url) url)
		host (pfind u :host)
		port (pfind u :port)
		path_query (url-path-query u)
		res :nil)
	(when (and host (> (length host) 0))
		(when (defq conn (net-open-rpc host port))
			(bind '(in out) conn)
			; 1. Request Line
			(http-write-crlf out (cat (to-upper method) " " path_query " HTTP/1.1"))
			; 2. Host Header
			(if (and (/= port 80) (/= port 443))
				(http-write-crlf out (cat "Host: " host ":" port))
				(http-write-crlf out (cat "Host: " host)))
			(http-write-crlf out "User-Agent: ChrysaLisp/1.0")
			(http-write-crlf out "Connection: close")
			(http-write-crlf out "Accept: */*")
			; 3. User Headers
			(when headers
				(each (lambda ((k v))
					(defq k_str (str k)
						k_name (if (starts-with ":" k_str) (rest k_str) k_str))
					(http-write-crlf out (cat k_name ": " v)))
					(partition headers 2)))
			; 4. Body Length & Dispatch
			(cond
				((and (str? body) (> (length body) 0))
					(unless (pfind headers :content-length)
						(http-write-crlf out (cat "Content-Length: " (length body))))
					(http-write-crlf out "")
					(write-blk out body))
				(:t
					(http-write-crlf out "")))
			(stream-flush out)
			; 5. Read Status Line (skipping optional leading empty lines)
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
			(net-close-rpc conn)))
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
	http-read-headers http-read-body))
(env-pop)