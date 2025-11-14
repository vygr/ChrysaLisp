;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DNS (Domain Name System) Resolver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")
(import "lib/net/udp.lisp")

;;;;;;;;;;;;;;;;;;
; DNS Constants
;;;;;;;;;;;;;;;;;;

(defcvar 'dns_port 53)
(defcvar 'dns_max_name 255)
(defcvar 'dns_max_label 63)

; DNS Query Types
(defcvar 'dns_type_a 1)        ; Host address
(defcvar 'dns_type_ns 2)       ; Authoritative name server
(defcvar 'dns_type_cname 5)    ; Canonical name
(defcvar 'dns_type_soa 6)      ; Start of authority
(defcvar 'dns_type_ptr 12)     ; Domain name pointer
(defcvar 'dns_type_mx 15)      ; Mail exchange
(defcvar 'dns_type_aaaa 28)    ; IPv6 address
(defcvar 'dns_type_any 255)    ; All records

; DNS Query Classes
(defcvar 'dns_class_in 1)      ; Internet

; DNS Response Codes
(defcvar 'dns_rcode_ok 0)
(defcvar 'dns_rcode_format_error 1)
(defcvar 'dns_rcode_server_failure 2)
(defcvar 'dns_rcode_name_error 3)
(defcvar 'dns_rcode_not_implemented 4)
(defcvar 'dns_rcode_refused 5)

;;;;;;;;;;;;;;;;;;
; DNS State
;;;;;;;;;;;;;;;;;;

(defq *dns-servers* (list))    ; List of DNS server IPs
(defq *dns-cache* (env))       ; DNS response cache
(defq *dns-query-id* 0)        ; Query ID counter

(defun dns/init (servers)
	; Initialize DNS resolver
	; Input: servers - list of DNS server IP addresses (4-byte arrays)
	(setq *dns-servers* servers)
	(setq *dns-cache* (env))
	(setq *dns-query-id* (net/random-range 1 65536))

	; Initialize UDP if not already done
	(unless *udp-sockets*
		(udp/init)))

;;;;;;;;;;;;;;;;;;
; DNS Name Encoding/Decoding
;;;;;;;;;;;;;;;;;;

(defun dns/encode-name (name)
	; Encode domain name to DNS format
	; Example: "www.example.com" -> [3]www[7]example[3]com[0]
	; Input: name - domain name string
	; Output: byte array

	(defq encoded (array)
	      labels (split name "."))

	; Encode each label
	(each (lambda (label)
		(defq len (length label))
		(when (> len dns_max_label)
			(prin "DNS label too long: " label)
			(prinl)
			(return nil))

		; Write label length
		(push encoded len)

		; Write label characters
		(each (# (push encoded (char-code %0)))
		      (explode label)))
		labels)

	; Null terminator
	(push encoded 0)

	encoded)

(defun dns/decode-name (data offset)
	; Decode DNS name from packet
	; Handles compression pointers
	; Returns: (name new_offset)

	(defq name ""
	      pos offset
	      jumped nil
	      jump_pos 0
	      labels (list))

	(while t
		; Read length byte
		(defq len (elem-get data pos))

		; Check for compression pointer (top 2 bits set)
		(if (= (logand len 0xC0) 0xC0)
			; Compression pointer
			(progn
				(defq ptr (logior
					(ash (logand len 0x3F) 8)
					(elem-get data (+ pos 1))))

				(unless jumped
					(setq jump_pos (+ pos 2)
					      jumped t))

				(setq pos ptr))

			; Regular label
			(progn
				(when (= len 0)
					; End of name
					(break))

				; Read label
				(defq label "")
				(defq i 0)
				(while (< i len)
					(setq label (str label
						(code-char (elem-get data (+ pos i 1)))))
					(setq i (+ i 1)))

				(push labels label)
				(setq pos (+ pos len 1)))))

	; Join labels with dots
	(setq name (join "." labels))

	; Return name and new offset
	(list name (if jumped jump_pos pos)))

;;;;;;;;;;;;;;;;;;
; DNS Message Creation
;;;;;;;;;;;;;;;;;;

(defun dns/create-query (domain qtype qclass)
	; Create DNS query message
	; Inputs: domain - domain name string, qtype - query type, qclass - class
	; Output: DNS query packet (byte array)

	(defq pkt (array)
	      query_id *dns-query-id*)

	; Increment query ID
	(setq *dns-query-id* (logand (+ *dns-query-id* 1) 0xFFFF))

	; Header (12 bytes)
	(net/write-u16 pkt 0 query_id)    ; ID
	(net/write-u16 pkt 2 0x0100)      ; Flags: standard query, recursion desired
	(net/write-u16 pkt 4 1)           ; QDCOUNT: 1 question
	(net/write-u16 pkt 6 0)           ; ANCOUNT: 0 answers
	(net/write-u16 pkt 8 0)           ; NSCOUNT: 0 authority
	(net/write-u16 pkt 10 0)          ; ARCOUNT: 0 additional

	; Question section
	(defq encoded_name (dns/encode-name domain))
	(each (# (push pkt %0)) encoded_name)

	; QTYPE and QCLASS
	(defq qtype_offset (length pkt))
	(net/write-u16 pkt qtype_offset qtype)
	(net/write-u16 pkt (+ qtype_offset 2) qclass)

	pkt)

;;;;;;;;;;;;;;;;;;
; DNS Message Parsing
;;;;;;;;;;;;;;;;;;

(defun dns/parse-header (data)
	; Parse DNS header
	; Output: environment with header fields

	(env
		:id (net/read-u16 data 0)
		:flags (net/read-u16 data 2)
		:qdcount (net/read-u16 data 4)
		:ancount (net/read-u16 data 6)
		:nscount (net/read-u16 data 8)
		:arcount (net/read-u16 data 10)
		:qr (ash (net/read-u16 data 2) -15)
		:rcode (logand (net/read-u16 data 2) 0x000F)))

(defun dns/parse-question (data offset)
	; Parse DNS question section
	; Returns: (question new_offset)

	(defq name_result (dns/decode-name data offset)
	      name (elem 0 name_result)
	      pos (elem 1 name_result)
	      qtype (net/read-u16 data pos)
	      qclass (net/read-u16 data (+ pos 2)))

	(list (env :name name :type qtype :class qclass)
	      (+ pos 4)))

(defun dns/parse-answer (data offset)
	; Parse DNS answer/resource record
	; Returns: (answer new_offset)

	(defq name_result (dns/decode-name data offset)
	      name (elem 0 name_result)
	      pos (elem 1 name_result)
	      rtype (net/read-u16 data pos)
	      rclass (net/read-u16 data (+ pos 2))
	      ttl (net/read-u32 data (+ pos 4))
	      rdlength (net/read-u16 data (+ pos 8))
	      rdata_offset (+ pos 10))

	; Parse RDATA based on type
	(defq rdata nil)

	(cond
		((= rtype dns_type_a)
			; IPv4 address
			(setq rdata (slice data rdata_offset (+ rdata_offset 4))))

		((= rtype dns_type_cname)
			; Canonical name
			(setq rdata (elem 0 (dns/decode-name data rdata_offset))))

		((= rtype dns_type_mx)
			; Mail exchange
			(defq preference (net/read-u16 data rdata_offset)
			      mx_name (elem 0 (dns/decode-name data (+ rdata_offset 2))))
			(setq rdata (env :preference preference :exchange mx_name)))

		(t
			; Unknown type - store raw data
			(setq rdata (slice data rdata_offset (+ rdata_offset rdlength)))))

	(list (env
		:name name
		:type rtype
		:class rclass
		:ttl ttl
		:rdata rdata)
	      (+ rdata_offset rdlength)))

;;;;;;;;;;;;;;;;;;
; DNS Query
;;;;;;;;;;;;;;;;;;

(defun dns/query (domain qtype)
	; Perform DNS query
	; Inputs: domain - domain name, qtype - query type
	; Output: list of answers or nil

	; Check cache first
	(defq cache_key (str domain ":" qtype)
	      cached (get *dns-cache* (keyword cache_key)))

	(when cached
		; Check if cache entry is still valid
		(defq age (- (time) (get cached :timestamp)))
		(when (< age (* (get cached :ttl) 1000000))
			(return (get cached :answers))))

	; No valid cache entry - query DNS server
	(when (= (length *dns-servers*) 0)
		(prin "No DNS servers configured")
		(prinl)
		(return nil))

	; Create query packet
	(defq query_pkt (dns/create-query domain qtype dns_class_in)
	      server_ip (elem-get *dns-servers* 0))

	; Create temporary UDP socket for query
	(defq local_port (udp/allocate-port)
	      response nil
	      response_received nil)

	; Set up handler
	(udp/bind local_port
		(lambda (src_ip src_port data)
			(when (and (every eql src_ip server_ip)
			          (= src_port dns_port))
				(setq response data
				      response_received t))))

	; Send query
	(udp/send server_ip local_port dns_port query_pkt)

	; Wait for response (with timeout)
	(defq timeout_us 5000000  ; 5 seconds
	      wait_start (time))

	(while (and (not response_received)
	           (< (- (time) wait_start) timeout_us))
		(task-sleep 10000))

	; Clean up
	(udp/unbind local_port)

	; Parse response
	(unless response
		(return nil))

	(defq header (dns/parse-header response))

	; Check response code
	(when (not (= (get header :rcode) dns_rcode_ok))
		(prin "DNS query failed: response code " (get header :rcode))
		(prinl)
		(return nil))

	; Parse answers
	(defq answers (list)
	      pos 12)  ; After header

	; Skip question section
	(defq i 0)
	(while (< i (get header :qdcount))
		(setq pos (elem 1 (dns/parse-question response pos))
		      i (+ i 1)))

	; Parse answer section
	(setq i 0)
	(while (< i (get header :ancount))
		(defq answer_result (dns/parse-answer response pos)
		      answer (elem 0 answer_result))
		(push answers answer)
		(setq pos (elem 1 answer_result)
		      i (+ i 1)))

	; Cache results
	(when (> (length answers) 0)
		(defq min_ttl 3600)  ; Default 1 hour
		(each (# (setq min_ttl (min min_ttl (get %0 :ttl))))
		      answers)

		(def *dns-cache* (keyword cache_key)
			(env :answers answers
			     :ttl min_ttl
			     :timestamp (time))))

	answers)

;;;;;;;;;;;;;;;;;;
; Convenience Functions
;;;;;;;;;;;;;;;;;;

(defun dns/resolve (domain)
	; Resolve domain to IPv4 address
	; Returns: IP address (4-byte array) or nil

	(defq answers (dns/query domain dns_type_a))

	(when answers
		; Find first A record
		(defq a_record (find (# (= (get %0 :type) dns_type_a)) answers))
		(when a_record
			(return (get a_record :rdata))))

	nil)
