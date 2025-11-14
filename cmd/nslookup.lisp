;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nslookup - DNS Query Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/net/utils.lisp")
(import "lib/net/dns.lisp")

(defun dns-type-to-string (type)
	; Convert DNS type number to string
	(cond
		((= type dns_type_a) "A")
		((= type dns_type_ns) "NS")
		((= type dns_type_cname) "CNAME")
		((= type dns_type_soa) "SOA")
		((= type dns_type_ptr) "PTR")
		((= type dns_type_mx) "MX")
		((= type dns_type_aaaa) "AAAA")
		((= type dns_type_any) "ANY")
		(t (str "TYPE" type))))

(defun string-to-dns-type (str)
	; Convert string to DNS type number
	(cond
		((= str "A") dns_type_a)
		((= str "NS") dns_type_ns)
		((= str "CNAME") dns_type_cname)
		((= str "SOA") dns_type_soa)
		((= str "PTR") dns_type_ptr)
		((= str "MX") dns_type_mx)
		((= str "AAAA") dns_type_aaaa)
		((= str "ANY") dns_type_any)
		(t dns_type_a)))  ; Default to A

(defun format-rdata (type rdata)
	; Format RDATA for display based on type
	(cond
		((= type dns_type_a)
			; IPv4 address
			(net/ip-to-string rdata))

		((= type dns_type_cname)
			; Canonical name
			rdata)

		((= type dns_type_mx)
			; Mail exchange
			(str (get rdata :preference) " " (get rdata :exchange)))

		(t
			; Unknown - show hex
			"[binary data]")))

(defun print-answer (answer)
	; Print DNS answer record
	(print "Name:    " (get answer :name))
	(prinl)
	(print "Type:    " (dns-type-to-string (get answer :type)))
	(prinl)
	(print "TTL:     " (get answer :ttl))
	(prinl)
	(print "Data:    " (format-rdata (get answer :type) (get answer :rdata)))
	(prinl)
	(prinl))

(defun main ()
	; Parse command-line arguments
	(defq usage `(
		(("-h" "--help") "show this help message")
		(("-t" "--type") "query type (A, CNAME, MX, etc.)" "A")
		(("-s" "--server") "DNS server to query" "8.8.8.8")
		(("-p" "--port") "DNS server port" "53")
		(("-d" "--debug") "show debug information")
		("*" "domain name to query"))
		args (options-parse usage))

	; Check for help or missing domain
	(when (or (get-option args "-h")
	          (< (length (get-option args "*")) 1))
		(print "Usage: nslookup [options] <domain>")
		(prinl)
		(print (usage-string usage))
		(prinl)
		(print)
		(prinl)
		(print "Common query types:")
		(prinl)
		(print "  A      - IPv4 address")
		(prinl)
		(print "  AAAA   - IPv6 address")
		(prinl)
		(print "  CNAME  - Canonical name")
		(prinl)
		(print "  MX     - Mail exchange")
		(prinl)
		(print "  NS     - Name server")
		(prinl)
		(print "  PTR    - Pointer record")
		(prinl)
		(print "  SOA    - Start of authority")
		(prinl)
		(print "  ANY    - All records")
		(prinl)
		(exit 0))

	; Get parameters
	(defq domain (elem-get (get-option args "*") 0)
	      qtype_str (get-option args "-t")
	      server_str (get-option args "-s")
	      server_port (num (get-option args "-p"))
	      debug (get-option args "-d")
	      qtype (string-to-dns-type qtype_str)
	      server_ip (net/string-to-ip server_str))

	; Validate server IP
	(unless server_ip
		(print "Invalid DNS server IP: " server_str)
		(prinl)
		(exit 1))

	; Initialize DNS resolver
	(dns/init (list server_ip))

	; Print query information
	(when debug
		(print "Server:  " server_str)
		(prinl)
		(print "Port:    " server_port)
		(prinl)
		(prinl))

	(print "Querying " domain " for " qtype_str " records...")
	(prinl)
	(prinl)

	; Perform query
	(defq start_time (time)
	      answers (dns/query domain qtype)
	      end_time (time)
	      query_time (- end_time start_time))

	; Check for results
	(if (and answers (> (length answers) 0))
		(progn
			; Print answers
			(print "Name:    " domain)
			(prinl)
			(prinl)

			; Group answers by type
			(defq answer_types (env))

			(each (lambda (answer)
				(defq type (get answer :type)
				      type_str (dns-type-to-string type)
				      type_list (get answer_types (keyword type_str)))

				(unless type_list
					(setq type_list (list)))

				(push type_list answer)
				(def answer_types (keyword type_str) type_list))
				answers)

			; Print each type
			(each (lambda (type_entry)
				(defq type_str (elem 0 type_entry)
				      type_answers (elem 1 type_entry))

				(print (str type_str) " records:")
				(prinl)

				(each (lambda (answer)
					(print "  " (format-rdata (get answer :type)
					                          (get answer :rdata))
					       " (TTL=" (get answer :ttl) ")")
					(prinl))
					type_answers)

				(prinl))
				(pairs answer_types))

			; Print query statistics
			(when debug
				(print "Query time: " (/ query_time 1000) " ms")
				(prinl)))

		; No answers
		(progn
			(print "No records found for " domain)
			(prinl)
			(exit 1))))

; Helper to get option value
(defun get-option (args key)
	(elem-get args (keyword key)))

; Run main
(main)
