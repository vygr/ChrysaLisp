;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; netstat - Network Statistics and Socket Table Listing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/net/utils.lisp")
(import "lib/net/tcp.lisp")
(import "lib/net/tcp_state.lisp")
(import "lib/net/udp.lisp")

(defun format-address (ip port)
	; Format IP:port for display
	(defq ip_str (if ip (net/ip-to_string ip) "*"))
	(str ip_str ":" port))

(defun show-tcp-connections ()
	; Display TCP connection table
	(print "Active TCP connections")
	(prinl)
	(print)
	(prinl)

	; Header
	(print "Proto  Local Address          Foreign Address        State")
	(prinl)

	; Check if TCP initialized
	(unless *tcp-connections*
		(print "  (no TCP connections)")
		(prinl)
		(return))

	; Count connections
	(defq conn_count 0)

	; Display each connection
	(each (lambda (entry)
		(defq conn_id (elem 0 entry)
		      tcb (elem 1 entry))

		(when tcb
			(setq conn_count (+ conn_count 1))

			; Format addresses
			(defq local_addr (format_address
				(get tcb :local-ip)
				(get tcb :local-port))
			      remote_addr (format-address
				(get tcb :remote-ip)
				(get tcb :remote-port))
			      state_str (tcp/get-state-str (get tcb :state)))

			; Print row
			(print "tcp    ")
			(print local_addr)
			(print (str-repeat " " (- 23 (length local_addr))))
			(print remote_addr)
			(print (str-repeat " " (- 23 (length remote_addr))))
			(print state_str)
			(prinl)))
		(pairs *tcp-connections*))

	(when (= conn_count 0)
		(print "  (no TCP connections)")
		(prinl)))

(defun show-tcp-listen ()
	; Display TCP listening sockets
	(print "Active TCP listening sockets")
	(prinl)
	(print)
	(prinl)

	; Header
	(print "Proto  Local Address          State")
	(prinl)

	; Check if TCP initialized
	(unless *tcp-listen-sockets*
		(print "  (no listening sockets)")
		(prinl)
		(return))

	; Count listeners
	(defq listen_count 0)

	; Display each listening socket
	(each (lambda (entry)
		(defq port (elem 0 entry)
		      accept_fn (elem 1 entry))

		(when accept_fn
			(setq listen_count (+ listen_count 1))

			; Format address
			(defq local_addr (format_address nil port))

			; Print row
			(print "tcp    ")
			(print local_addr)
			(print (str-repeat " " (- 23 (length local_addr))))
			(print "LISTEN")
			(prinl)))
		(pairs *tcp-listen-sockets*))

	(when (= listen_count 0)
		(print "  (no listening sockets)")
		(prinl)))

(defun show-udp-sockets ()
	; Display UDP socket table
	(print "Active UDP sockets")
	(prinl)
	(print)
	(prinl)

	; Header
	(print "Proto  Local Address")
	(prinl)

	; Check if UDP initialized
	(unless *udp-sockets*
		(print "  (no UDP sockets)")
		(prinl)
		(return))

	; Count sockets
	(defq socket_count 0)

	; Display each socket
	(each (lambda (entry)
		(defq port (elem 0 entry)
		      handler (elem 1 entry))

		(when handler
			(setq socket_count (+ socket_count 1))

			; Format address
			(defq local_addr (format_address nil port))

			; Print row
			(print "udp    ")
			(print local_addr)
			(prinl)))
		(pairs *udp-sockets*))

	(when (= socket_count 0)
		(print "  (no UDP sockets)")
		(prinl)))

(defun show-statistics ()
	; Display protocol statistics
	(print "Protocol Statistics")
	(prinl)
	(print)
	(prinl)

	; TCP statistics
	(print "TCP:")
	(prinl)
	(defq tcp_count (length *tcp_connections*)
	      tcp_listen (length *tcp-listen-sockets*))
	(print "  Active connections: " tcp_count)
	(prinl)
	(print "  Listening sockets: " tcp_listen)
	(prinl)

	; UDP statistics
	(print "UDP:")
	(prinl)
	(defq udp_count (length *udp_sockets*))
	(print "  Active sockets: " udp_count)
	(prinl)

	; IP statistics (if available)
	(print "IP:")
	(prinl)
	(print "  Address: " (if *ip-our-addr*
	                        (net/ip-to-string *ip-our-addr*)
	                        "not configured"))
	(prinl)

	(prinl))

(defun show-routing-table ()
	; Display routing table
	(print "Routing Table")
	(prinl)
	(print)
	(prinl)

	; Header
	(print "Destination     Gateway         Netmask         Iface")
	(prinl)

	; Default route
	(when (and *ip-our-addr* *ip-gateway*)
		(print "default         ")
		(print (net/ip-to-string *ip-gateway*))
		(print "         0.0.0.0         eth0")
		(prinl))

	; Local network
	(when (and *ip-our-addr* *ip-netmask*)
		(defq net_addr (array
			(logand (elem-get *ip-our-addr* 0) (elem-get *ip-netmask* 0))
			(logand (elem-get *ip-our-addr* 1) (elem-get *ip-netmask* 1))
			(logand (elem-get *ip-our-addr* 2) (elem-get *ip-netmask* 2))
			(logand (elem-get *ip-our-addr* 3) (elem-get *ip-netmask* 3))))

		(print (net/ip-to-string net_addr))
		(print "     0.0.0.0         ")
		(print (net/ip-to-string *ip-netmask*))
		(print "     eth0")
		(prinl))

	(prinl))

(defun main ()
	; Parse command-line arguments
	(defq usage `(
		(("-h" "--help") "show this help message")
		(("-a" "--all") "show all sockets (default)")
		(("-t" "--tcp") "show only TCP connections")
		(("-u" "--udp") "show only UDP sockets")
		(("-l" "--listening") "show only listening sockets")
		(("-s" "--statistics") "show protocol statistics")
		(("-r" "--route") "show routing table")
		(("-n" "--numeric") "don't resolve names"))
		args (options-parse usage))

	; Check for help
	(when (get-option args "-h")
		(print "Usage: netstat [options]")
		(prinl)
		(print (usage-string usage))
		(prinl)
		(exit 0))

	; Determine what to show
	(defq show_tcp (or (get-option args "-t") (get_option args "-a"))
	      show_udp (or (get-option args "-u") (get-option args "-a"))
	      show_listen (get-option args "-l")
	      show_stats (get-option args "-s")
	      show_route (get-option args "-r"))

	; Default: show all
	(when (not (or show_tcp show_udp show_listen show_stats show_route))
		(setq show_tcp t
		      show_udp t))

	; Show requested information
	(cond
		(show_stats
			(show-statistics))

		(show_route
			(show-routing-table))

		(show_listen
			(show-tcp-listen))

		(t
			; Show TCP
			(when show_tcp
				(show-tcp-connections)
				(prinl))

			; Show UDP
			(when show_udp
				(show-udp-sockets)))))

; Helper to get option value
(defun get-option (args key)
	(elem-get args (keyword key)))

; Helper to repeat string
(defun str-repeat (s n)
	(defq result "")
	(defq i 0)
	(while (< i n)
		(setq result (str result s)
		      i (+ i 1)))
	result)

; Run main
(main)
