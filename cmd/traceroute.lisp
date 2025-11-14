;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; traceroute - Trace Route to Network Host
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")
(import "lib/net/icmp.lisp")
(import "lib/net/udp.lisp")

(defun main ()
	; Parse command-line arguments
	(defq usage `(
		(("-h" "--help") "show this help message")
		(("-m" "--max-hops") "maximum number of hops" "30")
		(("-q" "--queries") "number of queries per hop" "3")
		(("-w" "--wait") "wait time per query in seconds" "5")
		(("-p" "--port") "base destination port" "33434")
		(("-I" "--icmp") "use ICMP ECHO instead of UDP")
		("*" "target IP address"))
		args (options-parse usage))

	; Check for help or missing target
	(when (or (get-option args "-h")
	          (< (length (get-option args "*")) 1))
		(print "Usage: traceroute [options] <target>")
		(prinl)
		(print (usage-string usage))
		(prinl)
		(exit 0))

	; Get parameters
	(defq target_str (elem-get (get_option args "*") 0)
	      max_hops (num (get-option args "-m"))
	      queries (num (get-option args "-q"))
	      wait_sec (num (get-option args "-w"))
	      base_port (num (get-option args "-p"))
	      use_icmp (get-option args "-I")
	      target_ip (net/string-to-ip target_str))

	; Validate IP address
	(unless target_ip
		(print "traceroute: invalid IP address: " target_str)
		(prinl)
		(exit 1))

	; Initialize ICMP
	(unless (get *icmp-initialized* :icmp)
		(icmp/init)
		(def *icmp-initialized* :icmp t))

	; Initialize UDP if using UDP probes
	(unless use_icmp
		(unless (get *udp-initialized* :udp)
			(udp/init)
			(def *udp-initialized* :udp t)))

	; Traceroute state
	(defq seq 0
	      reached_target nil)

	; Register ICMP handlers for time exceeded and echo reply
	(defq *trace-state* (env :reply-ip nil :reply_type nil))

	(icmp/register-echo-handler
		(lambda (src_ip id seq_num data)
			(def *trace-state* :reply-ip src_ip)
			(def *trace-state* :reply-type icmp_echo_reply)))

	; Note: We would also need to register a handler for ICMP Time Exceeded
	; For now, this is a simplified version

	; Print header
	(print "traceroute to " target_str " (" (net/ip-to-string target_ip) "), "
	       max_hops " hops max")
	(prinl)

	; Main traceroute loop
	(defq ttl 1)
	(while (and (<= ttl max_hops) (not reached_target))
		(print (str ttl) "  ")

		; Send queries for this hop
		(defq hop_found nil
		      hop_ip nil
		      hop_rtts (list))

		(defq query 0)
		(while (< query queries)
			(setq seq (+ seq 1))

			; Reset state
			(def *trace-state* :reply-ip nil)
			(def *trace-state* :reply-type nil)

			; Send probe
			(defq send_time (time))

			(if use_icmp
				; ICMP probe
				(progn
					(defq probe_id (net/random_range 1 65536)
					      probe_data (array 0x00))
					(icmp/send-ping target_ip probe_id seq probe_data ttl))

				; UDP probe
				(progn
					(defq probe_port (+ base_port seq)
					      probe_data (array 0x00))
					; Would send UDP packet with specific TTL
					; For now, skip UDP implementation
					))

			; Wait for reply
			(defq timeout_us (* wait_sec 1000000)
			      wait_start (time))

			(while (and (not (get *trace-state* :reply-ip))
			           (< (- (time) wait_start) timeout_us))
				(task-sleep 10000))

			; Process reply
			(defq reply_ip (get *trace-state* :reply_ip))

			(if reply_ip
				(progn
					; Calculate RTT
					(defq rtt (- (time) send_time)
					      rtt_ms (/ rtt 1000.0))

					; Store hop info
					(unless hop_ip
						(setq hop_ip reply_ip
						      hop_found t))

					(push hop_rtts rtt_ms)

					; Check if we reached target
					(when (every eql reply_ip target_ip)
						(setq reached_target t)))

				; Timeout
				(print "* "))

			(setq query (+ query 1)))

		; Print hop results
		(if hop_found
			(progn
				(print (net/ip-to-string hop_ip) "  ")

				; Print RTTs
				(each (lambda (rtt)
					(print rtt " ms  "))
				      hop_rtts)

				(prinl))

			(progn
				(print "* * *")
				(prinl)))

		(setq ttl (+ ttl 1)))

	; Print completion message
	(if reached_target
		(progn
			(print)
			(prinl)
			(print "Trace complete")
			(prinl))
		(progn
			(print)
			(prinl)
			(print "Trace did not reach target within " max_hops " hops")
			(prinl))))

; Helper to get option value
(defun get-option (args key)
	(elem-get args (keyword key)))

; Run main
(main)
