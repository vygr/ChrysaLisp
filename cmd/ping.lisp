;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ping - ICMP Echo Request/Reply Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/net/utils.inc")
(import "lib/net/ip.inc")
(import "lib/net/icmp.inc")

(defun main ()
	; Parse command-line arguments
	(defq usage `(
		(("-h" "--help") "show this help message")
		(("-c" "--count") "number of pings to send" "4")
		(("-i" "--interval") "interval between pings in seconds" "1")
		(("-w" "--timeout") "timeout for each ping in seconds" "5")
		(("-s" "--size") "size of ping payload in bytes" "56")
		("*" "target IP address or hostname"))
		args (options-parse usage))

	; Check for help or missing target
	(when (or (get-option args "-h")
	          (< (length (get-option args "*")) 1))
		(print "Usage: ping [options] <target>")
		(prinl)
		(print (usage-string usage))
		(prinl)
		(exit 0))

	; Get parameters
	(defq target_str (elem-get (get_option args "*") 0)
	      count (num (get-option args "-c"))
	      interval (num (get-option args "-i"))
	      timeout (num (get-option args "-w"))
	      payload_size (num (get-option args "-s"))
	      target_ip (net/string-to-ip target_str))

	; Validate IP address
	(unless target_ip
		(print "ping: invalid IP address: " target_str)
		(prinl)
		(exit 1))

	; Initialize ICMP if not already done
	(unless (get *icmp-initialized* :icmp)
		(icmp/init)
		(def *icmp-initialized* :icmp t))

	; Ping state
	(defq ping_id (net/random_range 1 65536)
	      seq 0
	      sent 0
	      received 0
	      min_rtt nil
	      max_rtt nil
	      total_rtt 0
	      start_time (time))

	; Register echo reply handler
	(defq reply_handler (lambda (src_ip id seq_num data)
		(when (= id ping_id)
			(defq rtt (- (time) (elem-get *ping-state* :send_time))
			      rtt_ms (/ rtt 1000.0))

			; Update statistics
			(setq received (+ received 1)
			      total_rtt (+ total_rtt rtt)
			      min_rtt (if min_rtt (min min_rtt rtt_ms) rtt_ms)
			      max_rtt (if max_rtt (max max_rtt rtt_ms) rtt_ms))

			; Print reply
			(print "64 bytes from " (net/ip-to-string src_ip)
			       ": icmp_seq=" seq_num
			       " ttl=64"
			       " time=" rtt_ms " ms")
			(prinl)

			; Mark as received
			(def *ping-state* :received t))))

	(icmp/register-echo-handler reply_handler)

	; Print header
	(print "PING " target_str " (" (net/ip-to-string target_ip) ") "
	       payload_size " bytes of data")
	(prinl)

	; Send pings
	(defq *ping-state* (env :send_time 0 :received nil))

	(while (< sent count)
		(setq seq (+ seq 1)
		      sent (+ sent 1))

		; Create payload
		(defq data (array))
		(defq i 0)
		(while (< i payload_size)
			(push data (logand (+ i 0x20) 0xFF))
			(setq i (+ i 1)))

		; Record send time
		(def *ping-state* :send-time (time))
		(def *ping-state* :received nil)

		; Send ping
		(icmp/send-ping target_ip ping_id seq data)

		; Wait for reply with timeout
		(defq timeout_us (* timeout 1000000)
		      wait_start (time))

		(while (and (not (get *ping-state* :received))
		           (< (- (time) wait_start) timeout_us))
			(task-sleep 10000))  ; Sleep 10ms

		; Check if timeout
		(unless (get *ping-state* :received)
			(print "Request timeout for icmp_seq=" seq)
			(prinl))

		; Sleep for interval (if not last ping)
		(when (< sent count)
			(task-sleep (* interval 1000000))))

	; Print statistics
	(prinl)
	(print "--- " target_str " ping statistics ---")
	(prinl)

	(defq packet_loss (* (/ (- sent received) sent) 100.0))
	(print sent " packets transmitted, " received " received, "
	       packet_loss "% packet loss, time "
	       (/ (- (time) start_time) 1000) "ms")
	(prinl)

	; Round-trip time statistics
	(when (> received 0)
		(defq avg_rtt (/ total_rtt received 1000.0))
		(print "rtt min/avg/max = " min_rtt "/" avg_rtt "/" max_rtt " ms")
		(prinl))

	; Exit with appropriate code
	(exit (if (> received 0) 0 1)))

; Helper to get option value
(defun get-option (args key)
	(elem-get args (keyword key)))

; Run main
(main)
