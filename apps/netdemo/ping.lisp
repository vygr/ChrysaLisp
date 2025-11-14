;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ping Utility - ICMP Echo Request/Reply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")
(import "lib/net/icmp.lisp")

(defun ping/main (args)
	; Simple ping utility
	; Usage: (ping/main '("192.168.1.1"))

	(when (< (length args) 1)
		(prin "Usage: ping <ip-address>")
		(prinl)
		(exit 1))

	(defq target-ip-str (elem_get args 0)
	      target-ip (net/string-to-ip target-ip-str))

	(unless target-ip
		(prin "Invalid IP address: " target-ip-str)
		(prinl)
		(exit 1))

	; Initialize ICMP
	(icmp/init)

	; Ping parameters
	(defq ping-id (net/random_range 1 65536)
	      seq 0
	      replies (list)
	      timeout 5000000)  ; 5 seconds

	; Register echo reply handler
	(icmp/register-echo-handler
		(lambda (src-ip id seq-num data)
			(when (= id ping-id)
				(push replies (env
					:src-ip src-ip
					:seq seq-num
					:time (time)
					:data data)))))

	(prin "PING " target-ip-str)
	(prinl)

	; Send 4 ping requests
	(defq count 0)
	(while (< count 4)
		(setq seq (+ seq 1))

		; Create ping data
		(defq data (array))
		(defq i 0)
		(while (< i 56)
			(push data (logand (+ i 0x30) 0xFF))
			(setq i (+ i 1)))

		; Record send time
		(defq send_time (time))

		; Send ping
		(icmp/send-ping target-ip ping-id seq data)

		(prin "Sent ICMP echo request to " target-ip-str " seq=" seq)
		(prinl)

		; Wait for reply (simplified - would need real timeout mechanism)
		(task-sleep 1000000)  ; 1 second

		; Check for reply
		(defq reply (find (# (= (elem-get %0 :seq) seq)) replies))
		(if reply
			(progn
				(defq rtt (- (elem-get reply :time) send_time))
				(prin "Reply from " (net/ip-to-string (elem-get reply :src-ip))
				      " seq=" (elem-get reply :seq)
				      " time=" (/ rtt 1000) "ms")
				(prinl))
			(progn
				(prin "Request timeout for seq=" seq)
				(prinl)))

		(setq count (+ count 1)))

	(prin "Ping complete")
	(prinl))

; Export main function
ping/main
