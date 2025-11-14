;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP (Transmission Control Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")

;;;;;;;;;;;;;;;;;;
; TCP State
;;;;;;;;;;;;;;;;;;

(defq *tcp-connections* (env))  ; Active TCP connections
(defq *tcp-listen-sockets* (env))  ; Listening sockets
(defq *tcp-next-port* 49152)  ; Next ephemeral port

(defun tcp/init ()
	; Initialize TCP layer
	(setq *tcp-connections* (env))
	(setq *tcp-listen-sockets* (env))
	(setq *tcp-next-port* 49152)
	; Register TCP as IP protocol handler
	(ip/register-handler ip_proto_tcp tcp/process))

;;;;;;;;;;;;;;;;;;
; TCP Connection ID
;;;;;;;;;;;;;;;;;;

(defun tcp/make-conn-id (local-ip local-port remote-ip remote-port)
	; Create connection identifier string
	(str (net/ip-to-string local-ip) ":" local-port ":"
	     (net/ip-to-string remote-ip) ":" remote-port))

;;;;;;;;;;;;;;;;;;
; TCP Control Block
;;;;;;;;;;;;;;;;;;

(defun tcp/create-tcb (local-ip local-port remote-ip remote-port)
	; Create TCP Control Block
	; Output: TCB environment
	(defq isn (net/random))  ; Initial sequence number

	(env
		:state tcp_state_closed
		:local-ip local-ip
		:local-port local-port
		:remote-ip remote-ip
		:remote-port remote-port

		; Send sequence variables
		:snd-una isn           ; Send unacknowledged
		:snd-nxt isn           ; Send next
		:snd-wnd tcp_default_window  ; Send window
		:snd-wl1 0             ; Segment seq for last window update
		:snd-wl2 0             ; Segment ack for last window update
		:iss isn               ; Initial send sequence

		; Receive sequence variables
		:rcv-nxt 0             ; Receive next
		:rcv-wnd tcp_default_window  ; Receive window
		:irs 0                 ; Initial receive sequence

		; Timers and retransmission
		:rto tcp_timeout_init  ; Retransmission timeout
		:srtt 0                ; Smoothed RTT
		:rttvar 0              ; RTT variance
		:last-ack-time (time)  ; Time of last ACK
		:retrans-count 0       ; Retransmission count

		; Buffers
		:recv-buffer (list)    ; Received data buffer
		:send-buffer (list)    ; Send data buffer
		:retrans-queue (list)  ; Retransmission queue

		; Options
		:mss 1460              ; Maximum segment size
		:conn-id nil))         ; Connection ID

;;;;;;;;;;;;;;;;;;
; TCP Packet Creation
;;;;;;;;;;;;;;;;;;

(defun tcp/create-packet (src-port dst-port seq ack flags window data)
	; Create TCP packet
	; Inputs:
	;   src-port, dst-port - port numbers
	;   seq - sequence number
	;   ack - acknowledgment number
	;   flags - TCP flags
	;   window - window size
	;   data - payload data (byte array)
	; Output: TCP packet (byte array)
	(defq pkt (array)
	      data-offset 5)  ; 5 * 4 = 20 bytes (no options)

	; Source port
	(net/write-u16 pkt 0 src-port)

	; Destination port
	(net/write-u16 pkt 2 dst-port)

	; Sequence number
	(net/write-u32 pkt 4 seq)

	; Acknowledgment number
	(net/write-u32 pkt 8 ack)

	; Data offset (4 bits) + Reserved (4 bits)
	(elem-set pkt 12 (ash data-offset 4))

	; Flags
	(elem-set pkt 13 flags)

	; Window
	(net/write-u16 pkt 14 window)

	; Checksum (will be calculated)
	(net/write-u16 pkt 16 0)

	; Urgent pointer
	(net/write-u16 pkt 18 0)

	; Add data
	(each (# (push pkt %0)) data)

	pkt)

(defun tcp/calculate-checksum (src-ip dst-ip tcp-packet)
	; Calculate TCP checksum with pseudo-header
	; Inputs:
	;   src-ip, dst-ip - IP addresses (4-byte arrays)
	;   tcp-packet - TCP packet (byte array)
	; Output: checksum value

	; Create pseudo-header
	(defq pseudo (array)
	      tcp-len (length tcp-packet))

	; Source IP
	(each (# (push pseudo %0)) src-ip)

	; Destination IP
	(each (# (push pseudo %0)) dst-ip)

	; Zero
	(push pseudo 0)

	; Protocol (TCP = 6)
	(push pseudo ip_proto_tcp)

	; TCP length
	(net/write-u16 pseudo (length pseudo) tcp-len)

	; Add TCP packet
	(each (# (push pseudo %0)) tcp-packet)

	; Calculate checksum
	(net/checksum pseudo 0 (length pseudo)))

;;;;;;;;;;;;;;;;;;
; TCP Packet Parsing
;;;;;;;;;;;;;;;;;;

(defun tcp/parse (data)
	; Parse TCP packet
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) tcp_hdr_min_len)
		nil
		(progn
			(defq data-offset (ash (elem-get data 12) -4)
			      hdr-len (* data-offset 4))

			(if (<= hdr-len (length data))
				(env
					:src-port (net/read-u16 data 0)
					:dst-port (net/read-u16 data 2)
					:seq (net/read-u32 data 4)
					:ack (net/read-u32 data 8)
					:data-offset data-offset
					:flags (elem-get data 13)
					:window (net/read-u16 data 14)
					:checksum (net/read-u16 data 16)
					:urgent (net/read-u16 data 18)
					:options (if (> hdr-len tcp_hdr_min_len)
					            (slice data tcp_hdr_min_len hdr-len)
					            (array))
					:data (slice data hdr-len (length data)))
				nil))))

;;;;;;;;;;;;;;;;;;
; TCP Flag Helpers
;;;;;;;;;;;;;;;;;;

(defun tcp/has-flag (flags flag)
	; Check if TCP flags contain specific flag
	(not (= 0 (logand flags flag))))

(defun tcp/flags-str (flags)
	; Convert TCP flags to string representation
	(defq parts (list))
	(when (tcp/has-flag flags tcp_flag_fin) (push parts "FIN"))
	(when (tcp/has-flag flags tcp_flag_syn) (push parts "SYN"))
	(when (tcp/has-flag flags tcp_flag_rst) (push parts "RST"))
	(when (tcp/has-flag flags tcp_flag_psh) (push parts "PSH"))
	(when (tcp/has-flag flags tcp_flag_ack) (push parts "ACK"))
	(when (tcp/has-flag flags tcp_flag_urg) (push parts "URG"))
	(join "," parts))

;;;;;;;;;;;;;;;;;;
; TCP Sequence Number Arithmetic
;;;;;;;;;;;;;;;;;;

(defun tcp/seq-lt (a b)
	; Sequence number less than (handles wraparound)
	(< (logand (- a b) 0xFFFFFFFF) 0x80000000))

(defun tcp/seq-gt (a b)
	; Sequence number greater than
	(tcp/seq-lt b a))

(defun tcp/seq-leq (a b)
	; Sequence number less than or equal
	(or (= a b) (tcp/seq-lt a b)))

(defun tcp/seq-geq (a b)
	; Sequence number greater than or equal
	(or (= a b) (tcp/seq-gt a b)))

;;;;;;;;;;;;;;;;;;
; TCP Connection Management
;;;;;;;;;;;;;;;;;;

(defun tcp/connect (dst-ip dst-port)
	; Initiate TCP connection (client)
	; Inputs: dst-ip - destination IP, dst-port - destination port
	; Output: TCB or nil if failed

	; Allocate local port
	(defq local-port *tcp-next-port*)
	(setq *tcp-next-port* (+ *tcp-next-port* 1))
	(when (>= *tcp-next-port* 65536)
		(setq *tcp-next-port* 49152))

	; Create TCB
	(defq tcb (tcp/create-tcb (ip/get-addr) local-port dst-ip dst-port)
	      conn-id (tcp/make-conn-id (ip/get-addr) local-port dst-ip dst-port))

	(elem-set tcb :conn-id conn-id)
	(elem-set tcb :state tcp_state_syn_sent)

	; Store connection
	(elem-set *tcp-connections* conn-id tcb)

	; Send SYN
	(tcp/send-packet tcb tcp_flag_syn (array))

	; Increment sequence number
	(elem-set tcb :snd-nxt (+ (elem-get tcb :snd-nxt) 1))

	tcb)

(defun tcp/listen (port accept-fn)
	; Listen for incoming TCP connections (server)
	; Inputs: port - port to listen on, accept-fn - callback for new connections
	; Output: t if success, nil if port in use
	(if (elem-get *tcp-listen-sockets* port)
		nil
		(progn
			(elem-set *tcp-listen-sockets* port accept-fn)
			t)))

(defun tcp/close (tcb)
	; Close TCP connection
	; Input: tcb - TCP control block
	(defq state (elem-get tcb :state))

	(cond
		((= state tcp_state_established)
			; Send FIN
			(elem-set tcb :state tcp_state_fin_wait_1)
			(tcp/send-packet tcb tcp_flag_fin (array))
			(elem-set tcb :snd-nxt (+ (elem-get tcb :snd-nxt) 1)))

		((= state tcp_state_close_wait)
			; Send FIN
			(elem-set tcb :state tcp_state_last_ack)
			(tcp/send-packet tcb tcp_flag_fin (array))
			(elem-set tcb :snd-nxt (+ (elem-get tcb :snd-nxt) 1)))

		(t
			; Immediately close
			(elem-set tcb :state tcp_state_closed)
			(elem-set *tcp-connections* (elem-get tcb :conn-id) nil))))

;;;;;;;;;;;;;;;;;;
; TCP Sending
;;;;;;;;;;;;;;;;;;

(defun tcp/send-packet (tcb flags data)
	; Send TCP packet
	; Inputs: tcb - TCP control block, flags - TCP flags, data - payload
	; Output: IP packet or nil

	(defq tcp-pkt (tcp/create-packet
		(elem-get tcb :local-port)
		(elem-get tcb :remote-port)
		(elem-get tcb :snd-nxt)
		(elem-get tcb :rcv-nxt)
		(logior flags (if (> (elem-get tcb :rcv-nxt) 0) tcp_flag_ack 0))
		(elem-get tcb :rcv-wnd)
		data))

	; Calculate checksum
	(defq cksum (tcp/calculate-checksum
		(elem-get tcb :local-ip)
		(elem-get tcb :remote-ip)
		tcp-pkt))
	(net/write-u16 tcp-pkt 16 cksum)

	; Send via IP layer
	(ip/send-packet (elem-get tcb :remote-ip) ip_proto_tcp tcp-pkt))

(defun tcp/send-data (tcb data)
	; Send data over established TCP connection
	; Inputs: tcb - TCP control block, data - data to send (byte array)
	; Output: t if sent, nil if failed

	(if (= (elem-get tcb :state) tcp_state_established)
		(progn
			; Send packet with PSH flag
			(tcp/send-packet tcb tcp_flag_psh data)

			; Update sequence number
			(elem-set tcb :snd-nxt
				(+ (elem-get tcb :snd-nxt) (length data)))

			; Add to retransmission queue
			(push (elem-get tcb :retrans-queue)
				(env :seq (elem-get tcb :snd-nxt)
				     :data data
				     :time (time)))

			t)
		nil))

(defun tcp/send-ack (tcb)
	; Send ACK packet
	; Input: tcb - TCP control block
	(tcp/send-packet tcb 0 (array)))

(defun tcp/send-rst (tcb)
	; Send RST packet
	; Input: tcb - TCP control block
	(tcp/send-packet tcb tcp_flag_rst (array)))

(continued in next message...)
