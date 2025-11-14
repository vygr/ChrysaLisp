;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP State Machine and Reception
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/tcp.lisp")

;;;;;;;;;;;;;;;;;;
; TCP State Machine
;;;;;;;;;;;;;;;;;;

(defun tcp/handle-syn (tcb tcp-pkt)
	; Handle SYN in various states
	(defq state (elem-get tcb :state))

	(cond
		((= state tcp_state_listen)
			; Passive open - received SYN
			(elem-set tcb :irs (elem-get tcp-pkt :seq))
			(elem-set tcb :rcv-nxt (+ (elem-get tcp-pkt :seq) 1))
			(elem-set tcb :state tcp_state_syn_received)

			; Send SYN-ACK
			(tcp/send-packet tcb (logior tcp_flag_syn tcp_flag_ack) (array))
			(elem-set tcb :snd-nxt (+ (elem-get tcb :snd-nxt) 1)))

		((= state tcp_state_syn_sent)
			; Simultaneous open or SYN-ACK received
			(elem-set tcb :irs (elem-get tcp-pkt :seq))
			(elem-set tcb :rcv-nxt (+ (elem-get tcp-pkt :seq) 1))

			(if (tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_ack)
				; SYN-ACK received
				(progn
					(elem-set tcb :snd-una (elem-get tcp-pkt :ack))
					(elem-set tcb :state tcp_state_established)
					; Send ACK
					(tcp/send-ack tcb))
				; Just SYN received (simultaneous open)
				(progn
					(elem-set tcb :state tcp_state_syn_received)
					; Send SYN-ACK
					(tcp/send-packet tcb (logior tcp_flag_syn tcp_flag_ack) (array))
					(elem-set tcb :snd-nxt (+ (elem-get tcb :snd-nxt) 1)))))))

(defun tcp/handle-ack (tcb tcp-pkt)
	; Handle ACK in various states
	(defq state (elem-get tcb :state)
	      ack-num (elem-get tcp-pkt :ack))

	(cond
		((= state tcp_state_syn_received)
			; ACK of our SYN
			(when (tcp/seq-gt ack-num (elem-get tcb :snd-una))
				(elem-set tcb :snd-una ack-num)
				(elem-set tcb :state tcp_state_established)))

		((= state tcp_state_established)
			; Normal ACK processing
			(when (tcp/seq-gt ack-num (elem-get tcb :snd-una))
				(elem-set tcb :snd-una ack-num)

				; Remove acknowledged data from retransmission queue
				(elem-set tcb :retrans-queue
					(filter (# (tcp/seq-gt (elem-get %0 :seq) ack-num))
					        (elem-get tcb :retrans-queue)))

				; Update window
				(elem-set tcb :snd-wnd (elem-get tcp-pkt :window))))

		((= state tcp_state_fin_wait_1)
			; ACK of our FIN
			(when (tcp/seq-geq ack-num (elem-get tcb :snd-nxt))
				(elem-set tcb :state tcp_state_fin_wait_2)))

		((= state tcp_state_closing)
			; ACK of our FIN
			(when (tcp/seq-geq ack-num (elem-get tcb :snd-nxt))
				(elem-set tcb :state tcp_state_time_wait)))

		((= state tcp_state_last_ack)
			; ACK of our FIN
			(when (tcp/seq-geq ack-num (elem-get tcb :snd-nxt))
				(elem-set tcb :state tcp_state_closed)
				(elem-set *tcp-connections* (elem-get tcb :conn-id) nil)))))

(defun tcp/handle-fin (tcb tcp-pkt)
	; Handle FIN in various states
	(defq state (elem-get tcb :state))

	; Update receive sequence number
	(elem-set tcb :rcv-nxt (+ (elem-get tcb :rcv-nxt) 1))

	(cond
		((= state tcp_state_established)
			; Received FIN - enter CLOSE_WAIT
			(elem-set tcb :state tcp_state_close_wait)
			; Send ACK
			(tcp/send-ack tcb))

		((= state tcp_state_fin_wait_1)
			; Simultaneous close or FIN received
			(elem-set tcb :state tcp_state_closing)
			; Send ACK
			(tcp/send-ack tcb))

		((= state tcp_state_fin_wait_2)
			; FIN received
			(elem-set tcb :state tcp_state_time_wait)
			; Send ACK
			(tcp/send-ack tcb))))

(defun tcp/handle-rst (tcb tcp-pkt)
	; Handle RST (reset connection)
	(elem-set tcb :state tcp_state_closed)
	(elem-set *tcp-connections* (elem-get tcb :conn-id) nil))

(defun tcp/handle-data (tcb tcp-pkt data-handler)
	; Handle data in ESTABLISHED state
	; Inputs: tcb, tcp-pkt, data-handler - callback function
	(when (= (elem-get tcb :state) tcp_state_established)
		(defq data (elem-get tcp-pkt :data)
		      seq (elem-get tcp-pkt :seq))

		; Check if data is in sequence
		(when (and (= seq (elem-get tcb :rcv-nxt))
		           (> (length data) 0))

			; Add data to receive buffer
			(each (# (push (elem-get tcb :recv-buffer) %0)) data)

			; Update receive sequence number
			(elem-set tcb :rcv-nxt (+ (elem-get tcb :rcv-nxt) (length data)))

			; Send ACK
			(tcp/send-ack tcb)

			; Call data handler if provided
			(when data-handler
				(data-handler tcb data)))))

;;;;;;;;;;;;;;;;;;
; TCP Packet Reception
;;;;;;;;;;;;;;;;;;

(defun tcp/process (src-ip dst-ip data)
	; Process incoming TCP packet
	; Inputs: src-ip, dst-ip - IP addresses (4-byte arrays), data - TCP packet
	; Output: t if processed, nil if error
	(defq tcp-pkt (tcp/parse data))

	(if tcp-pkt
		(progn
			; Verify checksum
			(defq cksum (tcp/calculate-checksum src-ip dst-ip data))
			(when (= cksum 0xFFFF)

				; Find connection or listening socket
				(defq conn-id (tcp/make-conn-id dst-ip
				                                (elem-get tcp-pkt :dst-port)
				                                src-ip
				                                (elem-get tcp-pkt :src-port))
				      tcb (elem-get *tcp-connections* conn-id))

				; If no connection, check for listening socket
				(when (and (not tcb)
				          (tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_syn))
					(defq listen-fn (elem-get *tcp-listen-sockets*
					                          (elem-get tcp-pkt :dst-port)))
					(when listen-fn
						; Create new TCB for incoming connection
						(setq tcb (tcp/create-tcb dst-ip
						                          (elem-get tcp-pkt :dst-port)
						                          src-ip
						                          (elem-get tcp-pkt :src-port)))
						(elem-set tcb :conn-id conn-id)
						(elem-set tcb :state tcp_state_listen)
						(elem-set *tcp-connections* conn-id tcb)

						; Call accept callback
						(listen-fn tcb)))

				; Process packet if we have a TCB
				(when tcb
					; Handle flags in order of priority
					(cond
						; RST - reset connection
						((tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_rst)
							(tcp/handle-rst tcb tcp-pkt))

						; SYN - synchronize
						((tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_syn)
							(tcp/handle-syn tcb tcp-pkt))

						; ACK - acknowledgment
						((tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_ack)
							(tcp/handle-ack tcb tcp-pkt)

							; Handle data if present
							(when (> (length (elem-get tcp-pkt :data)) 0)
								(tcp/handle-data tcb tcp-pkt nil))

							; FIN - finish
							(when (tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_fin)
								(tcp/handle-fin tcb tcp-pkt)))

						; Just FIN
						((tcp/has-flag (elem-get tcp-pkt :flags) tcp_flag_fin)
							(tcp/handle-fin tcb tcp-pkt))))

					t))
		nil))

;;;;;;;;;;;;;;;;;;
; TCP Helper Functions
;;;;;;;;;;;;;;;;;;

(defun tcp/get-state-str (state)
	; Get string representation of TCP state
	(cond
		((= state tcp_state_closed) "CLOSED")
		((= state tcp_state_listen) "LISTEN")
		((= state tcp_state_syn_sent) "SYN_SENT")
		((= state tcp_state_syn_received) "SYN_RECEIVED")
		((= state tcp_state_established) "ESTABLISHED")
		((= state tcp_state_fin_wait_1) "FIN_WAIT_1")
		((= state tcp_state_fin_wait_2) "FIN_WAIT_2")
		((= state tcp_state_close_wait) "CLOSE_WAIT")
		((= state tcp_state_closing) "CLOSING")
		((= state tcp_state_last_ack) "LAST_ACK")
		((= state tcp_state_time_wait) "TIME_WAIT")
		(t "UNKNOWN")))

(defun tcp/is-connected (tcb)
	; Check if TCP connection is established
	(= (elem-get tcb :state) tcp_state_established))

(defun tcp/can-send (tcb)
	; Check if we can send data
	(and (= (elem-get tcb :state) tcp_state_established)
	     (> (elem-get tcb :snd-wnd) 0)))

(defun tcp/bytes-in-flight (tcb)
	; Get number of bytes sent but not yet acknowledged
	(- (elem-get tcb :snd-nxt) (elem-get tcb :snd-una)))

(defun tcp/recv-data (tcb)
	; Retrieve received data from buffer
	; Input: tcb - TCP control block
	; Output: byte array of received data
	(defq data (elem-get tcb :recv-buffer))
	(elem-set tcb :recv-buffer (list))
	(apply array data))
