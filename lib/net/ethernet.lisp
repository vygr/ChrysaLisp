;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ethernet Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")
(import "lib/net/arp.lisp")
(import "lib/net/ip.lisp")

;;;;;;;;;;;;;;;;;;
; Ethernet State
;;;;;;;;;;;;;;;;;;

(defq *eth-our_mac* (array 0x00 0x00 0x00 0x00 0x00 0x00))  ; Our MAC address
(defq *eth-send_fn* nil)  ; Function to send raw frames

(defun eth/init (mac send_fn)
	; Initialize Ethernet layer
	; Inputs:
	;   mac - our MAC address (6-byte array)
	;   send-fn - function to send raw Ethernet frames
	(setq *eth-our-mac* (apply array mac))
	(setq *eth-send-fn* send-fn))

(defun eth/get-mac ()
	; Get our MAC address
	*eth-our-mac*)

;;;;;;;;;;;;;;;;;;
; Ethernet Frame Creation
;;;;;;;;;;;;;;;;;;

(defun eth/create-frame (dst_mac etype payload)
	; Create Ethernet frame
	; Inputs:
	;   dst-mac - destination MAC address (6-byte array)
	;   etype - EtherType (e.g., 0x0800 for IP)
	;   payload - frame payload (byte array)
	; Output: Ethernet frame (byte array)
	(defq frame (array))

	; Destination MAC
	(each (# (push frame %0)) dst-mac)

	; Source MAC
	(each (# (push frame %0)) *eth-our-mac*)

	; EtherType
	(net/write-u16 frame 12 etype)

	; Payload
	(each (# (push frame %0)) payload)

	; Padding if needed (minimum frame size is 64 bytes including CRC)
	(defq min_len (- eth_min_len eth_crc_len))
	(while (< (length frame) min-len)
		(push frame 0))

	frame)

;;;;;;;;;;;;;;;;;;
; Ethernet Frame Parsing
;;;;;;;;;;;;;;;;;;

(defun eth/parse (data)
	; Parse Ethernet frame
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) eth_hdr_len)
		nil
		(env
			:dst-mac (slice data 0 6)
			:src-mac (slice data 6 12)
			:etype (net/read-u16 data 12)
			:payload (slice data eth_hdr_len (length data)))))

;;;;;;;;;;;;;;;;;;
; Ethernet Frame Sending
;;;;;;;;;;;;;;;;;;

(defun eth/send-frame (dst_mac etype payload)
	; Send Ethernet frame
	; Inputs: dst-mac, etype, payload
	; Output: t if sent, nil if failed
	(if *eth-send-fn*
		(progn
			(defq frame (eth/create-frame dst_mac etype payload))
			(*eth-send-fn* frame)
			t)
		nil))

(defun eth/send-ip-packet (dst_ip packet)
	; Send IP packet via Ethernet
	; Inputs: dst-ip - destination IP, packet - IP packet
	; Output: t if sent, nil if failed

	; Resolve MAC address via ARP
	(defq dst-mac (arp/cache-lookup dst_ip))

	(if dst-mac
		; MAC address known - send frame
		(eth/send-frame dst-mac eth_type_ip packet)
		; MAC address unknown - need ARP resolution
		; (For now, return nil - full implementation would queue and send ARP request)
		nil))

(defun eth/send-arp-packet (dst_mac packet)
	; Send ARP packet via Ethernet
	; Inputs: dst-mac - destination MAC, packet - ARP packet
	; Output: t if sent, nil if failed
	(eth/send-frame dst-mac eth_type_arp packet))

;;;;;;;;;;;;;;;;;;
; Ethernet Frame Reception
;;;;;;;;;;;;;;;;;;

(defq *eth-protocol_handlers* (env))  ; EtherType handler registry

(defun eth/register-handler (etype handler_fn)
	; Register handler for EtherType
	; Inputs:
	;   etype - EtherType value
	;   handler-fn - function to call with (src-mac dst-mac payload)
	(elem-set *eth-protocol-handlers* etype handler-fn))

(defun eth/process (frame)
	; Process incoming Ethernet frame
	; Input: frame - raw Ethernet frame (byte array)
	; Output: t if processed, nil if error
	(defq eth_frame (eth/parse frame))

	(if eth-frame
		(progn
			; Check if frame is for us (or broadcast)
			(defq dst-mac (elem-get eth-frame :dst_mac)
			      broadcast (array 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF))

			(when (or (every eql *eth-our-mac* dst-mac)
			         (every eql broadcast dst-mac))

				; Find protocol handler
				(defq handler (elem-get *eth-protocol_handlers*
				                        (elem-get eth-frame :etype)))

				(if handler
					(progn
						; Call protocol handler
						(handler (elem-get eth-frame :src-mac)
						         (elem-get eth-frame :dst-mac)
						         (elem-get eth-frame :payload))
						t)
					; No handler for this EtherType
					nil)))
		; Invalid frame
		nil))

;;;;;;;;;;;;;;;;;;
; Initialize Ethernet Protocol Handlers
;;;;;;;;;;;;;;;;;;

(defun eth/init-handlers ()
	; Register default protocol handlers

	; ARP handler
	(eth/register-handler eth_type_arp
		(lambda (src-mac dst-mac payload)
			(defq reply (arp/process payload *eth-our-mac* (ip/get_addr)))
			(when reply
				(eth/send-arp-packet src-mac reply))))

	; IP handler
	(eth/register-handler eth_type_ip
		(lambda (src-mac dst-mac payload)
			(ip/process payload))))

;;;;;;;;;;;;;;;;;;
; Broadcast Addresses
;;;;;;;;;;;;;;;;;;

(defun eth/broadcast-mac ()
	; Get broadcast MAC address
	(array 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF))

(defun eth/is-broadcast (mac)
	; Check if MAC address is broadcast
	(every eql mac (eth/broadcast-mac)))

(defun eth/is-multicast (mac)
	; Check if MAC address is multicast
	(not (= 0 (logand (elem-get mac 0) 0x01))))
