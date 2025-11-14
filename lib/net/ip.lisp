;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IP (Internet Protocol) Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")

;;;;;;;;;;;;;;;;;;
; IP State
;;;;;;;;;;;;;;;;;;

(defq *ip-id-counter* 0)  ; Packet ID counter
(defq *ip-our-addr* (array 0 0 0 0))  ; Our IP address
(defq *ip-netmask* (array 255 255 255 0))  ; Our netmask
(defq *ip-gateway* (array 0 0 0 0))  ; Default gateway

(defun ip/init (ip netmask gateway)
	; Initialize IP layer
	; Inputs: ip, netmask, gateway - 4-byte arrays
	(setq *ip-our-addr* (apply array ip))
	(setq *ip-netmask* (apply array netmask))
	(setq *ip-gateway* (apply array gateway))
	(setq *ip-id-counter* (net/random-range 1 65536)))

(defun ip/get-addr ()
	; Get our IP address
	*ip-our-addr*)

;;;;;;;;;;;;;;;;;;
; IP Header Creation
;;;;;;;;;;;;;;;;;;

(defun ip/create-header (src-ip dst-ip protocol data-len &optional ttl df-flag)
	; Create IP header
	; Inputs:
	;   src-ip, dst-ip - 4-byte arrays
	;   protocol - protocol number (TCP=6, UDP=17, ICMP=1)
	;   data-len - length of data following header
	;   ttl - time to live (optional, default 64)
	;   df-flag - don't fragment flag (optional, default nil)
	; Output: byte array of IP header
	(defq hdr (array)
	      total-len (+ ip_hdr_min_len data-len)
	      id *ip-id-counter*
	      flags-offset (if df-flag ip_flag_df 0))

	; Increment ID for next packet
	(setq *ip-id-counter* (logand (+ *ip-id-counter* 1) 0xFFFF))

	; Version (4) and IHL (5 = 20 bytes)
	(elem-set hdr 0 0x45)

	; Type of Service
	(elem-set hdr 1 0)

	; Total length
	(net/write-u16 hdr 2 total-len)

	; Identification
	(net/write-u16 hdr 4 id)

	; Flags and fragment offset
	(net/write-u16 hdr 6 flags-offset)

	; Time to Live
	(elem-set hdr 8 (or ttl ip_default_ttl))

	; Protocol
	(elem-set hdr 9 protocol)

	; Checksum (will be calculated)
	(net/write-u16 hdr 10 0)

	; Source IP
	(each (# (elem-set hdr (+ 12 %1) %0)) src-ip)

	; Destination IP
	(each (# (elem-set hdr (+ 16 %1) %0)) dst-ip)

	; Calculate and set checksum
	(defq cksum (net/checksum hdr 0 ip_hdr_min_len))
	(net/write-u16 hdr 10 cksum)

	hdr)

;;;;;;;;;;;;;;;;;;
; IP Packet Parsing
;;;;;;;;;;;;;;;;;;

(defun ip/parse (data)
	; Parse IP packet
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) ip_hdr_min_len)
		nil
		(progn
			(defq ver-ihl (elem-get data 0)
			      version (ash ver-ihl -4)
			      ihl (logand ver-ihl 0x0F)
			      hdr-len (* ihl 4)
			      total-len (net/read-u16 data 2))

			; Validate
			(if (and (= version 4)
			         (>= ihl 5)
			         (<= total-len (length data)))
				(env
					:version version
					:ihl ihl
					:header-len hdr-len
					:tos (elem-get data 1)
					:total-len total-len
					:id (net/read-u16 data 4)
					:flags (ash (net/read-u16 data 6) -13)
					:frag-offset (logand (net/read-u16 data 6) 0x1FFF)
					:ttl (elem-get data 8)
					:protocol (elem-get data 9)
					:checksum (net/read-u16 data 10)
					:src-ip (slice data 12 16)
					:dst-ip (slice data 16 20)
					:options (if (> hdr-len ip_hdr_min_len)
					            (slice data ip_hdr_min_len hdr-len)
					            (array))
					:data (slice data hdr-len total-len))
				nil))))

(defun ip/verify-checksum (data)
	; Verify IP header checksum
	; Input: data - IP packet byte array
	; Output: t if valid, nil if invalid
	(if (< (length data) ip_hdr_min_len)
		nil
		(progn
			(defq ver-ihl (elem-get data 0)
			      ihl (logand ver-ihl 0x0F)
			      hdr-len (* ihl 4))
			(= (net/checksum data 0 hdr-len) 0xFFFF))))

;;;;;;;;;;;;;;;;;;
; IP Routing
;;;;;;;;;;;;;;;;;;

(defun ip/is-local (dst-ip)
	; Check if destination IP is on local network
	; Input: dst-ip - 4-byte array
	; Output: t if local, nil if needs gateway
	(net/ip-in-subnet dst-ip *ip-netmask* *ip-our-addr*))

(defun ip/next-hop (dst-ip)
	; Determine next hop for destination
	; Input: dst-ip - 4-byte array
	; Output: next hop IP address (4-byte array)
	(if (ip/is-local dst-ip)
		dst-ip
		*ip-gateway*))

;;;;;;;;;;;;;;;;;;
; IP Packet Sending
;;;;;;;;;;;;;;;;;;

(defun ip/send-packet (dst-ip protocol data &optional ttl)
	; Send IP packet
	; Inputs:
	;   dst-ip - 4-byte array
	;   protocol - protocol number
	;   data - payload data (byte array)
	;   ttl - optional time to live
	; Output: complete packet (IP header + data) or nil
	(defq hdr (ip/create-header *ip-our-addr* dst-ip
	                            protocol (length data) ttl)
	      packet (array))

	; Combine header and data
	(each (# (push packet %0)) hdr)
	(each (# (push packet %0)) data)

	packet)

;;;;;;;;;;;;;;;;;;
; IP Packet Reception
;;;;;;;;;;;;;;;;;;

(defq *ip-protocol-handlers* (env))  ; Protocol handler registry

(defun ip/register-handler (protocol handler-fn)
	; Register handler for IP protocol
	; Inputs:
	;   protocol - protocol number
	;   handler-fn - function to call with (src-ip dst-ip data)
	(elem-set *ip-protocol-handlers* protocol handler-fn))

(defun ip/process (packet)
	; Process incoming IP packet
	; Input: packet - raw IP packet (byte array)
	; Output: t if processed, nil if error
	(defq ip-pkt (ip/parse packet))

	(if (and ip-pkt (ip/verify-checksum packet))
		(progn
			; Check if packet is for us
			(when (or (every eql *ip-our-addr* (elem-get ip-pkt :dst-ip))
			          ; Broadcast check could go here
			          (every eql (array 255 255 255 255) (elem-get ip-pkt :dst-ip)))

				; Find protocol handler
				(defq handler (elem-get *ip-protocol-handlers*
				                        (elem-get ip-pkt :protocol)))

				(if handler
					(progn
						; Call protocol handler
						(handler (elem-get ip-pkt :src-ip)
						         (elem-get ip-pkt :dst-ip)
						         (elem-get ip-pkt :data))
						t)
					; No handler for this protocol
					nil)))
		; Invalid packet
		nil))

;;;;;;;;;;;;;;;;;;
; IP Fragmentation (Simplified)
;;;;;;;;;;;;;;;;;;

(defun ip/fragment (data mtu)
	; Fragment IP data into MTU-sized chunks
	; Inputs:
	;   data - byte array to fragment
	;   mtu - maximum transmission unit
	; Output: list of fragments
	(defq fragments (list)
	      offset 0
	      data-len (length data)
	      max-frag-size (- mtu ip_hdr_min_len))

	(while (< offset data-len)
		(defq frag-len (min max-frag-size (- data-len offset))
		      frag (slice data offset (+ offset frag-len)))
		(push fragments frag)
		(setq offset (+ offset frag-len)))

	fragments)
