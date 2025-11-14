;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ARP (Address Resolution Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")

;;;;;;;;;;;;;;;;;;
; ARP Cache
;;;;;;;;;;;;;;;;;;

(defq *arp-cache* (list))  ; List of ARP cache entries

(defun arp/init ()
	; Initialize ARP cache
	(setq *arp-cache* (list)))

(defun arp/cache-lookup (ip)
	; Look up IP address in ARP cache
	; Input: ip - 4-byte array
	; Output: MAC address (6-byte array) or nil
	(defq entry (find (# (and (elem-get %0 :valid)
	                          (every eql ip (elem-get %0 :ip))))
	                  *arp-cache*))
	(if entry (elem-get entry :mac) nil))

(defun arp/cache-add (ip mac)
	; Add or update entry in ARP cache
	; Inputs: ip - 4-byte array, mac - 6-byte array
	(defq now (time)
	      entry (find (# (every eql ip (elem-get %0 :ip))) *arp-cache*))
	(if entry
		; Update existing entry
		(progn
			(elem-set entry :mac mac)
			(elem-set entry :timestamp now)
			(elem-set entry :valid t))
		; Add new entry
		(push *arp-cache*
			(env
				:ip (apply array ip)
				:mac (apply array mac)
				:timestamp now
				:valid t))))

(defun arp/cache-timeout ()
	; Remove expired entries from ARP cache
	(defq now (time))
	(setq *arp-cache*
		(filter (# (< (- now (elem-get %0 :timestamp)) arp_cache_timeout))
		        *arp-cache*)))

;;;;;;;;;;;;;;;;;;
; ARP Packet Handling
;;;;;;;;;;;;;;;;;;

(defun arp/create-request (src_mac src_ip dst_ip)
	; Create ARP request packet
	; Inputs: src_mac (6 bytes), src_ip (4 bytes), dst_ip (4 bytes)
	; Output: byte array of ARP packet
	(defq pkt (array))

	; ARP header
	(net/write-u16 pkt 0 arp_hrd_ethernet)      ; Hardware type
	(net/write-u16 pkt 2 eth_type_ip)           ; Protocol type
	(elem-set pkt 4 eth_addr_len)               ; Hardware size
	(elem-set pkt 5 ip_addr_len)                ; Protocol size
	(net/write-u16 pkt 6 arp_op_request)        ; Operation

	; Sender hardware address (MAC)
	(each (# (elem-set pkt (+ 8 %1) %0)) src_mac)

	; Sender protocol address (IP)
	(each (# (elem-set pkt (+ 14 %1) %0)) src_ip)

	; Target hardware address (zeros for request)
	(defq i 0)
	(while (< i eth_addr_len)
		(elem-set pkt (+ 18 i) 0)
		(setq i (+ i 1)))

	; Target protocol address (IP)
	(each (# (elem-set pkt (+ 24 %1) %0)) dst_ip)

	pkt)

(defun arp/create-reply (src_mac src_ip dst_mac dst_ip)
	; Create ARP reply packet
	; Inputs: src_mac, src_ip, dst_mac, dst_ip (all byte arrays)
	; Output: byte array of ARP packet
	(defq pkt (array))

	; ARP header
	(net/write-u16 pkt 0 arp_hrd_ethernet)      ; Hardware type
	(net/write-u16 pkt 2 eth_type_ip)           ; Protocol type
	(elem-set pkt 4 eth_addr_len)               ; Hardware size
	(elem-set pkt 5 ip_addr_len)                ; Protocol size
	(net/write-u16 pkt 6 arp_op_reply)          ; Operation

	; Sender hardware address (MAC)
	(each (# (elem-set pkt (+ 8 %1) %0)) src_mac)

	; Sender protocol address (IP)
	(each (# (elem-set pkt (+ 14 %1) %0)) src_ip)

	; Target hardware address (MAC)
	(each (# (elem-set pkt (+ 18 %1) %0)) dst_mac)

	; Target protocol address (IP)
	(each (# (elem-set pkt (+ 24 %1) %0)) dst_ip)

	pkt)

(defun arp/parse (data)
	; Parse ARP packet
	; Input: data - byte array of ARP packet
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) 28)
		nil
		(env
			:htype (net/read-u16 data 0)
			:ptype (net/read-u16 data 2)
			:hlen (elem-get data 4)
			:plen (elem-get data 5)
			:oper (net/read-u16 data 6)
			:sha (slice data 8 14)
			:spa (slice data 14 18)
			:tha (slice data 18 24)
			:tpa (slice data 24 28))))

(defun arp/handle-request (arp_pkt our_mac our_ip)
	; Handle incoming ARP request
	; Inputs: arp_pkt - parsed ARP packet, our_mac, our_ip
	; Output: ARP reply packet or nil
	(when (and (= (elem-get arp_pkt :htype) arp_hrd_ethernet)
	           (= (elem-get arp_pkt :ptype) eth_type_ip)
	           (every eql our_ip (elem-get arp_pkt :tpa)))
		; This request is for us - send reply
		(arp/create-reply our_mac our_ip
		                  (elem-get arp_pkt :sha)
		                  (elem-get arp_pkt :spa))))

(defun arp/handle-reply (arp_pkt)
	; Handle incoming ARP reply
	; Input: arp_pkt - parsed ARP packet
	(when (and (= (elem-get arp_pkt :htype) arp_hrd_ethernet)
	           (= (elem-get arp_pkt :ptype) eth_type_ip))
		; Add to cache
		(arp/cache-add (elem-get arp_pkt :spa)
		               (elem-get arp_pkt :sha))))

(defun arp/process (data our_mac our_ip)
	; Process incoming ARP packet
	; Inputs: data - raw ARP packet, our_mac, our_ip
	; Output: ARP reply packet or nil
	(defq arp_pkt (arp/parse data))
	(if arp_pkt
		(cond
			((= (elem-get arp_pkt :oper) arp_op_request)
				; Update cache from request and possibly reply
				(arp/cache-add (elem-get arp_pkt :spa)
				               (elem-get arp_pkt :sha))
				(arp/handle-request arp_pkt our_mac our_ip))
			((= (elem-get arp_pkt :oper) arp_op_reply)
				; Update cache from reply
				(arp/handle-reply arp_pkt)
				nil)
			(t nil))
		nil))

;;;;;;;;;;;;;;;;;;
; ARP Resolution
;;;;;;;;;;;;;;;;;;

(defq *arp-pending* (env))  ; Pending ARP requests

(defun arp/resolve (ip timeout)
	; Resolve IP address to MAC address
	; Inputs: ip - 4-byte array, timeout - microseconds to wait
	; Output: MAC address (6-byte array) or nil if timeout

	; First check cache
	(defq mac (arp/cache-lookup ip))
	(if mac
		mac
		; Not in cache - need to send request
		; (This would integrate with the network stack's send mechanism)
		; For now, return nil
		nil))
