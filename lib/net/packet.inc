(include "./consts.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Packet Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Ethernet Frame
;;;;;;;;;;;;;;;;;;

(def-struct eth_frame 0
	(array ubyte eth_addr_len dst_mac)   ; Destination MAC address
	(array ubyte eth_addr_len src_mac)   ; Source MAC address
	(ushort etype)                       ; EtherType
	(offset payload))                    ; Frame payload

;;;;;;;;;;;;;;;;;;
; ARP Packet
;;;;;;;;;;;;;;;;;;

(def-struct arp_packet 0
	(ushort htype)                       ; Hardware type
	(ushort ptype)                       ; Protocol type
	(ubyte hlen)                         ; Hardware address length
	(ubyte plen)                         ; Protocol address length
	(ushort oper)                        ; Operation (request/reply)
	(array ubyte eth_addr_len sha)       ; Sender hardware address
	(array ubyte ip_addr_len spa)        ; Sender protocol address
	(array ubyte eth_addr_len tha)       ; Target hardware address
	(array ubyte ip_addr_len tpa))       ; Target protocol address

;;;;;;;;;;;;;;;;;;
; IP Header
;;;;;;;;;;;;;;;;;;

(def-struct ip_header 0
	(ubyte ver_ihl)                      ; Version (4 bits) + IHL (4 bits)
	(ubyte tos)                          ; Type of Service
	(ushort total_len)                   ; Total length
	(ushort id)                          ; Identification
	(ushort flags_offset)                ; Flags (3 bits) + Fragment offset (13 bits)
	(ubyte ttl)                          ; Time To Live
	(ubyte protocol)                     ; Protocol (TCP=6, UDP=17, ICMP=1)
	(ushort checksum)                    ; Header checksum
	(array ubyte ip_addr_len src_ip)     ; Source IP address
	(array ubyte ip_addr_len dst_ip)     ; Destination IP address
	(offset options))                    ; IP options (if IHL > 5)

;;;;;;;;;;;;;;;;;;
; ICMP Header
;;;;;;;;;;;;;;;;;;

(def-struct icmp_header 0
	(ubyte type)                         ; Message type
	(ubyte code)                         ; Message code
	(ushort checksum)                    ; Checksum
	(uint rest)                          ; Rest of header (type-specific)
	(offset data))                       ; ICMP data

;;;;;;;;;;;;;;;;;;
; UDP Header
;;;;;;;;;;;;;;;;;;

(def-struct udp_header 0
	(ushort src_port)                    ; Source port
	(ushort dst_port)                    ; Destination port
	(ushort length)                      ; Length (header + data)
	(ushort checksum)                    ; Checksum
	(offset data))                       ; UDP data

;;;;;;;;;;;;;;;;;;
; TCP Header
;;;;;;;;;;;;;;;;;;

(def-struct tcp_header 0
	(ushort src_port)                    ; Source port
	(ushort dst_port)                    ; Destination port
	(uint seq_num)                       ; Sequence number
	(uint ack_num)                       ; Acknowledgment number
	(ubyte data_offset)                  ; Data offset (4 bits) + Reserved (4 bits)
	(ubyte flags)                        ; Control flags
	(ushort window)                      ; Window size
	(ushort checksum)                    ; Checksum
	(ushort urgent_ptr)                  ; Urgent pointer
	(offset options))                    ; TCP options

;;;;;;;;;;;;;;;;;;
; Packet Buffer
;;;;;;;;;;;;;;;;;;

; Packet buffer for managing network packets
(def-struct pkt_buffer 0
	(ptr next)                           ; Next buffer in pool/chain
	(ptr data)                           ; Pointer to actual data
	(uint size)                          ; Total buffer size
	(uint len)                           ; Current data length
	(uint head)                          ; Header offset
	(uint tail)                          ; Tail offset
	(uint refcount)                      ; Reference count
	(struct eth eth_frame_size)          ; Parsed Ethernet header
	(struct ip ip_header_size)           ; Parsed IP header
	(struct transport 20)                ; Parsed transport header (TCP/UDP/ICMP)
	(offset buffer))                     ; Actual buffer data starts here

;;;;;;;;;;;;;;;;;;
; Network Interface
;;;;;;;;;;;;;;;;;;

(def-struct net_if 0
	(array ubyte eth_addr_len mac)       ; MAC address
	(array ubyte ip_addr_len ip)         ; IP address
	(array ubyte ip_addr_len netmask)    ; Netmask
	(array ubyte ip_addr_len gateway)    ; Gateway
	(uint mtu)                           ; Maximum Transmission Unit
	(ptr send_fn)                        ; Function to send packets
	(ptr recv_fn)                        ; Function to receive packets
	(ulong tx_packets)                   ; Transmitted packets counter
	(ulong rx_packets)                   ; Received packets counter
	(ulong tx_bytes)                     ; Transmitted bytes counter
	(ulong rx_bytes))                    ; Received bytes counter

;;;;;;;;;;;;;;;;;;
; ARP Cache Entry
;;;;;;;;;;;;;;;;;;

(def-struct arp_entry 0
	(array ubyte ip_addr_len ip)         ; IP address
	(array ubyte eth_addr_len mac)       ; MAC address
	(ulong timestamp)                    ; Last update time
	(ubyte state)                        ; Entry state (0=free, 1=valid, 2=pending)
	(align))

;;;;;;;;;;;;;;;;;;
; Socket Structure
;;;;;;;;;;;;;;;;;;

(def-struct net_socket 0
	(uint type)                          ; Socket type (stream/dgram/raw)
	(uint state)                         ; Socket state
	(uint protocol)                      ; Protocol number
	(ushort local_port)                  ; Local port
	(ushort remote_port)                 ; Remote port
	(array ubyte ip_addr_len local_ip)   ; Local IP address
	(array ubyte ip_addr_len remote_ip)  ; Remote IP address
	(ptr recv_queue)                     ; Receive queue (list of packets)
	(ptr send_queue)                     ; Send queue (list of packets)
	(ptr tcp_state)                      ; TCP state (if TCP socket)
	(ulong mbox_id)                      ; Mailbox ID for notifications
	(uint flags))                        ; Socket flags

;;;;;;;;;;;;;;;;;;
; TCP Control Block
;;;;;;;;;;;;;;;;;;

(def-struct tcp_cb 0
	(uint state)                         ; TCP state
	(uint snd_una)                       ; Send unacknowledged
	(uint snd_nxt)                       ; Send next
	(uint snd_wnd)                       ; Send window
	(uint snd_wl1)                       ; Segment sequence number for last window update
	(uint snd_wl2)                       ; Segment acknowledgment number for last window update
	(uint iss)                           ; Initial send sequence number
	(uint rcv_nxt)                       ; Receive next
	(uint rcv_wnd)                       ; Receive window
	(uint irs)                           ; Initial receive sequence number
	(uint mss)                           ; Maximum segment size
	(uint rto)                           ; Retransmission timeout
	(uint srtt)                          ; Smoothed round trip time
	(uint rttvar)                        ; Round trip time variance
	(ulong last_ack_time)                ; Time of last ACK
	(uint retrans_count)                 ; Retransmission count
	(ptr retrans_queue)                  ; Retransmission queue
	(ptr recv_buffer)                    ; Receive buffer
	(ptr send_buffer))                   ; Send buffer

;;;;;;;;;;;;;;;;;;
; Utility Macros
;;;;;;;;;;;;;;;;;;

; Extract IP header fields
(defun ip-version (ver_ihl)
	(ash ver_ihl -4))

(defun ip-ihl (ver_ihl)
	(logand ver_ihl 0x0F))

(defun ip-header-len (ver_ihl)
	(* (ip-ihl ver_ihl) 4))

; Extract TCP header fields
(defun tcp-data-offset (data_offset)
	(ash data_offset -4))

(defun tcp-header-len (data_offset)
	(* (tcp-data-offset data_offset) 4))

; IP address manipulation
(defun ip-addr-to-u32 (addr-ptr)
	; Convert 4-byte IP address to 32-bit unsigned integer
	(logior
		(ash (mem-read-byte addr-ptr 0) 24)
		(ash (mem-read-byte addr-ptr 1) 16)
		(ash (mem-read-byte addr-ptr 2) 8)
		(mem-read-byte addr-ptr 3)))

(defun u32-to-ip-addr (val addr-ptr)
	; Convert 32-bit unsigned integer to 4-byte IP address
	(mem-write-byte addr-ptr 0 (logand (ash val -24) 0xFF))
	(mem-write-byte addr-ptr 1 (logand (ash val -16) 0xFF))
	(mem-write-byte addr-ptr 2 (logand (ash val -8) 0xFF))
	(mem-write-byte addr-ptr 3 (logand val 0xFF)))

; MAC address comparison
(defun mac-equal (mac1 mac2)
	; Compare two MAC addresses
	(and
		(= (mem-read-byte mac1 0) (mem-read-byte mac2 0))
		(= (mem-read-byte mac1 1) (mem-read-byte mac2 1))
		(= (mem-read-byte mac1 2) (mem-read-byte mac2 2))
		(= (mem-read-byte mac1 3) (mem-read-byte mac2 3))
		(= (mem-read-byte mac1 4) (mem-read-byte mac2 4))
		(= (mem-read-byte mac1 5) (mem-read-byte mac2 5))))

; IP address comparison
(defun ip-equal (ip1 ip2)
	; Compare two IP addresses
	(and
		(= (mem-read-byte ip1 0) (mem-read-byte ip2 0))
		(= (mem-read-byte ip1 1) (mem-read-byte ip2 1))
		(= (mem-read-byte ip1 2) (mem-read-byte ip2 2))
		(= (mem-read-byte ip1 3) (mem-read-byte ip2 3))))
