# ChrysaLisp TCP/IP Stack

A complete, from-scratch implementation of a TCP/IP network stack for ChrysaLisp OS.

## Overview

This is a full-featured network stack implementing the standard Internet Protocol suite, designed specifically for ChrysaLisp's distributed, message-passing architecture. The implementation follows RFC specifications and provides a clean, layered architecture.

## Architecture

The stack is organized in layers, from lowest to highest:

```
┌─────────────────────────────────┐
│   Application Layer             │
│   (Socket API)                  │
├─────────────────────────────────┤
│   Transport Layer               │
│   TCP          UDP              │
├─────────────────────────────────┤
│   Network Layer                 │
│   IP           ICMP             │
├─────────────────────────────────┤
│   Link Layer                    │
│   Ethernet     ARP              │
└─────────────────────────────────┘
```

## Components

### Core Libraries

- **`consts.inc`** - Network protocol constants and definitions
- **`packet.inc`** - Packet structure definitions
- **`utils.lisp`** - Utility functions (checksums, byte order conversion, etc.)

### Link Layer

- **`ethernet.lisp`** - Ethernet frame handling
  - Frame creation and parsing
  - MAC address management
  - Protocol multiplexing (ARP, IP)
  - Broadcast and multicast support

### Network Layer

- **`arp.lisp`** - Address Resolution Protocol
  - ARP cache management
  - ARP request/reply handling
  - MAC address resolution

- **`ip.lisp`** - Internet Protocol (IPv4)
  - IP packet creation and parsing
  - Header checksum calculation
  - Fragmentation support
  - Routing (local network vs gateway)
  - Protocol multiplexing (TCP, UDP, ICMP)

- **`icmp.lisp`** - Internet Control Message Protocol
  - Echo request/reply (ping)
  - Destination unreachable
  - Time exceeded
  - Parameter problem

### Transport Layer

- **`udp.lisp`** - User Datagram Protocol
  - Connectionless datagram service
  - Port binding and management
  - Pseudo-header checksum
  - Socket-like API

- **`tcp.lisp`** - Transmission Control Protocol
  - Full TCP state machine
  - 3-way handshake (connection establishment)
  - 4-way handshake (connection termination)
  - Sequence number management
  - Sliding window flow control
  - Retransmission queue
  - RTT estimation
  - Congestion avoidance (basic)

- **`tcp_state.lisp`** - TCP state machine implementation
  - State transition logic
  - Packet processing by state
  - SYN, ACK, FIN, RST handling

### Application Layer

- **`socket.lisp`** - Unified socket API
  - BSD-style socket interface
  - Stream sockets (TCP)
  - Datagram sockets (UDP)
  - Bind, connect, listen, accept
  - Send, receive operations

## Usage

### Initialization

```lisp
(import "lib/net/ethernet.lisp")
(import "lib/net/arp.lisp")
(import "lib/net/ip.lisp")
(import "lib/net/icmp.lisp")
(import "lib/net/udp.lisp")
(import "lib/net/tcp.lisp")

; Initialize the stack
(defq my-mac (array 0x00 0x11 0x22 0x33 0x44 0x55)
      my-ip (array 192 168 1 100)
      netmask (array 255 255 255 0)
      gateway (array 192 168 1 1))

; Ethernet layer (provide send function)
(eth/init my-mac send-raw-frame-fn)

; Network layers
(arp/init)
(ip/init my-ip netmask gateway)
(icmp/init)

; Transport layers
(udp/init)
(tcp/init)

; Register protocol handlers
(eth/init-handlers)
```

### UDP Echo Server Example

```lisp
(import "lib/net/socket.lisp")

; Create UDP socket
(defq sock (socket/create sock_dgram))

; Set up handler for incoming data
(socket/set-handler sock
    (lambda (src-ip src-port data)
        ; Echo data back
        (socket/sendto sock src-ip src-port data)))

; Bind to port
(socket/bind sock 7777)

; Socket is now ready to receive/send
```

### TCP Echo Server Example

```lisp
(import "lib/net/socket.lisp")

; Create TCP listening socket
(defq server-sock (socket/create sock_stream))
(socket/bind server-sock 7777)

; Listen for connections
(socket/listen server-sock
    (lambda (tcb)
        ; New connection - tcb is the TCP control block
        ; Set up data handler or process in main loop
        ))

; Main loop to handle connections
; (Process TCP state machine, send/receive data)
```

### TCP Client Example

```lisp
(import "lib/net/socket.lisp")

; Create TCP socket
(defq sock (socket/create sock_stream))

; Connect to server
(defq server-ip (array 192 168 1 10))
(socket/connect sock server-ip 7777)

; Wait for connection to establish
; (Check socket/is-connected)

; Send data
(defq message (array 72 101 108 108 111))  ; "Hello"
(socket/send sock message)

; Receive response
(defq response (socket/recv sock))

; Close connection
(socket/close sock)
```

### Ping Example

```lisp
(import "lib/net/icmp.lisp")

; Register handler for ping replies
(icmp/register-echo-handler
    (lambda (src-ip id seq data)
        (prin "Pong from " (net/ip-to-string src-ip))))

; Send ping
(defq target-ip (array 192 168 1 1)
      ping-id 12345
      ping-seq 1
      ping-data (array 0x41 0x42 0x43))  ; "ABC"

(icmp/send-ping target-ip ping-id ping-seq ping-data)
```

## Protocol Features

### TCP Features Implemented

- **Connection Management**
  - Active open (client connect)
  - Passive open (server listen/accept)
  - Simultaneous open support
  - Graceful close (FIN handshake)
  - Connection reset (RST)

- **Reliability**
  - Sequence number tracking
  - Acknowledgment handling
  - Retransmission queue
  - Duplicate detection

- **Flow Control**
  - Sliding window protocol
  - Window size management
  - Sender/receiver window tracking

- **State Machine**
  - All 11 TCP states implemented
  - CLOSED, LISTEN, SYN_SENT, SYN_RECEIVED
  - ESTABLISHED, FIN_WAIT_1, FIN_WAIT_2
  - CLOSE_WAIT, CLOSING, LAST_ACK, TIME_WAIT

- **Timers**
  - Retransmission timeout (RTO)
  - Smoothed RTT calculation
  - TIME_WAIT timeout

### UDP Features

- **Connectionless Service**
  - Fire-and-forget datagram delivery
  - No connection setup required

- **Port Management**
  - Bind to specific ports
  - Ephemeral port allocation
  - Multiple sockets per host

- **Checksum**
  - Pseudo-header checksum calculation
  - Optional checksum verification

### ICMP Features

- **Echo Request/Reply** (Ping)
- **Destination Unreachable**
- **Time Exceeded**
- **Error Reporting**

### IP Features

- **IPv4 Protocol**
  - 20-byte minimum header
  - TTL management
  - Protocol multiplexing
  - Header checksum

- **Fragmentation**
  - Fragment creation
  - Don't Fragment flag support
  - MTU-based fragmentation

- **Routing**
  - Local network detection
  - Default gateway routing
  - Next-hop determination

### ARP Features

- **Address Resolution**
  - IP to MAC address mapping
  - ARP cache with timeout
  - Request/reply handling

- **Cache Management**
  - Automatic cache updates
  - Timeout-based expiration
  - Cache lookup optimization

## File Structure

```
lib/net/
├── README.md              # This file
├── consts.inc             # Protocol constants
├── packet.inc             # Packet structures
├── utils.lisp             # Utility functions
├── ethernet.lisp          # Ethernet layer
├── arp.lisp               # ARP protocol
├── ip.lisp                # IP layer
├── icmp.lisp              # ICMP protocol
├── udp.lisp               # UDP protocol
├── tcp.lisp               # TCP protocol
├── tcp_state.lisp         # TCP state machine
└── socket.lisp            # Socket API

apps/netdemo/
├── net_init.lisp          # Stack initialization
├── ping.lisp              # Ping utility
├── udp_echo_server.lisp   # UDP echo server
├── tcp_echo_server.lisp   # TCP echo server
└── tcp_client.lisp        # TCP client
```

## Design Principles

### 1. Layered Architecture
Each protocol layer is independent and communicates through well-defined interfaces. This allows for easy testing, debugging, and modification.

### 2. Pure Lisp Implementation
The entire stack is implemented in ChrysaLisp Lisp, making it highly portable and maintainable. No foreign function calls or C dependencies.

### 3. Integration with ChrysaLisp
The stack leverages ChrysaLisp's existing features:
- Message-passing for asynchronous I/O
- Task system for concurrent connections
- Memory management for packet buffers
- Time functions for timeouts

### 4. RFC Compliance
The implementation follows standard RFCs:
- RFC 791 (Internet Protocol)
- RFC 792 (Internet Control Message Protocol)
- RFC 768 (User Datagram Protocol)
- RFC 793 (Transmission Control Protocol)
- RFC 826 (Address Resolution Protocol)
- RFC 1071 (Computing the Internet Checksum)

### 5. Minimal Dependencies
The stack has minimal external dependencies and can operate in a bare-metal environment with just a network device driver.

## Integration Points

### Host OS Integration

To integrate with a host operating system's network stack:

1. **Implement send function**: Provide a function to send raw Ethernet frames
2. **Implement receive polling**: Poll for incoming frames and pass to `eth/process`
3. **Optional**: Use host OS for actual hardware access

```lisp
; Example host integration
(defun host-send-frame (frame)
    ; Call host OS network API
    (host-network-send frame))

(defun host-poll-frames ()
    ; Poll for incoming frames
    (defq frame (host-network-recv))
    (when frame
        (eth/process frame)))

; Initialize with host functions
(eth/init my-mac host-send-frame)

; Main loop
(while t
    (host-poll-frames)
    (task-sleep 1000))  ; 1ms
```

### Hardware Driver Integration

For bare-metal operation:

1. **Network Interface Driver**: Implement device driver for your NIC
2. **DMA Setup**: Configure DMA for efficient packet transfer
3. **Interrupt Handling**: Set up interrupts for packet arrival
4. **TX/RX Queues**: Manage transmit and receive ring buffers

## Performance Considerations

### Optimizations

- **O(1) Lookups**: Use hash maps for connection lookup
- **Zero-Copy**: Minimize data copying between layers
- **Lazy Evaluation**: Defer computation until needed
- **Cache Locality**: Keep related data structures together

### Limitations

- **Single-threaded**: Current implementation is single-threaded
- **No Scatter-Gather**: Simple buffer management
- **Basic Congestion Control**: Simplified TCP congestion avoidance
- **No IPv6**: IPv4 only

## Testing

Example test scenarios:

1. **Loopback Test**: Send packets to 127.0.0.1
2. **Ping Test**: ICMP echo request/reply
3. **UDP Echo**: Simple UDP echo server/client
4. **TCP Echo**: TCP echo server/client with multiple connections
5. **Stress Test**: Many concurrent connections

## Future Enhancements

Possible improvements:

- **IPv6 Support**: Add IPv6 protocol stack
- **Advanced TCP**: SACK, window scaling, timestamps
- **Congestion Control**: Implement Reno, NewReno, or CUBIC
- **Packet Reassembly**: IP fragment reassembly
- **Raw Sockets**: Direct IP access
- **Multicast**: IGMP and multicast support
- **DNS**: Domain Name System resolver
- **DHCP**: Dynamic Host Configuration Protocol client
- **TLS/SSL**: Secure socket layer

## References

- RFC 791 - Internet Protocol
- RFC 792 - Internet Control Message Protocol
- RFC 793 - Transmission Control Protocol
- RFC 768 - User Datagram Protocol
- RFC 826 - Ethernet Address Resolution Protocol
- RFC 1071 - Computing the Internet Checksum
- TCP/IP Illustrated, Volume 1 by W. Richard Stevens

## License

This TCP/IP stack implementation is part of the ChrysaLisp project.

## Credits

Implemented for ChrysaLisp, a unique Lisp-based operating system created by Chris Hinsley, author of TaOS and Elate from the early 1990s.
