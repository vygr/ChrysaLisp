# TCP/IP Stack Quick Start Guide

## Getting Started in 5 Minutes

### 1. Initialize the Stack

```lisp
; Load the network initialization module
(import "apps/netdemo/net_init.lisp")

; Run demo initialization
(net/demo-init)
```

Output:
```
Network Stack Demo Initialization

Initializing TCP/IP stack...
  - Ethernet layer
  - ARP
  - IP layer
  - ICMP
  - UDP
  - TCP
  - Registering protocol handlers
TCP/IP stack initialized successfully!
  MAC Address: 00:11:22:33:44:55
  IP Address:  192.168.1.100
  Netmask:     255.255.255.0
  Gateway:     192.168.1.1
```

### 2. Send a Ping

```lisp
; Load ping utility
(import "apps/netdemo/ping.lisp")

; Ping a host
(ping/main '("192.168.1.1"))
```

### 3. Start a UDP Echo Server

```lisp
; Load UDP echo server
(import "apps/netdemo/udp_echo_server.lisp")

; Start server on port 7777
(udp-echo-server/main '("7777"))
```

Output:
```
UDP echo server listening on port 7777
Press Ctrl+C to stop
```

### 4. Start a TCP Echo Server

```lisp
; Load TCP echo server
(import "apps/netdemo/tcp_echo_server.lisp")

; Start server on port 7777
(tcp-echo-server/main '("7777"))
```

Output:
```
TCP echo server listening on port 7777
Press Ctrl+C to stop
```

### 5. Connect with a TCP Client

```lisp
; Load TCP client
(import "apps/netdemo/tcp_client.lisp")

; Connect and send message
(tcp-client/main '("192.168.1.100" "7777" "Hello, World!"))
```

Output:
```
Connecting to 192.168.1.100:7777...
Connected!
Sending: Hello, World!
Received: Hello, World!
Connection closed
```

## Manual Stack Setup

For more control, initialize the stack manually:

```lisp
; Import required modules
(import "lib/net/ethernet.inc")
(import "lib/net/arp.inc")
(import "lib/net/ip.inc")
(import "lib/net/icmp.inc")
(import "lib/net/udp.inc")
(import "lib/net/tcp.inc")

; Define your network configuration
(defq my-mac (array 0x00 0x11 0x22 0x33 0x44 0x55)
      my-ip (array 192 168 1 100)
      netmask (array 255 255 255 0)
      gateway (array 192 168 1 1))

; Define send function (this would interface with your network driver)
(defq send-frame
    (lambda (frame)
        ; Your code to send raw Ethernet frame
        ; For example: (host-network-send frame)
        (prin "Sending " (length frame) " byte frame")
        (prinl)))

; Initialize layers
(eth/init my-mac send-frame)
(arp/init)
(ip/init my-ip netmask gateway)
(icmp/init)
(udp/init)
(tcp/init)
(eth/init-handlers)

(prin "Stack ready!")
(prinl)
```

## Simple UDP Application

```lisp
(import "lib/net/socket.inc")

; Create socket
(defq sock (socket/create sock_dgram))

; Bind to port
(socket/bind sock 5000)

; Set receive handler
(socket/set-handler sock
    (lambda (src-ip src-port data)
        (prin "Got " (length data) " bytes from "
              (net/ip-to-string src-ip) ":" src-port)
        (prinl)))

; Send data
(defq dest-ip (array 192 168 1 10))
(socket/sendto sock dest-ip 5000 (array 1 2 3 4 5))

; Close
(socket/close sock)
```

## Simple TCP Application

```lisp
(import "lib/net/socket.inc")

; Create socket
(defq sock (socket/create sock_stream))

; Connect
(socket/connect sock (array 192 168 1 10) 8080)

; Wait for connection (simplified - need event loop in practice)
(task-sleep 1000000)

; Send data
(if (socket/is-connected sock)
    (progn
        (socket/send sock (array 72 101 108 108 111))  ; "Hello"
        (task-sleep 1000000)
        (defq response (socket/recv sock))
        (prin "Received " (length response) " bytes")
        (prinl))
    (prin "Connection failed")
    (prinl))

; Close
(socket/close sock)
```

## Architecture Overview

```
Application Code
       ↓
Socket API (socket.lisp)
       ↓
┌──────────┬──────────┐
│   TCP    │   UDP    │  Transport Layer
└──────────┴──────────┘
       ↓
┌──────────┬──────────┐
│    IP    │   ICMP   │  Network Layer
└──────────┴──────────┘
       ↓
┌──────────┬──────────┐
│ Ethernet │   ARP    │  Link Layer
└──────────┴──────────┘
       ↓
Network Driver / Host OS
```

## Common Operations

### Check IP Address

```lisp
(import "lib/net/ip.inc")
(net/ip-to-string (ip/get-addr))  ; "192.168.1.100"
```

### Check MAC Address

```lisp
(import "lib/net/ethernet.inc")
(net/mac-to-string (eth/get-mac))  ; "00:11:22:33:44:55"
```

### Convert IP String to Bytes

```lisp
(import "lib/net/utils.inc")
(net/string-to-ip "192.168.1.1")  ; (array 192 168 1 1)
```

### Convert MAC String to Bytes

```lisp
(import "lib/net/utils.inc")
(net/string-to-mac "AA:BB:CC:DD:EE:FF")
; (array 0xAA 0xBB 0xCC 0xDD 0xEE 0xFF)
```

### Calculate Checksum

```lisp
(import "lib/net/utils.inc")
(defq data (array 1 2 3 4 5 6 7 8))
(net/checksum data 0 (length data))  ; Returns 16-bit checksum
```

## Troubleshooting

### Stack Not Receiving Packets

1. Check that `eth/process` is being called with incoming frames
2. Verify MAC address matches your interface
3. Ensure protocol handlers are registered with `eth/init-handlers`

### TCP Connection Not Establishing

1. Check IP routing - is destination on local network or via gateway?
2. Verify ARP cache has MAC for next hop
3. Check TCP state with `tcp/get-state-str`
4. Ensure both SYN and SYN-ACK are being exchanged

### UDP Packets Not Received

1. Verify port is bound correctly
2. Check that handler function is set
3. Ensure `udp/process` is being called by IP layer

## Next Steps

- Read the full [README.md](README.md) for detailed documentation
- Explore example applications in `apps/netdemo/`
- Implement your own network application
- Integrate with ChrysaLisp's task system for concurrent connections
- Add hardware driver for bare-metal networking

## Performance Tips

1. **Minimize copies**: Pass buffers by reference when possible
2. **Batch processing**: Process multiple packets in one iteration
3. **Event-driven**: Use ChrysaLisp's task system for I/O events
4. **Cache warming**: Pre-populate ARP cache for known hosts
5. **Connection pooling**: Reuse TCP connections when possible

## API Reference Quick Look

### Socket Functions
- `socket/create` - Create new socket
- `socket/bind` - Bind to local port
- `socket/listen` - Listen for connections (TCP)
- `socket/connect` - Connect to remote host (TCP)
- `socket/send` - Send data (TCP)
- `socket/sendto` - Send datagram (UDP)
- `socket/recv` - Receive data
- `socket/close` - Close socket

### Network Utilities
- `net/ip-to-string` - IP bytes to string
- `net/string-to-ip` - String to IP bytes
- `net/mac-to-string` - MAC bytes to string
- `net/string-to-mac` - String to MAC bytes
- `net/checksum` - Calculate Internet checksum

### Protocol Functions
- `ip/send-packet` - Send IP packet
- `icmp/send-ping` - Send ping
- `udp/send` - Send UDP datagram
- `tcp/connect` - Initiate TCP connection
- `tcp/send-data` - Send TCP data

Happy networking with ChrysaLisp!
