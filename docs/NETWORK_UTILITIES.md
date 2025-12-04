# ChrysaLisp Network Utilities

A comprehensive suite of network diagnostic and configuration utilities for ChrysaLisp, implementing standard Unix/Linux networking tools.

## Overview

These utilities provide command-line tools for network configuration, diagnostics, and troubleshooting on ChrysaLisp. They integrate with the TCP/IP stack implementation (`lib/net/`) and follow ChrysaLisp coding conventions.

## Utilities

### ping - ICMP Echo Request/Reply

Send ICMP echo requests to network hosts to test connectivity and measure round-trip time.

**Usage:**
```
ping [options] <target>
```

**Options:**
- `-h, --help` - Show help message
- `-c, --count <n>` - Number of pings to send (default: 4)
- `-i, --interval <n>` - Interval between pings in seconds (default: 1)
- `-w, --timeout <n>` - Timeout for each ping in seconds (default: 5)
- `-s, --size <n>` - Size of ping payload in bytes (default: 56)

**Examples:**
```
ping 192.168.1.1
ping -c 10 -i 0.5 192.168.1.1
ping -s 1024 192.168.1.1
```

**Output:**
```
PING 192.168.1.1 (192.168.1.1) 56 bytes of data
64 bytes from 192.168.1.1: icmp_seq=1 ttl=64 time=1.234 ms
64 bytes from 192.168.1.1: icmp_seq=2 ttl=64 time=1.156 ms
64 bytes from 192.168.1.1: icmp_seq=3 ttl=64 time=1.289 ms
64 bytes from 192.168.1.1: icmp_seq=4 ttl=64 time=1.201 ms

--- 192.168.1.1 ping statistics ---
4 packets transmitted, 4 received, 0% packet loss, time 3012ms
rtt min/avg/max = 1.156/1.220/1.289 ms
```

**Features:**
- ICMP echo request/reply validation
- Round-trip time measurement
- Packet loss statistics
- Configurable packet size and interval
- Timeout handling

---

### ifconfig - Network Interface Configuration

Display or configure network interfaces.

**Usage:**
```
ifconfig [options] [interface] [command]
```

**Options:**
- `-h, --help` - Show help message
- `-a, --all` - Show all interfaces
- `-s, --stats` - Show interface statistics

**Commands:**
- `up` - Bring interface up
- `down` - Bring interface down

**Examples:**
```
ifconfig                    # Show all interfaces
ifconfig eth0               # Show specific interface
ifconfig eth0 up            # Bring interface up
ifconfig eth0 down          # Bring interface down
```

**Output:**
```
eth0: flags=4163<UP,BROADCAST,MULTICAST>
  mtu 1500
  ether 00:11:22:33:44:55
  inet 192.168.1.100  netmask 255.255.255.0  broadcast 255.255.255.255
  RX packets 1234  bytes 567890
  TX packets 987  bytes 654321

lo: flags=4163<UP,BROADCAST,MULTICAST>
  mtu 65536
  ether 00:00:00:00:00:00
  inet 127.0.0.1  netmask 255.0.0.0  broadcast 127.0.0.1
  RX packets 42  bytes 12345
  TX packets 42  bytes 12345
```

**Features:**
- Interface status display
- MAC and IP address information
- MTU and netmask display
- Packet/byte statistics
- Interface up/down control

---

### netstat - Network Statistics and Socket Listing

Display network connections, routing tables, and interface statistics.

**Usage:**
```
netstat [options]
```

**Options:**
- `-h, --help` - Show help message
- `-a, --all` - Show all sockets (default)
- `-t, --tcp` - Show only TCP connections
- `-u, --udp` - Show only UDP sockets
- `-l, --listening` - Show only listening sockets
- `-s, --statistics` - Show protocol statistics
- `-r, --route` - Show routing table
- `-n, --numeric` - Don't resolve names

**Examples:**
```
netstat                     # Show all connections
netstat -t                  # Show TCP connections only
netstat -l                  # Show listening sockets
netstat -s                  # Show protocol statistics
netstat -r                  # Show routing table
```

**Output (Connections):**
```
Active TCP connections

Proto  Local Address          Foreign Address        State
tcp    192.168.1.100:8080     192.168.1.50:54321     ESTABLISHED
tcp    192.168.1.100:22       192.168.1.60:43210     ESTABLISHED
tcp    *:80                   *:*                    LISTEN

Active UDP sockets

Proto  Local Address
udp    *:53
udp    *:67
```

**Output (Statistics):**
```
Protocol Statistics

TCP:
  Active connections: 3
  Listening sockets: 2

UDP:
  Active sockets: 2

IP:
  Address: 192.168.1.100
```

**Output (Routing Table):**
```
Routing Table

Destination     Gateway         Netmask         Iface
default         192.168.1.1     0.0.0.0         eth0
192.168.1.0     0.0.0.0         255.255.255.0   eth0
```

**Features:**
- TCP connection state display
- UDP socket listing
- Listening socket identification
- Protocol statistics
- Routing table display
- TIME_WAIT state tracking

---

### traceroute - Trace Route to Network Host

Trace the packet route to a network host using TTL expiry or ICMP.

**Usage:**
```
traceroute [options] <target>
```

**Options:**
- `-h, --help` - Show help message
- `-m, --max-hops <n>` - Maximum number of hops (default: 30)
- `-q, --queries <n>` - Number of queries per hop (default: 3)
- `-w, --wait <n>` - Wait time per query in seconds (default: 5)
- `-p, --port <n>` - Base destination port (default: 33434)
- `-I, --icmp` - Use ICMP ECHO instead of UDP

**Examples:**
```
traceroute 8.8.8.8
traceroute -m 15 192.168.1.1
traceroute -I 8.8.8.8
traceroute -q 1 192.168.1.1
```

**Output:**
```
traceroute to 8.8.8.8 (8.8.8.8), 30 hops max
1  192.168.1.1  1.234 ms  1.156 ms  1.189 ms
2  10.0.0.1  5.678 ms  5.543 ms  5.621 ms
3  172.16.0.1  10.234 ms  10.156 ms  10.198 ms
4  8.8.8.8  15.432 ms  15.321 ms  15.389 ms

Trace complete
```

**Features:**
- TTL-based hop discovery
- ICMP and UDP probe support
- Multiple queries per hop
- RTT measurement per hop
- Timeout handling
- Path visualization

---

### nslookup - DNS Query Utility

Query DNS servers for domain name information.

**Usage:**
```
nslookup [options] <domain>
```

**Options:**
- `-h, --help` - Show help message
- `-t, --type <type>` - Query type (A, CNAME, MX, etc.) (default: A)
- `-s, --server <ip>` - DNS server to query (default: 8.8.8.8)
- `-p, --port <n>` - DNS server port (default: 53)
- `-d, --debug` - Show debug information

**Query Types:**
- `A` - IPv4 address
- `AAAA` - IPv6 address
- `CNAME` - Canonical name
- `MX` - Mail exchange
- `NS` - Name server
- `PTR` - Pointer record
- `SOA` - Start of authority
- `ANY` - All records

**Examples:**
```
nslookup example.com
nslookup -t MX example.com
nslookup -t CNAME www.example.com
nslookup -s 1.1.1.1 example.com
```

**Output:**
```
Querying example.com for A records...

Name:    example.com

A records:
  93.184.216.34 (TTL=3600)

Query time: 45 ms
```

**Output (MX Records):**
```
Querying example.com for MX records...

Name:    example.com

MX records:
  10 mail.example.com (TTL=3600)
  20 mail2.example.com (TTL=3600)
```

**Features:**
- Multiple DNS record types
- Configurable DNS server
- Response caching
- TTL display
- Query time measurement
- Debug mode

---

## DNS Resolver Library

The `lib/net/dns.inc` provides a complete DNS resolver implementation.

### Features

- **RFC-Compliant Protocol**: Implements DNS query/response format
- **Query Types**: A, AAAA, CNAME, MX, NS, PTR, SOA, ANY
- **Response Caching**: Automatic caching with TTL management
- **Name Compression**: Handles DNS name compression pointers
- **UDP Transport**: Uses UDP port 53 for queries
- **Configurable Servers**: Support for multiple DNS servers

### API

```lisp
; Initialize resolver with DNS servers
(dns/init (list
    (array 8 8 8 8)      ; Google DNS
    (array 1 1 1 1)))    ; Cloudflare DNS

; Query for specific record type
(defq answers (dns/query "example.com" dns_type_a))

; Convenience function for A records
(defq ip (dns/resolve "example.com"))

; Answer structure
; Each answer is an environment with:
;   :name - domain name
;   :type - record type (dns_type_a, dns_type_mx, etc.)
;   :class - record class (dns_class_in)
;   :ttl - time to live in seconds
;   :rdata - record data (IP address, name, etc.)
```

### DNS Cache

The resolver maintains an automatic cache:

- Entries cached by domain + query type
- TTL-based expiration
- Minimum TTL from all records used
- Automatic cache lookup before queries
- Improves performance for repeated queries

---

## Integration with TCP/IP Stack

All utilities integrate seamlessly with ChrysaLisp's TCP/IP stack:

```
Network Utilities (cmd/)
         ↓
    ┌─────────┬─────────┬─────────┐
    │  ICMP   │   UDP   │   TCP   │
    │ (ping)  │ (DNS)   │(netstat)│
    └─────────┴─────────┴─────────┘
         ↓         ↓         ↓
    ┌──────────────────────────────┐
    │      TCP/IP Stack            │
    │      (lib/net/)              │
    └──────────────────────────────┘
         ↓
    Network Interface
```

## Usage Examples

### Network Diagnostics Workflow

```bash
# 1. Check interface configuration
ifconfig

# 2. Ping default gateway
ping 192.168.1.1

# 3. Check route to external host
traceroute 8.8.8.8

# 4. Verify DNS resolution
nslookup example.com

# 5. Check active connections
netstat -a

# 6. View listening services
netstat -l

# 7. Check routing table
netstat -r
```

### DNS Investigation

```bash
# Query different record types
nslookup -t A example.com
nslookup -t MX example.com
nslookup -t NS example.com

# Use different DNS servers
nslookup -s 8.8.8.8 example.com
nslookup -s 1.1.1.1 example.com

# Debug mode
nslookup -d example.com
```

### Connection Monitoring

```bash
# Watch TCP connections
netstat -t

# Check TIME_WAIT states
netstat -t

# View statistics
netstat -s
```

## Implementation Details

### Coding Standards

All utilities follow ChrysaLisp coding conventions:

- **Functions**: `function-names` with hyphens
- **Variables**: `local_variables` with underscores
- **Constants**: `+constants` with plus prefix
- **Globals**: `*globals*` with earmuffs
- **State Management**: `defq` for local state, `setq` for mutation
- **Options Parsing**: Standard `lib/options/options.inc` library

### Command Structure

Each utility follows the standard ChrysaLisp command pattern:

1. Import required libraries
2. Define `main` function
3. Parse options with `options-parse`
4. Validate inputs
5. Execute operation
6. Format output to stdout
7. Exit with appropriate code

### Error Handling

- Validate all inputs (IP addresses, ports, etc.)
- Provide clear error messages
- Exit with non-zero code on failure
- Handle timeouts gracefully
- Check for required resources (interfaces, sockets, etc.)

## Testing

### Basic Tests

```bash
# Test ping
ping 127.0.0.1

# Test ifconfig
ifconfig lo

# Test netstat (with initialized stack)
netstat -s

# Test DNS (requires initialized stack and DNS)
nslookup localhost

# Test traceroute
traceroute 127.0.0.1
```

### Integration Tests

```bash
# Start TCP echo server
tcp-echo-server 7777 &

# Check it's listening
netstat -l

# Connect with client
tcp-client 127.0.0.1 7777 "Hello"

# Check connection
netstat -t
```

## Future Enhancements

Possible improvements:

- **ping**: Add flood mode, packet pattern specification
- **ifconfig**: Add IPv6 support, DHCP client
- **netstat**: Add column sorting, continuous monitoring mode
- **traceroute**: Add AS number lookup, visual path mapping
- **nslookup**: Add reverse DNS, DNSSEC validation
- **dig**: Full dig-compatible replacement with zone transfers
- **tcpdump**: Packet capture and analysis
- **iperf**: Network performance testing
- **route**: Explicit routing table management
- **arp**: ARP cache management utility

## References

- RFC 791 - Internet Protocol
- RFC 792 - Internet Control Message Protocol
- RFC 768 - User Datagram Protocol
- RFC 1035 - Domain Names - Implementation and Specification
- Stevens, W. Richard. "TCP/IP Illustrated, Volume 1"

## License

Part of the ChrysaLisp project.

## Credits

Network utilities implemented for ChrysaLisp, following the design philosophy of the original Unix networking tools while integrating with ChrysaLisp's distributed, message-passing architecture.
