# Network Stack Integration Guide

This document explains how the TCP/IP network stack integrates with ChrysaLisp through the host network driver.

## Architecture Overview

```
┌─────────────────────────────────────────────┐
│           Lisp Applications                  │
│  (ping, ifconfig, netstat, traceroute, etc)  │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│         Lisp Network Stack                   │
│  lib/net/*.lisp (TCP, UDP, IP, ICMP, ARP)   │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│      sys/net/class.vp (VP Assembly)          │
│    ABI Bridge: Lisp ↔ C Function Calls     │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│         vp64.cpp (Emulator)                  │
│  Register r4 = host_net_funcs pointer       │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│   src/host/net_pcap.cpp (C++ Driver)        │
│  Raw Ethernet via libpcap/Npcap             │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│     Host Network Interface                   │
│  (eth0, en0, Wi-Fi adapter, etc.)           │
└─────────────────────────────────────────────┘
```

## Component Details

### 1. C++ Host Driver (`src/host/net_pcap.cpp`)

**Purpose:** Provides raw Ethernet frame send/receive

**Functions exported** (via `host_net_funcs[]` array):
```cpp
int host_net_init(const char *iface, uint8_t *mac, uint8_t *ip,
                  uint8_t *netmask, uint8_t *gateway, uint32_t mtu);
int host_net_deinit();
int host_net_send(uint8_t *data, uint32_t length);
int host_net_recv(uint8_t *buffer, uint32_t buffer_size);
int host_net_poll();
int host_net_get_info(uint8_t *info_struct);
int host_net_get_stats(uint8_t *stats_struct);
```

**Build:**
```bash
# Linux (requires libpcap-dev)
export _HOST_NET=0
make

# Testing fallback (no libpcap required)
export _HOST_NET=1
make
```

### 2. VP Assembly Bridge (`sys/net/class.vp`)

**Purpose:** Wraps C functions for Lisp calling

**Methods provided:**
- `(host_net :net_init iface mac ip netmask gateway mtu)` → int
- `(host_net :net_deinit)` → int
- `(host_net :net_send data length)` → int
- `(host_net :net_recv buffer size)` → int
- `(host_net :net_poll)` → int
- `(host_net :net_get_info info_ptr)` → int
- `(host_net :net_get_stats stats_ptr)` → int

**ABI Convention:**
- VP register `r4` contains `host_net_funcs` pointer
- Uses `host-net-call` macro for C function invocation
- Handles register save/restore automatically

### 3. Runtime Integration

**main.cpp changes:**
```cpp
#ifdef _HOST_NET
extern void (*host_net_funcs[]);
#endif

// Pass to vp64:
vp64(..., host_net_funcs);  // or nullptr if not available
```

**vp64.cpp changes:**
```cpp
int vp64(..., int64_t* host_net_funcs)
{
    regs[0] = (int64_t)argv;
    regs[1] = (int64_t)host_os_funcs;
    regs[2] = (int64_t)host_gui_funcs;
    regs[3] = (int64_t)host_audio_funcs;
    regs[4] = (int64_t)host_net_funcs;  // ← NEW
    regs[15] = (int64_t)stack;
    ...
}
```

## Usage from Lisp

### Basic Network Operations

```lisp
; Import network bridge
(import "sys/net/class.vp")
(import "lib/net/ethernet.lisp")

; Initialize Ethernet layer
(eth/init my_mac my_send_fn)

; Send raw Ethernet frame
(defun my_send_fn (frame)
    (host_net :net_send frame (length frame)))

; Receive frames
(defq buffer (array 2048))
(defq len (host_net :net_recv buffer 2048))
(when (> len 0)
    (eth/process (slice buffer 0 len)))
```

### Using the TCP/IP Stack

```lisp
; Full example in apps/netdemo/ping.lisp
(import "lib/net/ip.lisp")
(import "lib/net/icmp.lisp")

; Initialize IP layer
(ip/init my_ip my_netmask my_gateway)

; Send ping
(icmp/send-ping target_ip ping_id seq_num data)

; Register echo reply handler
(icmp/register-echo-handler my_handler)
```

## Testing

### Integration Test

```bash
# Build with network support
export _HOST_NET=0  # or 1 for socket fallback
make

# Run integration test
./main_tui apps/test_network.lisp
```

### Expected Output

```
Network Driver Integration Test
===============================

Test 1: Checking network availability...
  ✓ Network functions available

Test 2: Getting network information...
  ✓ Network driver is initialized
  MAC: 00:11:22:33:44:55
  IP:  192.168.1.100
  Mask: 255.255.255.0
  Gateway: 192.168.1.1
  MTU: 1500

Test 3: Polling for packets...
  ✓ Poll successful: 0 packet(s) ready

✓ Integration test complete!
Network stack is ready to use.
```

## Build Options

### _HOST_NET Values

- **`_HOST_NET=0`**: Production driver (net_pcap.cpp)
  - **Requires:** libpcap-dev (Linux), libpcap (macOS), Npcap (Windows)
  - **Provides:** Raw Ethernet access (Layer 2)
  - **Use for:** Full TCP/IP stack functionality

- **`_HOST_NET=1`**: Testing driver (net_socket.cpp)
  - **Requires:** Standard sockets only
  - **Provides:** IP-level access (Layer 3)
  - **Use for:** Testing without libpcap, limited functionality

- **Not defined**: Network disabled
  - **Provides:** nullptr passed to vp64
  - **Network functions will fail gracefully**

### Platform-Specific Setup

#### Linux
```bash
sudo apt-get install libpcap-dev
export _HOST_NET=0
make
```

#### macOS
```bash
# libpcap included with OS
export _HOST_NET=0
make
```

#### Windows
```cmd
:: Install Npcap from https://npcap.com/
:: Download Npcap SDK
set _HOST_NET=0
set NPCAP_SDK=C:\npcap-sdk
make
```

## Troubleshooting

### "Network functions NOT available"

**Cause:** Built without `_HOST_NET` defined

**Solution:**
```bash
export _HOST_NET=0  # or 1
make clean && make
```

### "pcap.h: No such file or directory"

**Cause:** libpcap development files not installed

**Solution:**
```bash
# Linux
sudo apt-get install libpcap-dev

# macOS
brew install libpcap  # usually not needed

# Windows
# Download Npcap SDK from https://npcap.com/
```

### "Permission denied" when receiving packets

**Cause:** Raw packet capture requires elevated privileges

**Solution:**
```bash
# Run as root
sudo ./main_tui obj/x64/AMD64/sys/boot_image

# Or set capabilities (Linux only)
sudo setcap cap_net_raw,cap_net_admin=eip ./main_tui
```

## Performance Notes

- **libpcap (net_pcap.cpp)**: ~100,000+ packets/sec on modern hardware
- **Socket fallback (net_socket.cpp)**: Limited by kernel stack overhead
- **Zero-copy**: Driver uses memory-mapped buffers where possible
- **Polling**: Non-blocking poll for integration with ChrysaLisp event loop

## Security Considerations

- **Raw sockets require root/admin privileges**
- **Validate all incoming packet data** (done in lib/net/*.lisp)
- **Rate limiting** recommended for production use
- **Firewall rules** may block raw packet access

## Future Enhancements

- [ ] Add BPF (Berkeley Packet Filter) support for kernel-level filtering
- [ ] Implement packet buffer ring for improved throughput
- [ ] Add VLAN support
- [ ] Support for jumbo frames (MTU > 1500)
- [ ] Windows native driver (without Npcap dependency)

## References

- [libpcap documentation](https://www.tcpdump.org/manpages/pcap.3pcap.html)
- [Npcap for Windows](https://npcap.com/)
- [ChrysaLisp host interface pattern](../docs/ai_digest/porting.md)
- [TCP/IP stack implementation](../lib/net/README.md)
