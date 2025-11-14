# ChrysaLisp Network Driver API

## Overview

The ChrysaLisp network driver provides a host interface for Ethernet frame transmission and reception, following the same pattern as the GUI and Audio drivers.

## Driver Implementations

### 1. net_pcap.cpp (_HOST_NET=0) - **RECOMMENDED**

**Portable raw Ethernet driver using libpcap**

- **Platforms**: Linux, macOS, Windows
- **Access Level**: Raw Ethernet frames (Layer 2)
- **Dependencies**: libpcap (Linux/Mac), WinPcap/Npcap (Windows)
- **Use Case**: Production use, full TCP/IP stack implementation
- **Privileges**: Requires root/admin

**Advantages:**
- ✅ True raw Ethernet access
- ✅ Single portable API across all platforms
- ✅ Battle-tested (used by tcpdump, Wireshark)
- ✅ Efficient kernel-level packet filtering
- ✅ Full control over Ethernet frames

**Installation:**
- Linux: `sudo apt-get install libpcap-dev`
- macOS: Included with OS
- Windows: Install Npcap from https://npcap.com/

### 2. net_socket.cpp (_HOST_NET=1) - **TESTING ONLY**

**Standard socket-based driver (IP-level access)**

- **Platforms**: Linux, macOS, Windows
- **Access Level**: IP layer (Layer 3)
- **Dependencies**: None (standard sockets)
- **Use Case**: Testing, development, fallback
- **Privileges**: May require root for raw sockets

**Limitations:**
- ⚠️ No true Ethernet frame access
- ⚠️ Cannot implement ARP
- ⚠️ Limited to IP-level operations
- ⚠️ Not suitable for from-scratch TCP/IP stack

**Advantages:**
- ✅ No external dependencies
- ✅ Compiles everywhere
- ✅ Good for testing utilities (ping, traceroute)

### 3. net_raw.cpp (Legacy) - **DEPRECATED**

Original Linux-only implementation using AF_PACKET. Replaced by net_pcap.cpp.

## API Functions

All implementations provide the same API:

### host_net_init
```c
int host_net_init(const char *iface, uint8_t *mac, uint8_t *ip,
                  uint8_t *netmask, uint8_t *gateway, uint32_t mtu)
```
Initialize network interface.

**Parameters:**
- `iface` - Interface name ("eth0", "en0", etc.) or NULL for auto-detect
- `mac` - MAC address (6 bytes) or NULL to use interface MAC
- `ip` - IP address (4 bytes)
- `netmask` - Network mask (4 bytes)
- `gateway` - Gateway address (4 bytes)
- `mtu` - Maximum Transmission Unit (typically 1500)

**Returns:** 0 on success, -1 on error

### host_net_deinit
```c
int host_net_deinit()
```
Shutdown network interface and free resources.

**Returns:** 0 on success

### host_net_send
```c
int host_net_send(uint8_t *data, uint32_t length)
```
Send Ethernet frame.

**Parameters:**
- `data` - Frame data (including Ethernet header)
- `length` - Frame length in bytes

**Returns:** Number of bytes sent, or -1 on error

### host_net_recv
```c
int host_net_recv(uint8_t *buffer, uint32_t buffer_size)
```
Receive Ethernet frame (non-blocking).

**Parameters:**
- `buffer` - Buffer to store frame
- `buffer_size` - Size of buffer

**Returns:** Number of bytes received, 0 if no data, -1 on error

### host_net_poll
```c
int host_net_poll()
```
Check if received data is available.

**Returns:** 1 if data available, 0 if not, -1 on error

### host_net_get_info
```c
int host_net_get_info(void *info)
```
Get interface configuration.

**Buffer Layout (22 bytes):**
- 6 bytes: MAC address
- 4 bytes: IP address
- 4 bytes: Netmask
- 4 bytes: Gateway
- 4 bytes: MTU (uint32_t)

**Returns:** 0 on success, -1 on error

### host_net_get_stats
```c
int host_net_get_stats(void *stats)
```
Get interface statistics.

**Buffer Layout (32 bytes):**
- 8 bytes: RX packets (uint64_t)
- 8 bytes: RX bytes (uint64_t)
- 8 bytes: TX packets (uint64_t)
- 8 bytes: TX bytes (uint64_t)

**Returns:** 0 on success, -1 on error

## Function Table

```c
void (*host_net_funcs[]) = {
    (void*)host_net_init,
    (void*)host_net_deinit,
    (void*)host_net_send,
    (void*)host_net_recv,
    (void*)host_net_poll,
    (void*)host_net_get_info,
    (void*)host_net_get_stats,
};
```

## Integration with ChrysaLisp

### main.cpp Integration

```cpp
#ifdef _HOST_NET
extern void (*host_net_funcs[]);
#endif

// In main():
#ifdef _HOST_NET
    ret_val = vp64(..., host_os_funcs, host_gui_funcs,
                    host_audio_funcs, host_net_funcs);
#else
    ret_val = vp64(..., host_os_funcs, host_gui_funcs,
                    host_audio_funcs, nullptr);
#endif
```

### vp64.cpp Integration

```cpp
// Add network function table parameter
int vp64(uint8_t* data, int64_t *stack, int64_t *argv,
         int64_t *host_os_funcs, int64_t *host_gui_funcs,
         int64_t *host_audio_funcs, int64_t *host_net_funcs);

// Network ABI calls (similar to GUI/Audio)
case VP64_CALL_ABI_NET_INIT:
case VP64_CALL_ABI_NET_SEND:
case VP64_CALL_ABI_NET_RECV:
// ... etc
```

### ChrysaLisp Bridge (sys/net/class.vp)

```lisp
(def-method :net_init sys/net/net_init)
(def-method :net_send sys/net/net_send)
(def-method :net_recv sys/net/net_recv)
```

## Building

### Makefile Changes

```makefile
# Network driver support
ifdef HOST_NET
    CPPFLAGS += -D_HOST_NET=$(HOST_NET)

    ifeq ($(HOST_NET),0)
        # libpcap driver
        LDLIBS += -lpcap
        OBJS += net_pcap.o
    else
        # Socket driver
        OBJS += net_socket.o
    endif
endif
```

### Build Commands

**With libpcap (recommended):**
```bash
export HOST_NET=0
make clean
make
```

**With sockets (testing):**
```bash
export HOST_NET=1
make clean
make
```

**Without network:**
```bash
unset HOST_NET
make clean
make
```

## Usage Examples

### From ChrysaLisp

```lisp
; Initialize network
(import "lib/net/ethernet.lisp")

; Setup function to call host driver
(defun net/init-host ()
    ; Call host_net_init via PII
    (host-net-init "eth0"
                   (array 0x00 0x11 0x22 0x33 0x44 0x55)  ; MAC
                   (array 192 168 1 100)                   ; IP
                   (array 255 255 255 0)                   ; Netmask
                   (array 192 168 1 1)                     ; Gateway
                   1500))                                  ; MTU

; Send frame
(defun net/send-frame (frame)
    (host-net-send frame (length frame)))

; Receive frame
(defun net/recv-frame ()
    (defq buffer (array))
    (defq len (host-net-recv buffer 2048))
    (if (> len 0) buffer nil))
```

### Testing

```bash
# Run with network (requires root/admin)
sudo ./main_tui obj/x64/AMD64/sys/boot_image

# Test ping
ping 192.168.1.1

# Test interface config
ifconfig
```

## Platform-Specific Notes

### Linux

- Requires root or `CAP_NET_RAW` capability
- Use `sudo setcap cap_net_raw=eip ./main_tui` to avoid sudo
- Interfaces: eth0, wlan0, etc.
- libpcap usually pre-installed

### macOS

- Requires root (sudo)
- Interfaces: en0, en1, etc.
- libpcap included with OS
- BPF devices: /dev/bpf*

### Windows

- Requires Administrator privileges
- Interfaces: Complex GUID-based names
- Requires Npcap or WinPcap installation
- WinPcap compatibility mode recommended

## Performance Considerations

### Packet Filtering

Use BPF filters to reduce CPU usage:

```c
struct bpf_program fp;
pcap_compile(handle, &fp, "ip or arp", 0, PCAP_NETMASK_UNKNOWN);
pcap_setfilter(handle, &fp);
```

### Buffer Sizing

Increase buffers for high throughput:

```c
pcap_set_buffer_size(handle, 2 * 1024 * 1024);  // 2MB
```

### Non-blocking I/O

All implementations use non-blocking mode:
- Poll before recv to avoid blocking
- Handle EAGAIN/EWOULDBLOCK properly

## Security

- **Elevated Privileges**: Raw packet access requires root/admin
- **Promiscuous Mode**: Can see all network traffic
- **Injection**: Can send arbitrary packets
- **Firewall**: May need configuration

**Best Practices:**
- Drop privileges after initialization where possible
- Validate all received packets
- Rate-limit transmission
- Filter unnecessary traffic with BPF

## Troubleshooting

### "Permission denied"
- Run with sudo/admin
- Linux: Set capabilities with setcap
- Ensure Npcap/WinPcap installed (Windows)

### "No suitable device found"
- Check interface name (ifconfig/ipconfig)
- Ensure interface is up
- Try different interface

### "wpcap.dll not found" (Windows)
- Reinstall Npcap/WinPcap
- Check PATH includes Npcap directory

### Poor performance
- Enable BPF filtering
- Increase buffer size
- Check for packet loss in stats

## Future Enhancements

- Async I/O support (io_uring on Linux)
- Zero-copy packet handling
- Hardware offload support
- Multi-queue support
- DPDK integration option

## References

- libpcap: https://www.tcpdump.org/
- Npcap: https://npcap.com/
- BPF: https://www.kernel.org/doc/html/latest/networking/filter.html
- Raw sockets: Various OS documentation
