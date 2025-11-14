# Building ChrysaLisp with Network Support

This document describes how to build ChrysaLisp with the network driver on different platforms.

## Network Driver Implementation

The network driver (`net_pcap.cpp`) uses **libpcap** for portable Ethernet frame capture/injection across all platforms:

- **Linux**: libpcap (native)
- **macOS**: libpcap (included with OS)
- **Windows**: WinPcap or Npcap

### Why libpcap?

- **Portable**: Single API works on Windows/Mac/Linux
- **Raw Ethernet Access**: Required for from-scratch TCP/IP stack
- **Mature**: Battle-tested library used by tcpdump, Wireshark, etc.
- **Performance**: Efficient packet capture with kernel-level filtering

## Prerequisites

### Linux

Install libpcap development files:

**Debian/Ubuntu:**
```bash
sudo apt-get install libpcap-dev
```

**Fedora/RHEL:**
```bash
sudo dnf install libpcap-devel
```

**Arch Linux:**
```bash
sudo pacman -S libpcap
```

### macOS

libpcap is included with macOS, no installation needed.

If building fails, install via Homebrew:
```bash
brew install libpcap
```

### Windows

Install **Npcap** (recommended) or WinPcap:

**Npcap** (recommended - maintained, supports Windows 10/11):
1. Download from: https://npcap.com/
2. Run installer with "Install Npcap in WinPcap API-compatible Mode"
3. Download Npcap SDK from: https://npcap.com/#download
4. Extract SDK to `C:\npcap-sdk\` or set `NPCAP_SDK` environment variable

**WinPcap** (legacy):
1. Download from: https://www.winpcap.org/
2. Install WinPcap
3. Download WinPcap Developer's Pack
4. Extract to `C:\WpdPack\`

## Building

### Linux

```bash
# Enable network driver
export HOST_NET=1

# Build
cd src/host
make clean
make

# The network driver will be compiled into main_tui/main_gui
```

### macOS

```bash
# Enable network driver
export HOST_NET=1

# Build
cd src/host
make clean
make

# If libpcap headers not found, specify path:
export CFLAGS="-I/usr/local/opt/libpcap/include"
export LDFLAGS="-L/usr/local/opt/libpcap/lib"
make
```

### Windows (Visual Studio)

**Using CMake:**
```cmd
# Set Npcap SDK path
set NPCAP_SDK=C:\npcap-sdk

# Generate build files
mkdir build
cd build
cmake .. -DHOST_NET=ON -DNPCAP_SDK=%NPCAP_SDK%

# Build
cmake --build .
```

**Using MinGW/MSYS2:**
```bash
# Install dependencies
pacman -S mingw-w64-x86_64-libpcap

# Build
export HOST_NET=1
cd src/host
make clean
make
```

## Makefile Integration

Add to `src/host/Makefile`:

```makefile
# Network driver support
ifdef HOST_NET
	CPPFLAGS += -D_HOST_NET=$(HOST_NET)

	# Linux/macOS
	ifneq ($(OS),Windows_NT)
		LDLIBS += -lpcap
	else
		# Windows - WinPcap/Npcap
		ifdef NPCAP_SDK
			CPPFLAGS += -I$(NPCAP_SDK)/Include
			LDLIBS += -L$(NPCAP_SDK)/Lib/x64 -lwpcap -lws2_32
		else
			# Assume WpdPack
			CPPFLAGS += -IC:/WpdPack/Include
			LDLIBS += -LC:/WpdPack/Lib/x64 -lwpcap -lws2_32
		endif
	endif

	# Add network driver object
	OBJS += net_pcap.o
endif
```

## Running with Network Support

### Linux

**Requires root/sudo** for raw packet capture:

```bash
# Run as root
sudo ./main_tui obj/x64/AMD64/sys/boot_image [args]

# Or use capabilities (preferred)
sudo setcap cap_net_raw,cap_net_admin=eip ./main_tui
./main_tui obj/x64/AMD64/sys/boot_image [args]
```

### macOS

**Requires root/sudo**:

```bash
sudo ./main_tui obj/arm64/ARM64/sys/boot_image [args]
```

### Windows

**Run as Administrator** or with Npcap installed in WinPcap compatibility mode:

```cmd
main_tui.exe obj\x64\AMD64\sys\boot_image [args]
```

## Interface Selection

The network driver will automatically select the first available non-loopback interface. To specify an interface:

**Linux:**
```lisp
; In ChrysaLisp
(host-net-init "eth0" ...)
```

**macOS:**
```lisp
(host-net-init "en0" ...)
```

**Windows:**
```lisp
; Use device name from "getmac /v" output
(host-net-init "\\Device\\NPF_{GUID}" ...)
```

## Testing

### 1. Check Available Interfaces

**Linux/macOS:**
```bash
# List interfaces
ip link show        # Linux
ifconfig           # macOS

# Test pcap installation
tcpdump -D
```

**Windows:**
```cmd
# List interfaces
ipconfig /all

# Or in Npcap/WinPcap
WinDump -D
```

### 2. Test Network Driver

Create `test_network.lisp`:

```lisp
(import "lib/net/ethernet.inc")
(import "lib/net/ip.inc")

; Initialize network
(defq mac (array 0x00 0x11 0x22 0x33 0x44 0x55)
      ip (array 192 168 1 100)
      netmask (array 255 255 255 0)
      gateway (array 192 168 1 1))

; Call host network init via PII
; (This will be implemented in the ChrysaLisp bridge)

(print "Network driver test")
(prinl)
```

### 3. Run Network Utilities

```bash
# Ping
./ping 192.168.1.1

# Interface info
./ifconfig

# Network stats
./netstat -a
```

## Troubleshooting

### Linux

**Error: "Permission denied"**
- Run with sudo or set capabilities: `sudo setcap cap_net_raw=eip ./main_tui`

**Error: "No suitable device found"**
- Check interfaces: `ip link show`
- Ensure interface is up: `sudo ip link set eth0 up`

**Error: "libpcap.so: cannot open shared object file"**
- Install libpcap: `sudo apt-get install libpcap0.8`

### macOS

**Error: "You don't have permission to capture"**
- Run with sudo: `sudo ./main_tui ...`
- Or add user to access_bpf group (macOS 10.14+)

**Error: "dyld: Library not loaded: libpcap.dylib"**
- Install via Homebrew: `brew install libpcap`

### Windows

**Error: "The NPF driver isn't running"**
- Install/reinstall Npcap
- Start Npcap service: `net start npcap`

**Error: "wpcap.dll not found"**
- Reinstall Npcap/WinPcap
- Add to PATH: `C:\Windows\System32\Npcap\` or `C:\Windows\System32\`

**Error: "Access is denied"**
- Run as Administrator
- Ensure Npcap is installed with WinPcap compatibility mode

## Performance Considerations

### Packet Filtering

For better performance, set BPF filters to only capture relevant traffic:

```c
// Example: Only capture IPv4 traffic
struct bpf_program fp;
pcap_compile(handle, &fp, "ip", 0, PCAP_NETMASK_UNKNOWN);
pcap_setfilter(handle, &fp);
pcap_freecode(&fp);
```

### Buffer Size

Increase buffer size for high-throughput scenarios:

```c
// Linux
pcap_set_buffer_size(handle, 2 * 1024 * 1024);  // 2MB
```

### Promiscuous Mode

The driver enables promiscuous mode by default. Disable if only own traffic needed:

```c
pcap_open_live(device, snaplen, 0, timeout_ms, errbuf);  // 0 = no promisc
```

## Alternative: TAP/TUN Devices

For testing without raw access, use TAP/TUN virtual interfaces:

**Linux:**
```bash
# Create TAP device
sudo ip tuntap add dev tap0 mode tap
sudo ip addr add 192.168.100.1/24 dev tap0
sudo ip link set tap0 up
```

**macOS:**
```bash
# Install TunTap
brew install --cask tuntap

# Create TAP device
sudo kextload /Library/Extensions/tap.kext
```

**Windows:**
- Install TAP-Windows from OpenVPN
- Create TAP adapter in Network Connections

## Security Notes

- **Root/Admin required**: Raw packet capture needs elevated privileges
- **Promiscuous mode**: Can see all network traffic on the segment
- **Firewall**: May need to allow ChrysaLisp through host firewall
- **Network policy**: Check organizational policies before using

## References

- libpcap documentation: https://www.tcpdump.org/
- Npcap documentation: https://npcap.com/guide/
- WinPcap documentation: https://www.winpcap.org/docs/
- Berkeley Packet Filter (BPF): https://www.kernel.org/doc/html/latest/networking/filter.html
