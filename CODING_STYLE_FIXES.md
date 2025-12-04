# Coding Style Fixes for Network Stack

## Issues Found

All network `.lisp` files violate ChrysaLisp coding conventions by using **hyphens in variable and parameter names**.

### ChrysaLisp Naming Conventions

✅ **Correct:**
- `function-names` and `macro-names` use **hyphens** `-`
- `local_variables` and parameters use **underscores** `_`
- `+constants` use **plus prefix**
- `*globals*` use **asterisk earmuffs**

❌ **My Violations:**
- Used hyphens in local variable names (should be underscores)
- Used hyphens in function parameter names (should be underscores)

## Files Requiring Fixes

### lib/net/*.lisp
- utils.lisp
- arp.lisp
- ip.lisp
- icmp.lisp
- udp.lisp
- tcp.lisp
- tcp_state.lisp
- ethernet.lisp
- socket.lisp
- dns.lisp

### cmd/*.lisp
- ping.lisp
- ifconfig.lisp
- netstat.lisp
- traceroute.lisp
- nslookup.lisp

### apps/netdemo/*.lisp
- net_init.lisp
- ping.lisp
- udp_echo_server.lisp
- tcp_echo_server.lisp
- tcp_client.lisp

## Common Violations Pattern

```lisp
; ❌ WRONG - hyphens in variables
(defq ip-bytes (array 192 168 1 1))
(defq arp-pkt (arp/parse data))
(defq dst-mac (get-mac addr))

; ✅ CORRECT - underscores in variables
(defq ip_bytes (array 192 168 1 1))
(defq arp_pkt (arp/parse data))
(defq dst_mac (get-mac addr))

; ❌ WRONG - hyphens in parameters
(defun net/ip-to-string (ip-bytes)
    ...)

; ✅ CORRECT - underscores in parameters
(defun net/ip-to-string (ip_bytes)
    ...)
```

## Specific Examples to Fix

### utils.lisp
```lisp
; Parameters
ip-bytes → ip_bytes
ip-str → ip_str
mac-bytes → mac_bytes
mac-str → mac_str
src-off → src_off
dst-off → dst_off

; Variables
ip-val → ip_val
mask-val → mask_val
net-val → net_val
```

### arp.lisp
```lisp
; Variables
arp-pkt → arp_pkt
src-mac, dst-mac → src_mac, dst_mac
src-ip, dst-ip → src_ip, dst_ip
```

### ip.lisp
```lisp
; Variables
ip-pkt → ip_pkt
ver-ihl → ver_ihl
hdr-len → hdr_len
total-len → total_len
frag-offset → frag_offset
src-ip, dst-ip → src_ip, dst_ip
```

### icmp.lisp
```lisp
; Variables
icmp-pkt → icmp_pkt
orig-packet → orig_packet
copy-len → copy_len
```

### udp.lisp
```lisp
; Variables
udp-pkt → udp_pkt
src-port, dst-port → src_port, dst_port
total-len → total_len
```

### tcp.lisp & tcp_state.lisp
```lisp
; Variables
tcp-pkt → tcp_pkt
conn-id → conn_id
data-offset → data_offset
ack-num, seq-num → ack_num, seq_num
src-port, dst-port → src_port, dst_port
```

### ethernet.lisp
```lisp
; Variables
eth-frame → eth_frame
dst-mac, src-mac → dst_mac, src_mac
min-len → min_len
```

### dns.lisp
```lisp
; Variables
encoded-name → encoded_name
query-pkt → query_pkt
cache-key → cache_key
answer-result → answer_result
mx-name → mx_name
```

### cmd/*.lisp
```lisp
; Variables (across all command files)
target-ip, server-ip → target_ip, server_ip
ping-id → ping_id
local-addr, remote-addr → local_addr, remote_addr
conn-count, listen-count → conn_count, listen_count
if-name, if-data → if_name, if_data
```

## State Management Notes

While fixing, also verify:
1. ✅ Using `defq` for local variable creation (correct in all files)
2. ✅ Using `setq` for mutation (correct in all files)
3. ✅ Fusing operations where appropriate
4. ✅ Not shadowing built-in functions (verified - no violations)

## Action Required

Create corrected versions of all files with proper underscore naming for:
- All function parameters
- All local variables created with `defq`
- All variables used in `setq`, `when`, `if`, etc.

Keep hyphens only in:
- Function names (e.g., `net/ip-to-string`)
- Macro names
- Keywords (e.g., `:src-ip` in property maps)

## Testing After Fixes

All files must be tested to ensure:
1. Syntax is valid
2. Functions work correctly
3. No regressions in TCP/IP stack or utilities
