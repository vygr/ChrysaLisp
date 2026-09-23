---
name: chrysalisp-net-testing
display-name: ChrysaLisp Multi-Instance & Network Link Testing
description: Use when running, debugging, or testing network links, server/client protocols, and multi-instance ChrysaLisp setups using -b and background processes.
---

# ChrysaLisp Multi-Instance & Network Link Testing Skill

This skill documents the available ChrysaLisp network tests in `tests/net/`, how they operate, and how to run them across single-machine and multi-machine environments.

---

## 1. Network Test Inventory in `tests/net/`

The `tests/net/` directory contains three categories of network tests:

| Category | File(s) | Execution Mode | Scope |
| :--- | :--- | :--- | :--- |
| **Unit Test Suite** | `test_url.lisp`, `test_json.lisp` | Standard test suite (`tests`) | In-process URL & JSON parsing |
| **Loopback Link** | `test_loopback.sh` (`srv_loopback.lisp`, `cli_loopback.lisp`) | `./tests/net/test_loopback.sh` | Single-machine multi-instance TCP link |
| **Cluster Diagnostic** | `test_cluster.lisp` | `./run_tui.sh -f -s tests/net/test_cluster.lisp` | Physical LAN multi-machine cluster probe |

---

## 2. In-Process Unit Tests (`test_url.lisp`, `test_json.lisp`)

These modules are integrated into the canonical ChrysaLisp test suite in `tests/run_all.lisp`:

*	`test_url.lisp`: Tests URL encoding, decoding, path splitting, hex-escaping, and query parameter extraction.
*	`test_json.lisp`: Tests JSON tokenization, nested objects, arrays, numbers, and string escaping.

### Running via Test Harness

From the host shell (always pipe through `grep` to save tokens):
```bash
echo "tests" | ./run_tui.sh -f 2>&1 | grep -E "\[FAIL\]|\[SKIP\]|Passed:|Failed:|RESULT"
```

Inside an interactive TUI or Terminal session:
```lisp
tests
```

---

## 3. Automated Single-Machine Loopback Test (`test_loopback.sh`)

Validates TCP point-to-point network links (`service/net/link`), inter-node routing, and remote task dispatch (`open-remote`) on a single machine without requiring LAN peers or network access.

### How It Works

*	**Driver**: `tests/net/test_loopback.sh` orchestrates two independent ChrysaLisp VM instances on the local machine:
	1. Runs `./stop.sh` to ensure a clean slate.
	2. Launches the server instance (`tests/net/srv_loopback.lisp`) on CPU base 0, listening on port `:4567`.
	3. Polls with `lsof` until port 4567 is active.
	4. Launches the client instance (`tests/net/cli_loopback.lisp`) with CPU offset `-b 10`.
	5. The client connects to `127.0.0.1:4567`, discovers all 10 remote server nodes (20 nodes total), and dispatches `(kernel-stats)` tasks to every remote node via `open-remote`.
	6. Collects and validates all responses with rolling timeout and verifies all results originated from remote nodes.
	7. Traps exit to terminate background processes via `./stop.sh`.

### How to Run

```bash
./tests/net/test_loopback.sh
```

A successful run terminates with:
```
=== LOOPBACK TEST RESULT: SUCCESS ===
```

---

## 4. Multi-Machine Cluster Diagnostic Tool (`test_cluster.lisp`)

Inspects and verifies a live multi-machine ChrysaLisp cluster across local and remote physical machines over the local network (LAN).

### How It Works

`tests/net/test_cluster.lisp`:
1. **Starts `@Net` Service**: Checks `(mail-enquire "@Net,")` and automatically launches `(open-child "service/net/app.lisp" +kn_call_run)` if not already active.
2. **Dynamically Stabilizes Local Nodes**: Calls `(net-quiet 500000 6)` to wait until all local CPU node background processes finish booting and settle (no hardcoded node counts).
3. **Starts LAN Auto-Discovery**: Executes `(pipe-run "link -a" prin)` to listen for UDP broadcast beacons from network peers on port 3334.
4. **Waits for Peers & Stabilizes**: Dynamically waits for peer nodes to appear (`(> (length (lisp-nodes)) (length local_nodes))`) and stabilizes cluster topology with `(net-quiet 500000 8)` (4 seconds of network silence).
5. **Probes Entire Cluster**: Dispatches `cluster -v` to launch non-blocking asynchronous probes concurrently via `+kn_call_pin` across all nodes on all machines.
6. **Reports Topology Summary**: Reports CPU/OS/ABI architecture per machine, task counts, memory usage, stack depth, and discovered services (`@Net`, `@Lock`, `Terminal`), validating zero bad task counts.

### How to Run

Test under **both** native host and VP64 emulator modes:

*	**Native Host Execution:**
	```bash
	./run_tui.sh -f -s tests/net/test_cluster.lisp
	```

*	**VP64 Emulator Mode (`-e`):**
	```bash
	./run_tui.sh -e -f -s tests/net/test_cluster.lisp
	```

A successful run terminates with:
```
=== CLUSTER QUERY: SUCCESS ===
```

---

## 5. The `-b` (Base CPU Offset) Mechanism

When debugging or writing custom network scripts across multiple instances on one machine:

*	**Instance 0 (Server)**: Launch without `-b` (default `base_cpu=0`). It executes `./stop.sh` on startup to clean up stale processes and binds ports on base nodes (0..9).
*	**Instance 1 (Client)**: Launch with `-b 10`. The non-zero base offset instructs `funcs.sh` **not** to run `./stop.sh`, allowing the client to run alongside the background server on node IDs 10..19.
*	**Exit Behavior**: Omitting `-f` ensures the process does not terminate other instances when it exits.
