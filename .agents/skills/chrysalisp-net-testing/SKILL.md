---
name: chrysalisp-net-testing
display-name: ChrysaLisp Multi-Instance & Network Link Testing
description: Use when running, debugging, or testing network links, server/client protocols, and multi-instance ChrysaLisp setups using -b and background processes.
---

# ChrysaLisp Multi-Instance & Network Link Testing Skill

This skill documents the available ChrysaLisp network tests in `tests/net/`, how they operate, and how to run them across single-machine and multi-machine environments.

---

## Contents

Find the task below and read that section in full before acting.

*	**[1. Network Test Inventory in `tests/net/`](#1-network-test-inventory-in-testsnet)**
	What each network test is, how it runs, and what it covers.

*	**[2. In-Process Unit Tests (`test_url.lisp`, `test_json.lisp`)](#2-in-process-unit-tests-test_urllisp-test_jsonlisp)**
	The URL and JSON tests that run inside the normal suite.

*	**[3. Automated Single-Machine Loopback Test (`test_loopback.sh`)](#3-automated-single-machine-loopback-test-test_loopbacksh)**
	Two instances on one machine joined by a TCP link. How it works and how to
	run it.

*	**[4. Multi-Machine Cluster Diagnostic Tool (`test_cluster.lisp`)](#4-multi-machine-cluster-diagnostic-tool-test_clusterlisp)**
	Checking a live cluster across machines on the LAN.

*	**[5. Driving Another Machine (`remote_onslaught.lisp`)](#5-driving-another-machine-remote_onslaughtlisp)**
	A worked example: open a game on another machine's GUI, get the mailbox
	id of its service back, and play it from here. How to reach a service
	whose name is not seen across machines.

*	**[6. The `-b` (Base CPU Offset) Mechanism](#6-the--b-base-cpu-offset-mechanism)**
	Read before launching a second instance by hand, it is what stops it
	killing the first.

## 1. Network Test Inventory in `tests/net/`

The `tests/net/` directory contains three categories of network tests:

| Category | File(s) | Execution Mode | Scope |
| :--- | :--- | :--- | :--- |
| **Unit Test Suite** | `test_url.lisp`, `test_json.lisp` | Standard test suite (`tests`) | In-process URL & JSON parsing |
| **Loopback Link** | `test_loopback.sh` (`srv_loopback.lisp`, `cli_loopback.lisp`) | `./tests/net/test_loopback.sh` | Single-machine multi-instance TCP link |
| **Cluster Diagnostic** | `test_cluster.lisp` | `./run_tui.sh -f -s tests/net/test_cluster.lisp` | Physical LAN multi-machine cluster probe |

---

## 2. In-Process Unit Tests (`test_url.lisp`, `test_json.lisp`)

These modules are part of the ChrysaLisp test suite, found automatically like any `tests/<category>/test_*.lisp` file:

*	`test_url.lisp`: Tests URL encoding, decoding, path splitting, hex-escaping, and query parameter extraction.
*	`test_json.lisp`: Tests JSON tokenization, nested objects, arrays, numbers, and string escaping.

### Running via Test Harness

From the host shell, `-m net` runs just these modules:
```bash
echo "tests -m net" | ./run_tui.sh -f
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

## 5. Driving Another Machine (`remote_onslaught.lisp`)

`tests/net/remote_onslaught.lisp` is a worked example of doing real work on
another machine over a link. It finds the other machine with `link -a`,
probes its nodes for the one that can see the `Gui` service, and pins a task
there that opens the Onslaught game and mails back the mailbox id of the
game's service. The bot is then run on this machine, `onslaught -m id -b 60`,
playing the game on the other one over the link, reading its state and
setting its keys 20 times a second. The other machine needs `./run.sh` and
`link -l 3333 -a`. Run it with:

```bash
./run_tui.sh -f -s tests/net/remote_onslaught.lisp
```

The game declares itself as `@Onslaught`, and an `@` name is only seen on its
own machine, so a `(mail-enquire)` from here does not find it. But a mailbox
is good from anywhere, mail is routed by its id. So the way to reach a system
wide or local service on another machine is to have a task over there look
the name up and send the mailbox id back.

The same pattern, a task string given to `(open-task code node +kn_call_pin 0
mbox)` that mails its result to a reply mailbox passed in as hex, runs
anything on a remote node, the unit tests with `(pipe-run "tests" ...)` for
one. It is not a suite module, it is not named `test_`.

---

## 6. The `-b` (Base CPU Offset) Mechanism

When debugging or writing custom network scripts across multiple instances on one machine:

*	**Instance 0 (Server)**: Launch without `-b` (default `base_cpu=0`). It executes `./stop.sh` on startup to clean up stale processes and binds ports on base nodes (0..9).
*	**Instance 1 (Client)**: Launch with `-b 10`. The non-zero base offset instructs `funcs.sh` **not** to run `./stop.sh`, allowing the client to run alongside the background server on node IDs 10..19.
*	**Exit Behavior**: Omitting `-f` ensures the process does not terminate other instances when it exits.
