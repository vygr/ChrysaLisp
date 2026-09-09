---
name: chrysalisp-net-testing
display-name: ChrysaLisp Multi-Instance & Network Link Testing
description: Use when running, debugging, or testing network links, server/client protocols, and multi-instance ChrysaLisp setups using -b and background processes.
---

# ChrysaLisp Multi-Instance & Network Link Testing Skill

This skill explains how to run and debug two separate ChrysaLisp VM instances on a single machine (e.g. a server and a client) to test system-to-system TCP links (`service/net/link`), inter-node messaging, and distributed services without needing a physical second machine.

---

## 1. The `-b` (Base CPU Offset) Mechanism

The ChrysaLisp shell launcher (`funcs.sh`, used by `run_tui.sh` and `run_mesh.sh`) manages process lifecycles and node CPU IDs:

- **Startup Cleanup**: If `base_cpu -eq 0` (the default when `-b` is omitted), `funcs.sh` runs `./stop.sh` on startup, which terminates all running `main_tui` / `main_gui` processes.
- **Independent Instances with `-b`**: If `base_cpu` is non-zero (e.g. `-b 10`), `funcs.sh` does **NOT** call `./stop.sh` on startup. The new instance runs independently with CPU node offsets shifted by `base_cpu`.
- **Exit Cleanup with `-f`**: If foreground mode `-f` is passed, `boot_cpu_tui` will call `./stop.sh` when the process exits with code 0. Omitting `-f` ensures the process does not shut down other instances upon exit.

---

## 2. Server & Client Testing Workflow

### Step 1: Start Server in Background (Without `-b`)
Launch the server without `-b` in the background with `&`, redirecting output to a dedicated log file:

```bash
./run_tui.sh -n 1 -s scratch/run_srv.lisp > scratch/server.log 2>&1 &
```

- Because `-b` is omitted (`base_cpu=0`), it runs `./stop.sh` once at startup, cleaning up any stale processes.
- The server starts fresh, binds its listening port (e.g. `4444` or `3333`), and remains running in the background.

Check that the server is listening:
```bash
head -n 10 scratch/server.log
```

### Step 2: Run Client with `-b 10`
Launch the client instance using `-b 10`:

```bash
./run_tui.sh -b 10 -n 1 -s scratch/run_cli.lisp
```

- Because `base_cpu` is `10`, the client does **NOT** run `./stop.sh`.
- The background server remains completely undisturbed.
- You can re-run client sessions repeatedly against the same running server instance.

### Step 3: Inspect Both Sides

- **Client side**: Streams directly to your terminal standard output.
- **Server side**: Inspect live with `tail -f scratch/server.log` or read `scratch/server.log` after the test.

### Step 4: Cleanup
When finished with testing, terminate all background instances:

```bash
./stop.sh
```

---

## 3. Reference Test Scripts

### Server Script (`scratch/run_srv.lisp`)
```lisp
(print "=== SERVER LISTENING ON :4444 ===")
(mail-send (open-child "service/net/link" +kn_call_open) ":4444")
(while :t
	(task-sleep 1000000))
```

### Client Script (`scratch/run_cli.lisp`)
```lisp
(print "=== CLIENT CONNECTING TO 127.0.0.1:4444 ===")
(mail-send (open-child "service/net/link" +kn_call_open) "127.0.0.1:4444")
(task-sleep 3000000)
(print "=== CLIENT SESSION FINISHED ===")
((ffi "service/gui/lisp_deinit"))
```

---

## 4. Rebuilding ChrysaLisp vs Host C++

- **Host C++ changes** (`src/host/main.cpp`, `src/host/net.cpp`, etc.):
  Rebuild the host binaries with host `make`:
  ```bash
  make
  ```

- **ChrysaLisp VP / Lisp / System image changes** (`service/net/link.vp`, `class/*`, `sys/*`):
  Never run host `make` to compile ChrysaLisp code. Rebuild the system boot image from within ChrysaLisp using:
  ```bash
  ./run_tui.sh -f -s tests/build/test_build.lisp
  ```
