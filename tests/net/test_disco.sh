#!/bin/bash
set -e

# ChrysaLisp network auto-discovery test runner
# Cleans up background processes on exit
cleanup() {
	./stop.sh > /dev/null 2>&1 || true
}
trap cleanup EXIT

# 1. Ensure clean slate
./stop.sh > /dev/null 2>&1 || true

# 2. Start server in background (node base 0, ports 0..9)
./run_tui.sh -s tests/net/srv_disco.lisp > tests/net/server_disco.log 2>&1 &
SRV_PID=$!

# 3. Wait for port 4567 to open
for i in {1..30}; do
	if lsof -i :4567 > /dev/null 2>&1; then
		break
	fi
	sleep 0.1
done

# 4. Run client in foreground (node base 10, ports 10..19)
./run_tui.sh -b 10 -s tests/net/cli_disco.lisp
