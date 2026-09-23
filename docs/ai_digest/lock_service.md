# Lock Service (@Lock)

The `@Lock` service is a distributed, high-performance hierarchical lock
manager providing shared-read and exclusive-write synchronization across a
ChrysaLisp network cluster.

## Overview

Distributed coordination in ChrysaLisp avoids complex consensus algorithms and
heavyweight, pointer-chasing trie allocations. Instead, the lock service
leverages core vector and sequence primitives (`split`, `every`, `pmap`, and
O(1) list operations) to deliver robust synchronization in under 100 lines
of script code.

The service provides:

*	**Hierarchical Path Locking:** Parent paths lock out child paths, and child
	paths lock out parent paths automatically.

*	**Shared-Read and Exclusive-Write Semantics:** Multiple readers can access
	the same path or overlapping subpaths concurrently, while writers receive
	strict mutual exclusion.

*	**Path-Wise Strict FIFO Queueing:** Requests are evaluated in the order they
	arrive without queue jumping, preventing writer and reader starvation while
	allowing independent paths to proceed without head-of-line blocking.

*	**Fault-Tolerant Expiration:** Autonomous reclamation of abandoned locks
	when tasks crash, nodes disconnect, or client requests time out.

*	**Scoped RAII Macros:** `with-lock`, `with-read-lock`, and `with-write-lock`
	guarantee deterministic lock release across normal execution without manual
	release boilerplate.

*	**Audit History Ring Buffer:** In-memory, bounded ring buffer of recent lock
	acquisitions and releases for inspection, debugging, and testing.

## RPC Protocol and Data Structures

The client interface is defined in `service/lock/app.inc`. Communication with the
service uses standard message passing through a registered mailbox name.

```file
"service/lock/app.inc" "(enums +lock_type" "+lock_history"
```

The protocol supports three operation types:

*	`+lock_type_claim` (0): Request shared or exclusive access to a path.

*	`+lock_type_release` (1): Relinquish an existing lock on a path.

*	`+lock_type_history` (2): Query recent lock activity from the audit buffer.

And two locking modes:

*	`+lock_mode_write` (0): Exclusive lock mode. Conflicts with any active write
	or read on an overlapping path.

*	`+lock_mode_read` (1): Shared lock mode. Conflicts only with active writes
	on an overlapping path, permitting concurrent readers.

### Service Discovery and Resilient Startup

Client tasks locate the lock service dynamically via the system mail registry:

```file
"service/lock/app.inc" "(defun lock-service" ""
```

```file
"service/lock/app.inc" "(defun ensure-lock-service" ""
```

*	`lock-service` queries `(mail-enquire "@Lock,")` to find the registered
	mailbox across the local or remote cluster nodes, parsing and returning its
	24-byte `netid`.

*	`ensure-lock-service` wraps `lock-service` with retry polling (up to 50
	iterations with a 10 ms sleep interval), providing a 500 ms grace window to
	absorb startup scheduling races when nodes or services initialize
	asynchronously.

### Client API

Client applications interact with `@Lock` via RPC helper functions:

```file
"service/lock/app.inc" "(defun lock-claim-rpc" ""
```

```file
"service/lock/app.inc" "(defun lock-release-rpc" ""
```

```file
"service/lock/app.inc" "(defun lock-history-rpc" ""
```

*	`lock-claim-rpc` transparently ensures the lock service is running via
	`ensure-lock-service`, allocates a disposable mailbox, sends a claim
	request, and waits for confirmation. It defaults to `+lock_mode_write` if the
	mode argument is omitted, ensuring safe mutual exclusion by default and
	retaining 100% backward compatibility with legacy call sites (such as `jit`
	and `files-classes-info`).

*	`lock-release-rpc` does not require a mode argument because a key can only
	ever reside in either the write list or the read list at any given moment.

*	`lock-history-rpc` requests recent lock and unlock events from `@Lock`,
	returning the audit log as a list of strings (or `:nil` if the service
	cannot be reached).

## Scoped Resource Acquisition (RAII Macros)

To eliminate leaked locks caused by missing release calls or unexpected flow of
control, `service/lock/app.inc` provides scoped locking macros:

```file
"service/lock/app.inc" "(defmacro with-lock" ""
```

```file
"service/lock/app.inc" "(defmacro with-read-lock" ""
```

```file
"service/lock/app.inc" "(defmacro with-write-lock" ""
```

### Syntax and Flexibility

The macros support both concise single-key forms and tuple configurations with
custom modes or timeouts:

```vdu
;; Simple single-key form (defaults to write mode and 10s timeout)
(with-lock "database/master"
	(update-records))

;; Tuple specification with custom mode and timeout
(with-lock ("database/users" +lock_mode_read (task-timeout 2))
	(query-users))

;; Explicit read lock (shared)
(with-read-lock "fonts/OpenSans.ctf"
	(load-font))

;; Explicit write lock (exclusive) with timeout
(with-write-lock ("output/build.bin" (task-timeout 5))
	(emit-binary))
```

### Macro Architecture

The macros leverage ChrysaLisp's hygienic macro facilities:

*	`list??` detects whether `spec` is a list tuple or a bare string key.

*	`gensym` generates unique temporary symbols (`k`, `r`) to prevent symbol
	capture or double evaluation of the key expression.

*	`static-qq` generates compile-time quasi-quoted templates without runtime
	quasiquote interpreter overhead.

*	The acquired lock is held strictly for the duration of `body`, released
	immediately upon normal completion via `lock-release-rpc`, and the final
	evaluated expression in `body` is returned.

### Real-World Case Study: Docs App File Handler

The ChrysaLisp documentation viewer (`apps/desktop/docs/handlers/file.inc`) is a
prime real-world example of `with-read-lock`. When rendering embedded `file`
code blocks in Markdown documents, it acquires a shared read lock on the target
file, streams the matching lines into a memory buffer, closes the stream, and
guarantees lock release before rendering the syntax-highlighted widget:

```file
"apps/desktop/docs/handlers/file.inc" "(with-read-lock file_path" "(setq stream :nil)))"
```

Notice how `(setq stream :nil)` is executed inside the `with-read-lock` body
before releasing the lock, adhering to the ChrysaLisp invariant that file
descriptors are closed before releasing file synchronization.

## Hierarchical Conflict Detection

Path relationships in `@Lock` are resolved using ChrysaLisp's native `split` and
`every` primitives.

```file
"service/lock/app_impl.lisp" "(defun conflict?" ""
```

Paths are tokenized with `(split key "/")`. In ChrysaLisp, `split` acts as a
token scanner that automatically skips leading, trailing, and consecutive
delimiters without allocating empty strings.

The conflict test `(every (const eql) key_path (pfind %0 :path))` exploits the
property that `every` evaluates up to the minimum length of both sequences:

*	**Exact Matches:** `("a" "b")` vs `("a" "b")` compares 2 elements and
	returns `:t`.

*	**Ancestor vs. Descendant:** `("a")` vs `("a" "b")` compares 1 element and
	returns `:t` (the parent lock blocks child operations).

*	**Descendant vs. Ancestor:** `("a" "b")` vs `("a")` compares 1 element and
	returns `:t` (the child lock blocks parent operations).

*	**Disjoint Paths:** `("a" "b")` vs `("a" "c")` fails at index 1 and returns
	`:nil` (unrelated branches proceed concurrently).

*	**Root / Cluster Lock:** An empty key `""` or `"/"` produces `'()`. Because
	the minimum length of an empty list with any path is zero, `every` vacuously
	returns `:t`, cleanly locking the entire hierarchy.

## Queueing and Scheduling Architecture

The service maintains four internal lists in `main`:

*	`lock_writes`: Currently granted exclusive write lock records.

*	`lock_reads`: Currently granted shared read lock records. Multiple readers
	on the exact same key are compacted into a single record where `:mode`
	stores the active reader count, keeping conflict scans O(unique paths).

*	`lock_pending`: Unfulfilled claim requests waiting in FIFO order.

*	`lock_history`: Bounded circular audit trail of recent lock operations.

### Path-Wise Strict FIFO Draining

The `merge-locks` function drains `lock_pending` from oldest to newest:

```file
"service/lock/app_impl.lisp" "(defun merge-locks" "new_pending)"
```

To eliminate starvation while preserving maximum concurrency:

1.	A candidate request checks for conflicts against active locks (`writes`, and
	`reads` if requesting a write).

2.	A candidate request also checks against a temporary `blocked` list containing
	all earlier pending requests in the current pass that could not yet be
	granted.

3.	If an earlier request on an overlapping path is blocked, all subsequent
	requests on that path are appended to `blocked` and deferred. This prevents
	a flood of incoming reads from starving an earlier pending write.

4.	Requests on independent, disjoint paths do not conflict with `blocked` and
	are granted immediately without head-of-line delays.

5.	When requests are submitted, the caller's node ID is merged with
	`(lisp-nodes)` to bypass potential cluster-wide eventual consistency lag
	when new nodes join and immediately issue lock claims.

## Fault Tolerance and Expiration

In a distributed environment, holding locks indefinitely across task crashes or
network partitions causes cluster-wide deadlocks. The lock service solves this
with a three-tier expiration strategy driven by a periodic 1-second timer tick:

```file
"service/lock/app_impl.lisp" "(defun purge-expired" "changed)"
```

### 1. Ephemeral Node Failure Detection

Every `reply_id` is a 24-byte `netid` tuple containing an 8-byte mailbox ID and a
16-byte `node_id`.

When a node reboots, the kernel generates a brand-new, random `node_id`. The old
`node_id` is purged from `(lisp-nodes)` across the cluster.

If `(find (task-nodeid (pfind rec :reply)) nodes)` fails, the node that claimed
the lock is dead. The service immediately revokes the lock and reclaims the
resource without waiting for a lease timeout.

### 2. Caller Timeout Pruning (Ghost Lock Prevention)

In `lock-claim-rpc`, the client creates a transient mailbox `mbox` and awaits
confirmation using `mail-read-timeout`. When that timeout expires, the RPC
exits and `mbox` is immediately deallocated by reference counting.

If a pending request waits in `lock_pending` longer than its client timeout
(`timeout`), the client has already abandoned the call. `merge-locks` discards
the expired request from `lock_pending` rather than granting a lock that will
never be released.

### 3. Active Lease Expiration (TTL)

If a task crashes, leaks, or hangs on a surviving node without releasing its
lock, `purge-expired` checks whether `(- now (pfind rec :time))` has exceeded
`+lock_default_lease` (60 seconds by default). Expired locks are revoked using
O(1) swap-and-pop removal, and `merge-locks` immediately grants waiting
waiters.

## Audit History and Diagnostics

The service records an audit log of lock grants and releases in a bounded
circular buffer:

```file
"service/lock/app_impl.lisp" "(defun log-lock-history" "+lock_max_history))))"
```

*	Each transition appends a formatted event string: `"<key> (<action> <mode>)"`,
	for example `"docs/ai_digest/lock_service.md (lock write)"` or
	`"fonts/OpenSans.ctf (unlock read)"`.

*	When the buffer exceeds `+lock_max_history` (128 entries), older events are
	evicted in-place via `(erase history 0 ...)`.

*	**TUI `locks` Command:** The CLI tool `cmd/locks.lisp` queries
	`(lock-history-rpc)` and prints recent lock transitions directly in the
	terminal.

*	**Automated Verification:** System unit tests (`tests/system/test_lock.lisp`)
	inspect `(lock-history-rpc)` to verify that file-modifying tools (`save`,
	`cat`, `ctf`, `files-depends`) acquire and release the correct lock modes
	and paths.

## Service Implementation

The complete main loop for `service/lock/app_impl.lisp`:

```file
"service/lock/app_impl.lisp" "(defun main ()" ""
```