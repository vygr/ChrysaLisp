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

*   **Hierarchical Path Locking:** Parent paths lock out child paths, and child
    paths lock out parent paths automatically.

*   **Shared-Read and Exclusive-Write Semantics:** Multiple readers can access
    the same path or overlapping subpaths concurrently, while writers receive
    strict mutual exclusion.

*   **Path-Wise Strict FIFO Queueing:** Requests are evaluated in the order they
    arrive without queue jumping, preventing writer and reader starvation while
    allowing independent paths to proceed without head-of-line blocking.

*   **Fault-Tolerant Expiration:** Autonomous reclamation of abandoned locks
    when tasks crash, nodes disconnect, or client requests time out.

## RPC Protocol and Data Structures

The client interface is defined in `service/lock/app.inc`. Communication with the
service uses standard message passing through a registered mailbox name.

```file
"service/lock/app.inc" "(enums +lock_type" "+lock_release_size)"
```

The protocol supports two operation types (`+lock_type_claim` and
`+lock_type_release`) and two locking modes:

*   `+lock_mode_write` (0): Exclusive lock mode. Conflicts with any active write
    or read on an overlapping path.

*   `+lock_mode_read` (1): Shared lock mode. Conflicts only with active writes
    on an overlapping path, permitting concurrent readers.

### Client API

Client applications interact with `@Lock` via two primary RPC functions:

```file
"service/lock/app.inc" "(defun lock-claim-rpc" ""
```

```file
"service/lock/app.inc" "(defun lock-release-rpc" ""
```

*   `lock-claim-rpc` defaults to `+lock_mode_write` if the mode argument is
    omitted, ensuring safe mutual exclusion by default and retaining 100%
    backward compatibility with legacy call sites (such as `jit` and
    `files-classes-info`).

*   `lock-release-rpc` does not require a mode argument because a key can only
    ever reside in either the write list or the read list at any given moment.

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

*   **Exact Matches:** `("a" "b")` vs `("a" "b")` compares 2 elements and
    returns `:t`.

*   **Ancestor vs. Descendant:** `("a")` vs `("a" "b")` compares 1 element and
    returns `:t` (the parent lock blocks child operations).

*   **Descendant vs. Ancestor:** `("a" "b")` vs `("a")` compares 1 element and
    returns `:t` (the child lock blocks parent operations).

*   **Disjoint Paths:** `("a" "b")` vs `("a" "c")` fails at index 1 and returns
    `:nil` (unrelated branches proceed concurrently).

*   **Root / Cluster Lock:** An empty key `""` or `"/"` produces `'()`. Because
    the minimum length of an empty list with any path is zero, `every` vacuously
    returns `:t`, cleanly locking the entire hierarchy.

## Queueing and Scheduling Architecture

The service maintains three internal lists in `main`:

*   `lock_writes`: Currently granted exclusive write lock records.

*   `lock_reads`: Currently granted shared read lock records. Multiple readers
    on the exact same key are compacted into a single record where `:mode`
    stores the active reader count, keeping conflict scans O(unique paths).

*   `lock_pending`: Unfulfilled claim requests waiting in FIFO order.

### Path-Wise Strict FIFO Draining

The `merge-locks` function drains `lock_pending` from oldest to newest:

```file
"service/lock/app_impl.lisp" "(defun merge-locks" "new_pending)"
```

To eliminate starvation while preserving maximum concurrency:

1.  A candidate request checks for conflicts against active locks (`writes`, and
    `reads` if requesting a write).

2.  A candidate request also checks against a temporary `blocked` list containing
    all earlier pending requests in the current pass that could not yet be
    granted.

3.  If an earlier request on an overlapping path is blocked, all subsequent
    requests on that path are appended to `blocked` and deferred. This prevents
    a flood of incoming reads from starving an earlier pending write.

4.  Requests on independent, disjoint paths do not conflict with `blocked` and
    are granted immediately without head-of-line delays.

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

## Service Implementation

The complete main loop for `service/lock/app_impl.lisp`:

```file
"service/lock/app_impl.lisp" "(defun main ()" ""
```