# MAME Threading Model Decision

**Date:** 2025-11-18
**Status:** DECIDED - Hybrid Approach (Phase 1: Single-threaded, Phase 2: Stub threads)
**Author:** Claude (AI Assistant)

## Executive Summary

For the MAME port to ChrysaLisp, we will use a **phased threading approach**:

1. **Phase 1 (MVP):** Single-threaded MAME with threading disabled
2. **Phase 2 (Performance):** Stub threading with cooperative yield points
3. **Phase 3 (Future):** True threading via ChrysaLisp tasks (if needed)

## Problem Statement

MAME uses threading extensively for:
- **Audio generation** - Separate thread for audio samples
- **Parallel device emulation** - Multiple emulated chips running concurrently
- **Background loading** - Asset loading during gameplay

ChrysaLisp uses **cooperative multitasking** (tasks with explicit yielding) rather than preemptive threading, creating a mismatch.

## Options Evaluated

### Option A: Single-Threaded MAME ⭐ **CHOSEN FOR PHASE 1**

**Implementation:**
```bash
# Build MAME with threading disabled
make NOTHREADS=1 SUBTARGET=chrysalisp
```

**Pros:**
- ✅ Simplest implementation - zero threading code needed
- ✅ No synchronization bugs
- ✅ Fast to implement (just a build flag)
- ✅ Proven - many embedded MAME ports use this
- ✅ Works immediately with ChrysaLisp cooperative model

**Cons:**
- ❌ Potentially slower performance
- ❌ Audio may stutter on slower systems
- ❌ Can't take advantage of multi-core CPUs

**Performance Impact:**
- Modern games (2000+): Minimal (most are single-threaded anyway)
- Classic arcade (1970s-1990s): None (simple enough to run single-threaded)
- Audio: Acceptable (48kHz audio @ 60fps = only 800 samples per frame)

**Verdict:** Perfect for MVP. Most classic arcade games will run fine.

---

### Option B: Stub Threading (pthread compatibility layer) ⭐ **CHOSEN FOR PHASE 2**

**Implementation:**
```cpp
// Provide minimal pthread API that maps to single-threaded execution
pthread_t pthread_create(...) {
    // Don't actually create thread, just record function
    return fake_thread_handle;
}

void pthread_join(...) {
    // Execute the "thread" function immediately
    execute_pending_thread_function();
}

pthread_mutex_t pthread_mutex_init(...) {
    // No-op (single-threaded = no contention)
    return fake_mutex;
}
```

**Pros:**
- ✅ Source compatibility with MAME (no code changes needed)
- ✅ Still runs single-threaded (safe with ChrysaLisp)
- ✅ Can add cooperative yields at strategic points
- ✅ Easy to debug

**Cons:**
- ❌ Not true parallelism
- ❌ Requires implementing full pthread API surface
- ❌ May still have performance issues

**Verdict:** Good intermediate step. Allows MAME code to compile unmodified while we test.

---

### Option C: Real Threads with ChrysaLisp Tasks

**Implementation:**
```cpp
pthread_t pthread_create(void* (*start_routine)(void*), void* arg) {
    // Create actual ChrysaLisp task
    // Use (task-spawn) from Lisp layer
    return chrysalisp_task_spawn(start_routine, arg);
}

pthread_mutex_lock(pthread_mutex_t* mutex) {
    // Use mailbox-based locking
    chrysalisp_mailbox_lock(mutex->mailbox);
}
```

**Pros:**
- ✅ True parallelism on multi-core systems
- ✅ Best performance for complex games
- ✅ Integrates with ChrysaLisp distributed system

**Cons:**
- ❌ Complex implementation
- ❌ Need to add cooperative yield points in MAME
- ❌ Synchronization complexity with mailboxes
- ❌ May require modifying MAME source

**Verdict:** Deferred to future. Not needed for classic arcade games.

---

## Decision

### Phase 1 (Current - MVP): Single-Threaded

**Timeline:** Immediate
**Goal:** Get MAME running with Pac-Man or similar game

**Implementation:**
1. Build MAME with `NOTHREADS=1`
2. Test with simple games
3. Measure performance
4. If acceptable, ship MVP

**Success Criteria:**
- Pac-Man runs at 60 FPS
- Audio is smooth (no stuttering)
- Input is responsive

---

### Phase 2 (Next - Compatibility): Stub Threading

**Timeline:** After Phase 1 complete
**Goal:** Support more complex games that expect threading

**Implementation:**
1. Create `apps/mame/src/adapters/mame_threading_stub.cpp`
2. Implement minimal pthread API:
   - `pthread_create` - Queue function for later execution
   - `pthread_join` - Execute queued function
   - `pthread_mutex_*` - No-ops (single-threaded)
   - `pthread_cond_*` - No-ops
3. Add cooperative yield points:
   ```cpp
   void pthread_yield() {
       mame_thread_yield(); // Calls ChrysaLisp yield
   }
   ```
4. Inject `pthread_yield()` calls in MAME's main loop

**Success Criteria:**
- More complex games run (CPS2, Neo Geo, etc.)
- Still single-threaded but yields to ChrysaLisp
- No crashes from threading assumptions

---

### Phase 3 (Future - Performance): True ChrysaLisp Tasks

**Timeline:** TBD (based on performance needs)
**Goal:** Maximum performance for demanding games

**Implementation:**
1. Extend ChrysaLisp PII with task creation API
2. Map pthread API to ChrysaLisp tasks
3. Implement mailbox-based synchronization
4. Profile and optimize

**Success Criteria:**
- Utilizes multiple CPU cores
- Complex 3D games run smoothly
- Scales across distributed ChrysaLisp nodes

**Note:** May not be needed - classic arcade games are not demanding.

---

## Implementation Plan

### Immediate Actions (Phase 1)

1. **Update Makefile** to pass `NOTHREADS=1` to MAME build
2. **Document limitation** in README
3. **Test with target games**:
   - Pac-Man
   - Galaga
   - Donkey Kong
   - Space Invaders
4. **Benchmark performance** on target platforms

### Short-term (Phase 2, if needed)

1. **Create stub pthread library**
2. **Link into MAME build**
3. **Add yield points**
4. **Test with more games**:
   - Street Fighter II
   - Metal Slug
   - KOF series

### Long-term (Phase 3, if needed)

1. **Extend ChrysaLisp PII**
2. **Implement task-based threading**
3. **Profile and optimize**

---

## Testing Plan

### Phase 1 Testing

Run these games single-threaded and verify:

| Game | Expected FPS | Audio Quality | Input Response |
|------|-------------|---------------|----------------|
| Pac-Man | 60 | Perfect | Instant |
| Galaga | 60 | Perfect | Instant |
| Donkey Kong | 60 | Perfect | Instant |
| Space Invaders | 60 | Perfect | Instant |

Target platforms:
- x86_64 Linux (primary)
- ARM64 Linux (Raspberry Pi)
- macOS (x86_64 and ARM64)

### Acceptance Criteria

Phase 1 is successful if:
- ✅ All test games run at full speed (60 FPS)
- ✅ Audio has no stuttering or dropouts
- ✅ Input latency < 16ms (1 frame)
- ✅ Memory usage < 100MB
- ✅ Build time < 5 minutes

If any criterion fails, proceed to Phase 2.

---

## Rationale

**Why single-threaded first?**

1. **Simplicity** - Get something working quickly
2. **Classic arcade games** - Don't need threading
3. **Testing** - Easier to debug without concurrency
4. **Benchmarking** - Establishes baseline performance
5. **ChrysaLisp philosophy** - Cooperative multitasking aligns better

**Why not jump to full threading?**

1. **Complexity** - Would delay MVP by weeks
2. **Unknown benefit** - May not be needed
3. **Risk** - Threading bugs are hard to debug
4. **Scope creep** - Focus on working emulator first

**When to reconsider?**

1. Performance issues with complex games
2. User demand for multi-core support
3. Port to higher-end platforms (desktop vs embedded)

---

## References

- MAME threading documentation: `docs/threading.md` (in MAME source)
- ChrysaLisp task system: `docs/ai_digest/fault_tolerant.md`
- Pthread API reference: https://man7.org/linux/man-pages/man7/pthreads.7.html

---

## Appendix: Code Examples

### Phase 1: Build Configuration

```makefile
# In apps/mame/Makefile
MAME_FLAGS := NOTHREADS=1 SUBTARGET=chrysalisp SOURCES=src/mame/drivers/pacman.cpp
```

### Phase 2: Stub Threading Example

```cpp
// apps/mame/src/adapters/mame_threading_stub.cpp
#include <pthread.h>

typedef struct {
    void* (*start_routine)(void*);
    void* arg;
    void* result;
    bool executed;
} stub_thread_t;

static stub_thread_t* pending_threads[64];
static int num_pending = 0;

int pthread_create(pthread_t* thread, const pthread_attr_t* attr,
                   void* (*start_routine)(void*), void* arg) {
    stub_thread_t* stub = malloc(sizeof(stub_thread_t));
    stub->start_routine = start_routine;
    stub->arg = arg;
    stub->executed = false;

    pending_threads[num_pending++] = stub;
    *thread = (pthread_t)stub;
    return 0;
}

int pthread_join(pthread_t thread, void** retval) {
    stub_thread_t* stub = (stub_thread_t*)thread;
    if (!stub->executed) {
        stub->result = stub->start_routine(stub->arg);
        stub->executed = true;
    }
    if (retval) *retval = stub->result;
    free(stub);
    return 0;
}
```

---

**Decision Approved:** November 18, 2025
**Next Review:** After Phase 1 completion or 2 weeks, whichever comes first
