/*
 * MAME Threading Adapter Implementation
 *
 * Provides a stub pthread-compatible API for MAME.
 * Phase 1: All operations are no-ops or immediate execution (single-threaded)
 * Phase 2: Can be extended to use ChrysaLisp tasks
 */

#include "../../include/mame_pii_adapter.h"
#include <stdlib.h>
#include <string.h>

/*
 * Thread implementation
 */

// Internal thread structure
typedef struct mame_thread_internal {
    mame_thread_func_t func;
    void* param;
    void* result;
    bool completed;
} mame_thread_internal_t;

mame_thread_t mame_thread_create(mame_thread_func_t func, void* param)
{
    if (!func) {
        mame_log(MAME_LOG_ERROR, "Thread function is NULL");
        return nullptr;
    }

    // Allocate thread structure
    mame_thread_internal_t* thread = (mame_thread_internal_t*)
        malloc(sizeof(mame_thread_internal_t));

    if (!thread) {
        mame_log(MAME_LOG_ERROR, "Failed to allocate thread structure");
        return nullptr;
    }

    thread->func = func;
    thread->param = param;
    thread->result = nullptr;
    thread->completed = false;

    // Note: In Phase 1 (stub mode), we don't actually create a thread
    // The function will be executed when pthread_join is called
    // In Phase 2, this would create a ChrysaLisp task

    mame_log(MAME_LOG_DEBUG, "Thread created (stub mode)");
    return (mame_thread_t)thread;
}

void mame_thread_join(mame_thread_t thread)
{
    if (!thread) {
        return;
    }

    mame_thread_internal_t* t = (mame_thread_internal_t*)thread;

    // Execute the thread function if not already done
    if (!t->completed) {
        mame_log(MAME_LOG_DEBUG, "Executing thread function");
        t->func(t->param);
        t->completed = true;
    }

    // Clean up
    free(t);
}

void mame_thread_yield(void)
{
    // In single-threaded mode, this is a no-op
    // In Phase 2, this would call ChrysaLisp's task-sleep or yield function

    // For now, just sleep briefly to avoid busy-waiting
    mame_time_sleep(1000); // 1ms
}

/*
 * Mutex implementation
 */

// Internal mutex structure
typedef struct mame_mutex_internal {
    int locked;  // 0 = unlocked, 1 = locked
    // In a real implementation, this would contain a mailbox ID or similar
} mame_mutex_internal_t;

mame_mutex_t mame_mutex_create(void)
{
    mame_mutex_internal_t* mutex = (mame_mutex_internal_t*)
        malloc(sizeof(mame_mutex_internal_t));

    if (!mutex) {
        mame_log(MAME_LOG_ERROR, "Failed to allocate mutex");
        return nullptr;
    }

    mutex->locked = 0;

    mame_log(MAME_LOG_DEBUG, "Mutex created (stub mode)");
    return (mame_mutex_t)mutex;
}

void mame_mutex_destroy(mame_mutex_t mutex)
{
    if (!mutex) {
        return;
    }

    free((mame_mutex_internal_t*)mutex);
    mame_log(MAME_LOG_DEBUG, "Mutex destroyed");
}

void mame_mutex_lock(mame_mutex_t mutex)
{
    if (!mutex) {
        return;
    }

    mame_mutex_internal_t* m = (mame_mutex_internal_t*)mutex;

    // In single-threaded mode, this is just a flag
    // In real implementation, this would use mailbox-based locking
    if (m->locked) {
        mame_log(MAME_LOG_WARNING, "Mutex already locked (deadlock in single-threaded mode!)");
    }

    m->locked = 1;
}

void mame_mutex_unlock(mame_mutex_t mutex)
{
    if (!mutex) {
        return;
    }

    mame_mutex_internal_t* m = (mame_mutex_internal_t*)mutex;

    if (!m->locked) {
        mame_log(MAME_LOG_WARNING, "Mutex unlock called on unlocked mutex");
    }

    m->locked = 0;
}
