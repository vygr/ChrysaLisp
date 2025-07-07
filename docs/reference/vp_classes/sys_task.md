# sys_task

## Lisp Bindings

### (task-count bias)

### (task-netid)

### (task-sleep usec)

## VP methods

### :callback -> sys/task/callback

```code
inputs
:r0 = user data address (ptr)
:r1 = callback address (ptr)
trashes
:r0-:r14
```

### :count -> sys/task/count

```code
inputs
:r0 = task count bias (int)
outputs
:r0 = new task count (int)
trashes
:r0-:r2
```

### :defer -> sys/task/defer

```code
inputs
:r0 = task control node to defer to (ptr)
trashes
none
info
restore task
```

### :dump -> sys/task/dump

```code
inputs
:rsp = task stack pointer (ptr)
trashes
none
```

### :mailbox -> sys/task/mailbox

```code
outputs
:r0-:r2 = current ID (net_id)
trashes
:r0-:r2
```

### :restore -> sys/task/restore

```code
trashes
:r0-:r14
info
restore next ready task
```

### :resume -> sys/task/resume

```code
inputs
:r0 = task control node to resume (ptr)
outputs
:r0 = task control node to resume (ptr)
trashes
:r1-:r2
```

### :set_priority -> sys/task/set_priority

```code
inputs
:r0 = priority (uint)
trashes
:r0-:r4
```

### :sleep -> sys/task/sleep

```code
inputs
:r0 = time delay in usec (ulong)
trashes
none
info
0 for yield
```

### :stacks -> sys/task/stacks

```code
outputs
:r0 = maximum task stack size (uint)
trashes
:r0-:r8
```

### :start -> sys/task/start

```code
inputs
:r0 = new task func pointer (ptr)
outputs
:r0 = new task control block (ptr)
:r1 = new task mailbox address (ptr)
:r2-:r4 = new task ID (net_id)
trashes
:r0-:r14
```

### :statics_init -> sys/task/statics_init

```code
info
init task statics
```

### :stop -> sys/task/stop

```code
info
stop current task, switch to next task
```

### :suspend -> sys/task/suspend

```code
trashes
none
info
suspend current task, switch to next task
```

### :task_callback -> class/obj/null

### :tcb -> sys/task/tcb

```code
outputs
:r0 = current task tcb (ptr)
trashes
:r0
```

### :timer -> sys/task/timer

```code
outputs
:r0 = current time (ulong)
trashes
:r0-:r14
info
resume tasks ready to run.
mail mailboxes on timouts.
```

