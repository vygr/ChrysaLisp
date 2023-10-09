# sys_task

### :callback -> sys/task/callback

```code
inputs
:r0 = user data address (ptr)
:r1 = callback address (ptr)
trashes
:r0-:r14
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

### :lisp_mailbox -> sys/task/lisp_mailbox

### (task-netid)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_sleep -> sys/task/lisp_sleep

### (task-sleep usec)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_timeslice -> sys/task/lisp_timeslice

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
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

