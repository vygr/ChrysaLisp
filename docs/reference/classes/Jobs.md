# Jobs

```code
(Jobs path task_mbox reply_mbox [size]) -> jobs

children of the task at path, a node's worth of them if no size is
given, on any of the nodes. A size that is a list, (herd_max
[herd_init herd_growth]), is a herd on the nodes of this machine,
those that share its file system, as (Local) has it. They are
started at once.
```

### :add

```code
(. jobs :add jobs) -> jobs

more jobs for the queue, the last of them is the first to go
```

### :answered

```code
(. jobs :answered msg) -> :nil | num

a job is done, msg is what came to the reply mailbox. How many
are still not answered is the result. :nil if the answer is
from a child that has since been started again, its job went
back on the queue and the answer is to be left alone.
```

### :clear

```code
(. jobs :clear) -> jobs

the queue is emptied, the jobs the children have now are left
to finish and are all that is out
```

### :close

```code
(. jobs :close) -> jobs

every child is told to go
```

### :failed

```code
(. jobs :failed msg) -> :nil | num

as (:answered), for a job that went wrong. It is not put back
on the queue, and its child is started again.
```

### :job

```code
(. jobs :job msg) -> :nil | job

the job an answer is to, msg is what came to the reply mailbox,
asked before it is passed to (:answered). :nil if the answer
is from a child that has since been started again.
```

### :launched

```code
(. jobs :launched msg) -> jobs

a child has started, msg is what came to the task mailbox
```

### :out

```code
(. jobs :out) -> num

how many jobs are not yet answered
```

### :refresh

```code
(. jobs :refresh [timeout]) -> :t | :nil

start again any child that has gone, or has had its job for
longer than the timeout
```

### :restart

```code
(. jobs :restart) -> jobs

every child is started again, and the queue is emptied. A child
with nothing to do for a while has gone, and one part way
through a job that is no longer wanted is told to stop.
```

### :size

```code
(. jobs :size) -> num

how many children there are
```

