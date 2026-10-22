# Pipe

```image
docs/reference/classes/Pipe.cwb
```

```code
(Pipe cmdline [user_select]) -> pipe | :nil
```

### :abort

```code
(. pipe :abort) -> pipe

flag all streams as aborted, wake the in streams
```

### :close

```code
(. pipe :close) -> pipe

clear the stdin stream, which will send
stopping and stopped into the pipe
```

### :eof

```code
(. pipe :eof) -> pipe

the end of the pipe's stdin. The stdin stream is cleared, which
sends stopping and stopped into the pipe, and what the pipe has
yet to say can still be read
```

### :poll

```code
(. pipe :poll) -> :nil | :t
```

### :read

```code
(. pipe :read) -> :nil | :t | data

:nil if pipe closed
:t if user select

What a pipe says is its stdout and the stderr of each of its
commands. They are streams of their own and what is on them comes
in any order: a stderr can stop before the last of the stdout has
been read, and the stdout can stop before what a command said on
its stderr has come, the error it ended with. To close on the
first to stop lost the rest of the others, and to close when the
stdout did lost that error, now and then, on a machine with a lot
to do.

So the pipe is closed when its stdout has stopped and every stderr
has, however long that takes. A command says its stderr is
stopping as it ends, class/stdio/class.vp, as every system has
since the snapshot of 2026-10-22. It waited no more than 50ms for
a stderr once the stdout had stopped, for a system of before, and
a Pi with every test running lost an error so
```

### :write

```code
(. pipe :write string) -> pipe

first stream is :out to first pipe element
```

