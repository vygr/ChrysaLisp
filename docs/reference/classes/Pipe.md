# Pipe

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

clear the stdin stream, which will send stopping and stopped
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
```

### :write

```code
(. pipe :write string) -> pipe
```

