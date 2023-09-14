# Pipe

```code
(Pipe cmdline &optional user_select) -> pipe | :nil
```code

### :close

```code
(. pipe :close) -> pipe

clear the stdin stream, which will send stopping and stopped
```code

### :poll

```code
(. pipe :poll) -> :nil | :t
```code

### :read

```code
(. pipe :read) -> :nil | :t | data

:nil if pipe closed
:t if user select
```code

### :write

```code
(. pipe :write string) -> pipe
```code

