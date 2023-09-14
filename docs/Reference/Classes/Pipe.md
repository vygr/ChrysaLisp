# Pipe

```code
(Pipe cmdline &optional user_select) -> pipe | :nil
```code

### :close

```code
(. pipe :close) -> pipe
```code

### :poll

```code
(. pipe :poll) -> :nil | :t
```code

### :read

```code
(. pipe :read) -> :nil | :t | data
```code

### :write

```code
(. pipe :write string) -> pipe
```code

