# Local

```image
docs/reference/classes/Local.cwb
```

## Fmap

```code
(Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local

a herd of worker tasks on the nodes of this machine, those that share
its file system. The herd is herd_init workers, and herd_growth more
for each node other than this one, and no more than herd_max. They are
started at once, on each node in turn.
```

### :close

```code
(. local :close) -> local

close tasks
```

### :refresh

```code
(. local :refresh [timeout]) -> :t | :nil

scan known nodes and update map
```

### :reply

```code
(. local :reply msg) -> :nil | val

what a kernel answers when it is asked to start a worker: its
entry, to keep the id in, :nil if none was started. The nodes of
a herd are this machine's, there is none of them to leave
```

### :restart

```code
(. local :restart key val) -> local

restart task
```

