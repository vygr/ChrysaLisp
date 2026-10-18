# Local

```image
docs/reference/classes/Local.cwb
```

## Farm

```code
(Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local

a herd of worker tasks on the nodes of this machine, those that share
its file system. The herd is herd_init workers, and herd_growth more
for each node other than this one, and no more than herd_max. They are
started at once, on each node in turn.
```

### :grown

```code
(. local :grown) -> local

more nodes may have joined, the herd grown to what they call for
```

### :nodes

```code
(. local :nodes) -> nodes

the nodes of this machine
```

### :place

```code
(. local :place key nodes) -> nodes

each child on the next of the nodes in turn
```

### :reply

```code
(. local :reply msg) -> :nil | val

what a kernel answers when it is asked to start a worker: its
entry, to keep the id in, :nil if none was started. The nodes of
a herd are this machine's, there is none of them to leave
```

