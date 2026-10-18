# Farm

```image
docs/reference/classes/Farm.cwb
```

## Fmap

```code
(Farm fnc_create fnc_destroy size) -> farm

the nodes that were asked for a task and could not start it. They are
not asked again while this farm lasts: a node need not be one that
runs what it is sent, see docs/ai_digest/alien_nodes.md
```

### :close

```code
(. farm :close) -> farm

close tasks
```

### :grow_to

```code
(. farm :grow_to want) -> farm

children started till there are want
```

### :grown

```code
(. farm :grown) -> farm

after a refresh, a farm stays the size it was made
```

### :nodes

```code
(. farm :nodes) -> nodes

the nodes a child may be started on, all those that have not refused
```

### :place

```code
(. farm :place key nodes) -> nodes

where the child of key is asked for, any of them
```

### :refresh

```code
(. farm :refresh [timeout]) -> :t | :nil

scan known nodes and update map, merge local node here as well
a child on a node that refused is not there to be found, it is
all the nodes there are that a child is looked for in
```

### :refused

```code
(. farm :refused) -> nodes

the nodes that could not start a child
```

### :reply

```code
(. farm :reply msg) -> :nil | val

what a kernel answers when it is asked to start a child. If it
did, the entry of that child, to keep the id in. If it could not,
the id is 0: that node is not asked again, and the child is asked
for at once of another, if there is one that has not refused
```

### :restart

```code
(. farm :restart key val) -> farm

restart task
```

