# Global

```image
docs/reference/classes/Global.cwb
```

## Fmap

```code
(Global fnc_create fnc_destroy) -> global

the nodes that were asked for a task and could not start it. They are
not asked again: a node need not be one that runs what it is sent, see
docs/ai_digest/alien_nodes.md
and how many times the task of each node has been started again for
its time being up: each doubles how long the next is given, so a slow
node is given the time it needs, never started again for ever
```

### :close

```code
(. global :close) -> global

close tasks
```

### :refresh

```code
(. global :refresh [timeout]) -> :t | :nil

scan known nodes and update node map
merge local node to bypass eventual consistency UI lag. The
timeout is the patience to start with, doubled for each time the
task of that node has been started again for its time being up
```

### :refused

```code
(. global :refused) -> nodes

the nodes that could not start the task
```

### :reply

```code
(. global :reply msg) -> :nil | val

what a kernel answers when it is asked to start the task of a
node. If it did, the entry of that node, to keep the id in. If it
could not, the id is 0, the entry is destroyed, and that node is
not asked again
```

