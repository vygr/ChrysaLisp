# Stage

```image
docs/reference/classes/Stage.cwb
```

```code
(Stage) -> stage
```

### :add

```code
(. stage :add actor) -> stage

put an actor at the front
```

### :add_back

```code
(. stage :add_back actor) -> stage
```

### :get_actors

```code
(. stage :get_actors) -> actors

the back one first
```

### :hit

```code
(. stage :hit x y [event]) -> :nil | actor

the frontmost actor that says a point is on it, and a contact that
lands there its own, Chris: "It just says I have this event
co-ordinate, is that you ?"
```

### :owner

```code
(. stage :owner id) -> :nil | actor

the actor a pointer that is down belongs to
```

### :pass

```code
(. stage :pass actor id events) -> :nil | actor

an actor that had a contact gives it to the first behind it that
takes it, as if it had landed there, and that one is given the
events of it so far, the first as it went down
```

### :pointers

```code
(. stage :pointers events) -> actors

a batch of events, no more than one for a pointer. Each goes to
the actor its pointer belongs to, an actor is given all of its
own at once. The actors that were given any
```

### :sub

```code
(. stage :sub actor) -> stage

take an actor off, and it has no pointers
```

