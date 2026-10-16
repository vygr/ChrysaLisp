# Bindings

```image
docs/reference/classes/Bindings.cwb
```

```code
(Bindings) -> bindings

rules, each what it matches and what a pointer it matches is to be.
The first rule that matches is the one, so the particular go before
the general
```

### :bind

```code
(. bindings :bind kind id buttons what) -> bindings

a rule, put before those there are. kind, id and buttons are each
what the pointer must have, or :nil for any. id is a device, what
(ptr-device) gives, and matches every contact of it: what a device
is bound to, its colour and tool, is whoever has that device's,
and each finger of a panel is the same hand. buttons matches if
every one of them is held. what is a list of keys and values,
(:tool :pen :color 0xff000000 :width 4.0)
```

### :devices

```code
(. bindings :devices [slot]) -> ((color what slot) ...)

each device, with the buttons it pressed with, that has been bound
to something of its own, the colour of its dot, what it is bound
to and its place. slot, (slot kind id buttons) -> :nil | place,
gives each its place, and its colour by the place mod the colours,
:nil not shown; else the first bound is the first. The menus and
the toolbars show the same dots. Chris: "on the float menus each
device showed a little dot (coloured per device, of that board)
to show what that device had selected as its modes"
```

### :find

```code
(. bindings :find event) -> :nil | what
```

### :set

```code
(. bindings :set kind id buttons key value) -> bindings

one thing of what that device, with those buttons, is to be, as a
binder sets it, a toolbar or a menu pressed by it. The rule that
names just that is made, if there is none, from all such a pointer
is now, so the rest of what it is stays as it was. buttons 0 would
match every press of the device, before the rules of its buttons,
so it is the left, as a pointer that came up is
```

### :swap

```code
(. bindings :swap key from to) -> bindings

every rule that has key as one of from has it as that of to, all at
once: the pens of the Whiteboard given other inks, each device that
held one holds it
```

### :unbind

```code
(. bindings :unbind kind id buttons) -> bindings

the rules that match just that are taken out
```

### :value

```code
(. bindings :value event key [default]) -> value

one thing of what a pointer is to be
```

