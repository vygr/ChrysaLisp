# Bindings

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
what the pointer must have, or :nil for any. buttons matches if
every one of them is held. what is a list of keys and values,
(:tool :pen :color 0xff000000 :width 4.0)
```

### :find

```code
(. bindings :find event) -> :nil | what
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

