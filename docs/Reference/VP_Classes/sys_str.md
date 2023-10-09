# sys_str

## VP methods

### :compare -> sys/str/compare

```code
inputs
:r0 = c string1 (pubyte)
:r1 = c string2 (pubyte)
outputs
:r0 = 0 if same, else -, +
trashes
:r0-:r3
```

### :copy -> sys/str/copy

```code
inputs
:r0 = c string (pubyte)
:r1 = c string copy (pubyte)
outputs
:r0 = c string end (pubyte)
:r1 = c string copy end (pubyte)
trashes
:r2
```

### :from_long -> sys/str/from_long

```code
inputs
:r0 = number (ulong)
:r1 = c string buffer (pubyte)
:r2 = base (ulong)
outputs
:r0 = c string buffer end (pubyte)
trashes
:r0-:r4
```

### :length -> sys/str/length

```code
inputs
:r0 = c string (pubyte)
outputs
:r0 = c string (pubyte)
:r1 = c string len (bytes)
trashes
:r1-:r2
```

### :read_utf8 -> sys/str/read_utf8

```code
inputs
:r0 = utf8 data pointer (pubyte)
outputs
:r0 = utf8 data pointer (pubyte)
:r1 = utf8 char (uint)
trashes
:r0-:r2
```

### :to_long -> sys/str/to_long

```code
inputs
:r0 = c string (pubyte)
:r1 = base (ulong)
outputs
:r3 = num | fixed (ulong)
:r4 = fixed point position (uint)
trashes
:r0-:r4
```

### :to_num -> sys/str/to_num

```code
inputs
:r0 = c string (pubyte)
outputs
:r3 = num | fixed (ulong)
:r4 = fixed point position (uint)
trashes
:r0-:r5
```

