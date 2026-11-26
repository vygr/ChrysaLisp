# :seq

```image
docs/reference/vp_classes/seq.cwb
```

## :obj

## Lisp Bindings

### (cat seq ...) -> seq

### (each! lambda seqs [start end])

### (elem-get seq idx) -> elem

### (filter! lambda seq [start end out]) -> out | (...)

### (find elem seq [idx]) -> :nil | idx

### (first seq) -> :nil | elem

### (last seq) -> :nil | elem

### (length seq) -> num

### (map! lambda seqs [start end out]) -> out | (...)

### (most seq) -> empty | seq

### (partition seq [cnt]) -> (seq ...)

### (!) -> idx

### (reduce! lambda seqs init [start end]) -> val

### (rest seq) -> empty | seq

### (rfind elem seq [idx]) -> :nil | idx

### (second seq) -> :nil | elem

### (slice seq start end) -> seq

### (some! lambda seqs [mode start end]) -> :nil | val

### (splice seq1 seq2 idxs) -> seq

### (third seq) -> :nil | elem

## VP methods

### :cat -> class/obj/null

### :find -> class/obj/null

### :get_length -> class/obj/null

### :get_raw -> class/seq/get_raw

```code
inputs
:r0 = seq object (ptr)
outputs
:r0 = seq object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes), 0 if none
trashes
:r1-:r3
info
a sequence as raw data, where it is, how many elements and how wide
each: a str and an array of numbers have it. One of objects, a list,
has none, an element size of 0, and this is that
```

### :ref_elem -> class/obj/null

### :rfind -> class/obj/null

### :slice -> class/obj/null

### :splice -> class/obj/null

### :type -> class/seq/type

```code
inputs
:r0 = seq object (ptr)
outputs
:r0 = seq object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vtable -> class/seq/vtable

