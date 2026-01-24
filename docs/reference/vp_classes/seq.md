# :seq

## :obj

## Lisp Bindings

### (cat seq ...) -> seq

### (each! lambda seqs [start end])

### (elem-get seq idx) -> elem

### (filter! lambda seq [out start end]) -> out | (...)

### (find elem seq [idx]) -> :nil | idx

### (first seq) -> :nil | elem

### (last seq) -> :nil | elem

### (length seq) -> num

### (map! lambda seqs [out start end]) -> out | (...)

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

### :ref_elem -> class/obj/null

### :rfind -> class/obj/null

### :slice -> class/obj/null

### :splice -> class/obj/null

### :vtable -> class/seq/vtable

