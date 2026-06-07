# :list

## :array

## Lisp Bindings

### (copy form) -> 'form

### (list [elem ...]) -> list

### (lmatch? list list) -> :nil | :t

### (merge dlist slist) -> dlist

### (pivot lambda list start end)

## VP methods

### :append -> class/list/append

```code
inputs
:r0 = list object (ptr)
:r1 = source list object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = list object (ptr)
trashes
:r1-:r9, :f0-:f15
```

### :cat -> class/list/cat

```code
inputs
:r0 = list object (ptr)
:r1 = list of list objects (ptr)
outputs
:r0 = new list object (ptr)
trashes
:r0-:r9, :r14, :f0-:f15
```

### :clear -> class/list/clear

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :collect -> class/list/collect

```code
inputs
:r0 = list object (ptr)
:r1 = list of seq objects (ptr)
:r2 = element index (uint)
outputs
:r0 = list object (ptr)
trashes
:r1-:r6, :r8-:r11, :r14, :f0-:f15
```

### :copy -> class/list/copy

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
:r1 = copy list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :create -> class/list/create

### :deinit -> class/list/deinit

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :erase -> class/list/erase

```code
inputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r14, :f0-:f15
```

### :erase2 -> class/list/erase2

```code
inputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r14, :f0-:f15
```

### :find -> class/list/find

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = start index (uint)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = -1, else index (int)
trashes
:r2-:r9, :r14
```

### :min_length -> class/list/min_length

```code
inputs
:r0 = list of seq objects (ptr)
outputs
:r0 = list of seq objects (ptr)
:r1 = +max_long, or minimum length (uint)
trashes
:r1-:r5, :r14
```

### :print -> class/list/print

```code
inputs
:r0 = list object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :ref_all -> class/list/ref_all

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r4
```

### :ref_back -> class/list/ref_back

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
trashes
:r1-:r2
```

### :ref_elem -> class/list/ref_elem

```code
inputs
:r0 = list object (ptr)
:r1 = element index (uint)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
trashes
:r1-:r2
```

### :rfind -> class/list/rfind

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = start index (uint)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = 0, else index (int)
trashes
:r2-:r9, :r14
```

### :set_elem -> class/list/set_elem

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :slice -> class/list/slice

```code
inputs
:r0 = list object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice list object (ptr)
trashes
:r0-:r8, :r14, :f0-:f15
```

### :splice -> class/list/splice

```code
inputs
:r0 = src1 list object (ptr)
:r1 = src2 list object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = splice list object (ptr)
trashes
:r0-:r11, :r14, :f0-:f15
```

### :vcreate -> class/list/create

### :vtable -> class/list/vtable

