# :hmap

## :list

## Lisp Bindings

### (env-copy env num) -> env

### (def env sym val [sym val] ...) -> val

### (defq sym val [sym val] ...) -> val

### (def? sym [env]) -> :nil | val

### (env [num]) -> env

### (get sym [env]) -> :nil | val

### (tolist env) -> ((sym val) ...)

### (penv [env]) -> :nil | env

### (env-resize num [env]) -> env

### (set env sym val [sym val] ...) -> val

### (setq sym val [sym val] ...) -> val

### (undef env sym [sym] ...) -> env

## VP methods

### :cfind -> class/hmap/cfind

```code
inputs
:r0 = hmap object (ptr)
:r1 = key object (ptr)
:r2 = key callback (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r10
```

### :copy -> class/hmap/copy

```code
inputs
:r0 = hmap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
:r1 = hmap copy object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :create -> class/hmap/create

```code
inputs
:r0 = num buckets (uint)
outputs
:r0 = 0 if error, else hmap object (ptr)
trashes
:r0-:r5, :r14, :f0-:f15
```

### :deinit -> class/hmap/deinit

```code
inputs
:r0 = hmap object (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :each -> class/hmap/each

```code
inputs
:r0 = hmap object (ptr)
:r1 = predicate function (ptr)
:r2 = predicate data (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14, :f0-:f15
callback predicate
inputs
:r0 = predicate data (ptr)
:r1 = element iterator (pptr)
trashes
:r1-:r14, :f0-:f15
```

### :each_callback -> class/obj/null

### :eql -> class/obj/eql

```code
inputs
:r0 = obj object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = obj object (ptr)
:r1 = 0 if same, else not
trashes
:r1
```

### :get -> class/hmap/get

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0 if not found, else value object (ptr)
trashes
:r1-:r8
```

### :init -> class/hmap/init

```code
inputs
:r0 = hmap object (ptr)
:r1 = vtable (pptr)
:r2 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :f0-:f15
```

### :list -> class/hmap/list

```code
inputs
:r0 = hmap object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :pfind -> class/hmap/pfind

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r7
```

### :pinsert -> class/hmap/pinsert

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :resize -> class/hmap/resize

```code
inputs
:r0 = hmap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :search -> class/hmap/search

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0, else iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r8
```

### :set -> class/hmap/set

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0 if not found, else value object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :set_parent -> class/hmap/set_parent

```code
inputs
:r0 = hmap object (ptr)
:r1 = 0, else hmap parent object (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :type -> class/hmap/type

```code
inputs
:r0 = hmap object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vtable -> class/hmap/vtable

