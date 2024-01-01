# hmap

## hset

## Lisp Bindings

### (def env sym val [sym val] ...)

### (defq sym val [sym val] ...)

### (def? sym [env])

### (env [num])

### (get sym [env])

### (tolist env)

### (penv [env])

### (env-resize num [env])

### (set env sym val [sym val] ...)

### (setq sym val [sym val] ...)

### (undef env sym [sym] ...)

## VP methods

### :copy -> class/hmap/copy

```code
inputs
:r0 = hmap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
:r1 = hmap copy object (ptr)
trashes
:r1-:r14
```

### :create -> class/hmap/create

### :deinit -> class/hmap/deinit

```code
inputs
:r0 = hmap object (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14
```

### :find -> class/hmap/find

```code
inputs
:r0 = hmap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r14
```

### :for_each -> class/hmap/for_each

```code
inputs
:r0 = hmap object (ptr)
:r1 = predicate function (ptr)
:r2 = predicate data (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r4...
callback predicate
inputs
:r0 = predicate data (ptr)
:r1 = element iterator (pptr)
trashes
...
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
:r1-:r14
```

### :init -> class/hmap/init

```code
inputs
:r0 = hmap object (ptr)
:r1 = vtable (pptr)
:r2 = 0, else key compare callback (ptr)
:r3 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7
```

### :insert -> class/hmap/insert

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
:r1-:r14
```

### :list -> class/hmap/list

```code
inputs
:r0 = hmap object (ptr)
outputs
:r0 = hmap object (ptr)
:r1 = list object (ptr)
trashes
:r1-:r14
```

### :resize -> class/hmap/resize

```code
inputs
:r0 = hmap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14
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
:r1-:r14
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
:r1-:r14
```

### :set_parent -> class/hmap/set_parent

```code
inputs
:r0 = hmap object (ptr)
:r1 = 0, else hmap parent object (ptr)
outputs
:r0 = hmap object (ptr)
trashes
:r1-:r14
```

### :type -> class/hmap/type

### :vtable -> class/hmap/vtable

