# emap

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

### :copy -> class/emap/copy

```code
inputs
:r0 = emap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = emap object (ptr)
:r1 = emap copy object (ptr)
trashes
:r1-:r14
```

### :create -> class/emap/create

### :deinit -> class/emap/deinit

```code
inputs
:r0 = emap object (ptr)
outputs
:r0 = emap object (ptr)
trashes
:r1-:r14
```

### :each -> class/emap/each

```code
inputs
:r0 = emap object (ptr)
:r1 = predicate function (ptr)
:r2 = predicate data (ptr)
outputs
:r0 = emap object (ptr)
trashes
:r1-:r4...
callback predicate
inputs
:r0 = predicate data (ptr)
:r1 = element iterator (pptr)
trashes
...
```

### :find -> class/emap/find

```code
inputs
:r0 = emap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r14
```

### :get -> class/emap/get

```code
inputs
:r0 = emap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = 0 if not found, else value object (ptr)
trashes
:r1-:r14
```

### :init -> class/emap/init

```code
inputs
:r0 = emap object (ptr)
:r1 = vtable (pptr)
:r2 = num buckets (uint)
outputs
:r0 = emap object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7
```

### :insert -> class/emap/insert

```code
inputs
:r0 = emap object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r14
```

### :list -> class/emap/list

```code
inputs
:r0 = emap object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = list object (ptr)
trashes
:r1-:r14
```

### :resize -> class/emap/resize

```code
inputs
:r0 = emap object (ptr)
:r1 = num buckets (uint)
outputs
:r0 = emap object (ptr)
trashes
:r1-:r14
```

### :search -> class/emap/search

```code
inputs
:r0 = emap object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = 0, else iterator (pptr)
:r2 = bucket list (ptr)
trashes
:r1-:r14
```

### :set -> class/emap/set

```code
inputs
:r0 = emap object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = emap object (ptr)
:r1 = 0 if not found, else value object (ptr)
trashes
:r1-:r14
```

### :set_parent -> class/emap/set_parent

```code
inputs
:r0 = emap object (ptr)
:r1 = 0, else emap parent object (ptr)
outputs
:r0 = emap object (ptr)
trashes
:r1-:r14
```

### :type -> class/emap/type

### :vtable -> class/emap/vtable

