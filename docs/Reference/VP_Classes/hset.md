# hset

## obj

### :clear -> class/hset/clear

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14
```

### :create -> class/hset/create

### :deinit -> class/hset/deinit

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14
```

### :each_callback -> class/obj/null

### :find -> class/hset/find

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = hset object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r14
```

### :flush -> class/hset/flush

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14
```

### :for_each -> class/hset/for_each

```code
inputs
:r0 = hset object (ptr)
:r1 = predicate function (ptr)
:r2 = predicate data (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r4...
callback predicate
inputs
:r0 = predicate data (ptr)
:r1 = element iterator (pptr)
:r2 = bucket list pointer (ptr)
trashes
...
```

### :get_bucket -> class/hset/get_bucket

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = hset object (ptr)
:r1 = bucket list object (ptr)
trashes
:r1-:r14
```

### :init -> class/hset/init

```code
inputs
:r0 = hset object (ptr)
:r1 = vtable (pptr)
:r2 = 0, else key compare callback (ptr)
:r3 = num buckets (uint)
outputs
:r0 = hset object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5
```

### :insert -> class/hset/insert

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = hset object (ptr)
:r1 = element iterator (pptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r14
```

### :key_callback -> class/obj/null

### :vtable -> class/hset/vtable

