# :hset

## :obj

## VP methods

### :bucket -> class/hset/bucket

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = hset object (ptr)
:r1 = bucket list object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :clear -> class/hset/clear

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :create -> class/hset/create

### :deinit -> class/hset/deinit

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :each -> class/hset/each

```code
inputs
:r0 = hset object (ptr)
:r1 = predicate function (ptr)
:r2 = predicate data (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14, :f0-:f15
callback predicate
inputs
:r0 = predicate data (ptr)
:r1 = element iterator (pptr)
:r2 = bucket list pointer (ptr)
trashes
:r0-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
```

### :flush -> class/hset/flush

```code
inputs
:r0 = hset object (ptr)
outputs
:r0 = hset object (ptr)
trashes
:r1-:r14, :f0-:f15
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
:r1-:r5, :f0-:f15
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
:r1-:r14, :f0-:f15
```

### :key_callback -> class/obj/null

### :vtable -> class/hset/vtable

