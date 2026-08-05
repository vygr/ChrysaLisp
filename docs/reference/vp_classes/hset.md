# :hset

## :list

## VP methods

### :bucket -> class/hset/bucket

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = hset object (ptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r6
```

### :cfind -> class/hset/cfind

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
:r2 = key callback (ptr)
outputs
:r0 = hset object (ptr)
:r1 = 0, else found iterator (pptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r12
```

### :cinsert -> class/hset/cinsert

```code
inputs
:r0 = hset object (ptr)
:r1 = key object (ptr)
:r2 = key callback (ptr)
outputs
:r0 = hset object (ptr)
:r1 = element iterator (pptr)
:r2 = bucket list object (ptr)
trashes
:r1-:r13, :f0-:f15
```

### :create -> class/hset/create

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
:r1-:r14, :f0-:f15
```

### :each_callback -> class/obj/null

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
:r2 = num buckets (uint)
outputs
:r0 = hset object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :f0-:f15
```

### :type -> class/hset/type

### :vtable -> class/hset/vtable

