# num

## obj

## Lisp Bindings

### (abs num)

### (+ num num ...)

### (logand [num] ...)

### (>>> num cnt)

### (/ num num ...)

### (= num num ...)

### (>= num num ...)

### (> num num ...)

### (num-intern num)

### (<= num num ...)

### (< num num ...)

### (max num num ...)

### (min num num ...)

### (% num num ...)

### (* num num ...)

### (n2f num)

### (n2i num)

### (n2r num)

### (/= num num ...)

### (neg num)

### (logior [num] ...)

### (random num)

### (<< num cnt)

### (>> num cnt)

### (sign num)

### (sqrt num)

### (- num num ...)

### (logxor [num] ...)

## VP methods

### :abs -> class/num/abs

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :add -> class/num/add

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :create -> class/num/create

### :div -> class/num/div

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0 if error, else result num object (ptr)
trashes
:r1-:r14
```

### :eq -> class/num/eq

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :ge -> class/num/ge

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :get_value -> class/num/get_value

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = value (long)
trashes
:r1
```

### :gt -> class/num/gt

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :hash -> class/num/hash

```code
inputs
:r0 = num (ptr)
outputs
:r0 = num (ptr)
:r1 = hash code (ulong)
trashes
:r1-:r14
```

### :init -> class/num/init

```code
inputs
:r0 = num object (ptr)
:r1 = vtable (pptr)
:r2 = initial value (long)
outputs
:r0 = num object (ptr)
:r1 = 0 if error, else ok
trashes
:r1
```

### :intern -> class/num/intern

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = interned num object (ptr)
trashes
:r0-:r14
```

### :le -> class/num/le

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :lt -> class/num/lt

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :max -> class/num/max

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :min -> class/num/min

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :mod -> class/num/mod

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0 if error, else result num object (ptr)
trashes
:r1-:r14
```

### :mul -> class/num/mul

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :ne -> class/num/ne

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :neg -> class/num/neg

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :print -> class/num/print

```code
inputs
:r0 = num object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = num object (ptr)
trashes
:r1-:r14
```

### :random -> class/num/random

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :set_value -> class/num/set_value

```code
inputs
:r0 = num object (ptr)
:r1 = value (long)
outputs
:r0 = num object (ptr)
:r1 = value (long)
trashes
none
```

### :sign -> class/num/sign

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :sqrt -> class/num/sqrt

```code
inputs
:r0 = num object (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :sub -> class/num/sub

```code
inputs
:r0 = num object (ptr)
:r1 = list of num objects (ptr)
outputs
:r0 = num object (ptr)
:r1 = result num object (ptr)
trashes
:r1-:r14
```

### :type -> class/num/type

### :vcreate -> class/num/create

### :vtable -> class/num/vtable

