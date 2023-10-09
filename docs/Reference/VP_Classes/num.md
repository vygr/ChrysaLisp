# num

## obj

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

### :lisp_abs -> class/num/lisp_abs

### (abs num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_add -> class/num/lisp_add

### (+ num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_and -> class/num/lisp_and

### (logand [num] ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_asr -> class/num/lisp_asr

### (>>> num cnt)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_div -> class/num/lisp_div

### (/ num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_eq -> class/num/lisp_eq

### (= num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_ge -> class/num/lisp_ge

### (>= num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_gt -> class/num/lisp_gt

### (> num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_intern -> class/num/lisp_intern

### (num-intern num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_le -> class/num/lisp_le

### (<= num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_lt -> class/num/lisp_lt

### (< num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_max -> class/num/lisp_max

### (max num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_min -> class/num/lisp_min

### (min num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_mod -> class/num/lisp_mod

### (% num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_mul -> class/num/lisp_mul

### (* num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_n2f -> class/num/lisp_n2f

### (n2f num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_n2i -> class/num/lisp_n2i

### (n2i num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_n2r -> class/num/lisp_n2r

### (n2r num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_ne -> class/num/lisp_ne

### (/= num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_neg -> class/num/lisp_neg

### (neg num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_or -> class/num/lisp_or

### (logior [num] ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_random -> class/num/lisp_random

### (random num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_shl -> class/num/lisp_shl

### (<< num cnt)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_shr -> class/num/lisp_shr

### (>> num cnt)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_sign -> class/num/lisp_sign

### (sign num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_sqrt -> class/num/lisp_sqrt

### (sqrt num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_sub -> class/num/lisp_sub

### (- num num ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_xor -> class/num/lisp_xor

### (logxor [num] ...)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
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

