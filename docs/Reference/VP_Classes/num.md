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

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(abs num)
```

### :lisp_add -> class/num/lisp_add

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(+ num num ...)
```

### :lisp_and -> class/num/lisp_and

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(logand [num] ...)
```

### :lisp_asr -> class/num/lisp_asr

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(>>> num cnt)
```

### :lisp_div -> class/num/lisp_div

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(/ num num ...)
```

### :lisp_eq -> class/num/lisp_eq

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(= num num ...)
```

### :lisp_ge -> class/num/lisp_ge

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(>= num num ...)
```

### :lisp_gt -> class/num/lisp_gt

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(> num num ...)
```

### :lisp_intern -> class/num/lisp_intern

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(num-intern num)
```

### :lisp_le -> class/num/lisp_le

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(<= num num ...)
```

### :lisp_lt -> class/num/lisp_lt

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(< num num ...)
```

### :lisp_max -> class/num/lisp_max

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(max num num ...)
```

### :lisp_min -> class/num/lisp_min

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(min num num ...)
```

### :lisp_mod -> class/num/lisp_mod

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(% num num ...)
```

### :lisp_mul -> class/num/lisp_mul

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(* num num ...)
```

### :lisp_n2f -> class/num/lisp_n2f

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(n2f num)
```

### :lisp_n2i -> class/num/lisp_n2i

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(n2i num)
```

### :lisp_n2r -> class/num/lisp_n2r

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(n2r num)
```

### :lisp_ne -> class/num/lisp_ne

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(/= num num ...)
```

### :lisp_neg -> class/num/lisp_neg

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(neg num)
```

### :lisp_or -> class/num/lisp_or

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(logior [num] ...)
```

### :lisp_random -> class/num/lisp_random

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(random num)
```

### :lisp_shl -> class/num/lisp_shl

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(<< num cnt)
```

### :lisp_shr -> class/num/lisp_shr

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(>> num cnt)
```

### :lisp_sign -> class/num/lisp_sign

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(sign num)
```

### :lisp_sqrt -> class/num/lisp_sqrt

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(sqrt num)
```

### :lisp_sub -> class/num/lisp_sub

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(- num num ...)
```

### :lisp_xor -> class/num/lisp_xor

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(logxor [num] ...)
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

