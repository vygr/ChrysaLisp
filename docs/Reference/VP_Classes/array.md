# array

## seq

### :append -> class/array/append

```code
inputs
:r0 = array object (ptr)
:r1 = source array object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = array object (ptr)
trashes
:r1-:r9
```

### :cat -> class/array/cat

```code
inputs
:r0 = array object (ptr)
:r1 = list of array objects (ptr)
outputs
:r0 = 0 if error, else new array object (ptr)
trashes
:r0-:r11
```

### :clear -> class/array/clear

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
trashes
:r1
```

### :create -> class/array/create

### :deinit -> class/array/deinit

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
trashes
:r1-:r14
```

### :erase -> class/array/erase

```code
inputs
:r0 = array object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = array object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
```

### :erase2 -> class/array/erase2

```code
inputs
:r0 = array object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = array object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
```

### :find -> class/array/find

```code
inputs
:r0 = array object (ptr)
:r1 = element (long)
outputs
:r0 = array object (ptr)
:r1 = element (long)
:r2 = -1, else index (int)
trashes
:r2-:r4
```

### :get_begin -> class/array/get_begin

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = begin element iter (plong)
trashes
:r1
```

### :get_both -> class/array/get_both

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = begin element iter (plong)
:r2 = end element iter (plong)
trashes
:r1-:r2
```

### :get_capacity -> class/array/get_capacity

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = capacity (uint)
trashes
:r1
```

### :get_element -> class/array/get_element

```code
inputs
:r0 = array object (ptr)
:r1 = element index (uint)
outputs
:r0 = array object (ptr)
:r1 = element (long)
trashes
:r1-:r2
```

### :get_element2 -> class/array/get_element2

```code
inputs
:r0 = array object (ptr)
:r1 = element index (uint)
outputs
:r0 = array object (ptr)
:r1 = element1 (long)
:r2 = element2 (long)
trashes
:r1-:r2
```

### :get_end -> class/array/get_end

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = end element iter (plong)
trashes
:r1-:r2
```

### :get_first -> class/array/get_first

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = element (long)
trashes
:r1
```

### :get_first2 -> class/array/get_first2

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = element1 (long)
:r2 = element2 (long)
trashes
:r1-:r2
```

### :get_iter -> class/array/get_iter

```code
inputs
:r0 = array object (ptr)
:r1 = element index (uint)
outputs
:r0 = array object (ptr)
:r1 = element iter (plong)
trashes
:r1-:r2
```

### :get_iters -> class/array/get_iters

```code
inputs
:r0 = array object (ptr)
:r1 = begin index (uint)
:r2 = end index (uint)
outputs
:r0 = array object (ptr)
:r1 = begin element iter (plong)
:r2 = end element iter (plong)
trashes
:r1-:r3
```

### :get_length -> class/array/get_length

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = array length (uint)
trashes
:r1
```

### :get_second -> class/array/get_second

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = element (long)
trashes
:r1
```

### :init -> class/array/init

```code
inputs
:r0 = array object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = array object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r2
```

### :lisp_array -> class/array/lisp_array

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

### :lisp_cap -> class/array/lisp_cap

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

### :lisp_clear -> class/array/lisp_clear

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

### :lisp_elemset -> class/array/lisp_elemset

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

### :lisp_fixeds -> class/array/lisp_fixeds

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

### :lisp_nums -> class/array/lisp_nums

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

### :lisp_path -> class/array/lisp_path

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

### :lisp_pop -> class/array/lisp_pop

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

### :lisp_push -> class/array/lisp_push

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

### :lisp_reals -> class/array/lisp_reals

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

### :partition -> class/array/partition

```code
inputs
:r0 = array object (ptr)
:r1 = lower partition iter (plong)
:r2 = upper partition iter (plong)
:r3 = sort callback (ptr)
:r4 = sort context (ptr)
outputs
:r0 = array object (ptr)
:r1 = partition iter (plong)
trashes
:r1-:r14
sort callback
inputs
:r0 = sort context (ptr)
:r1 = iter1 (plong)
:r2 = iter2 (plong)
outputs
:r0 = +, 0, -
trashes
:r1-:r14
```

### :pop_back -> class/array/pop_back

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = element (long)
trashes
:r1-:r2
```

### :pop_back2 -> class/array/pop_back2

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = element1 (long)
:r2 = element2 (long)
trashes
:r1-:r2
```

### :print -> class/array/print

```code
inputs
:r0 = array object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = array object (ptr)
trashes
:r1-:r14
```

### :push_back -> class/array/push_back

```code
inputs
:r0 = array object (ptr)
:r1 = element (long)
outputs
:r0 = array object (ptr)
:r1 = element (long)
:r2 = begin element iter (plong)
:r3 = end element iter (plong)
trashes
:r2-:r5
```

### :push_back2 -> class/array/push_back2

```code
inputs
:r0 = array object (ptr)
:r1 = element1 (long)
:r2 = element2 (long)
outputs
:r0 = array object (ptr)
:r1 = element1 (long)
:r2 = element2 (long)
:r3 = begin element iter (plong)
:r4 = end element iter (plong)
trashes
:r3-:r5
```

### :ref_back -> class/array/ref_back

```code
inputs
:r0 = array object (ptr)
outputs
:r0 = array object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3
```

### :ref_element -> class/array/ref_element

```code
inputs
:r0 = array object (ptr)
:r1 = element index (uint)
outputs
:r0 = array object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3
```

### :rfind -> class/array/rfind

```code
inputs
:r0 = array object (ptr)
:r1 = element (long)
outputs
:r0 = array object (ptr)
:r1 = element (long)
:r2 = -1, else index (int)
trashes
:r2-:r4
```

### :set_capacity -> class/array/set_capacity

```code
inputs
:r0 = array object (ptr)
:r1 = capacity (uint)
outputs
:r0 = array object (ptr)
trashes
:r1-:r5
```

### :set_element -> class/array/set_element

```code
inputs
:r0 = array object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = array object (ptr)
trashes
:r2-:r3
```

### :set_length -> class/array/set_length

```code
inputs
:r0 = array object (ptr)
:r1 = length (uint)
outputs
:r0 = array object (ptr)
:r1 = length (uint)
trashes
none
```

### :slice -> class/array/slice

```code
inputs
:r0 = array object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = array object (ptr)
:r1 = slice array object (ptr)
trashes
:r1-:r8
```

### :sort -> class/array/sort

```code
inputs
:r0 = array object (ptr)
:r1 = stack array object (ptr)
:r2 = lower iter (plong)
:r3 = upper iter (plong)
:r4 = compare callback (ptr)
:r5 = sort context (ptr)
outputs
:r0 = array object (ptr)
trashes
:r1-:r14
sort callback
inputs
:r0 = context (ptr)
:r1 = iter1 (plong)
:r2 = iter2 (plong)
outputs
:r0 = +, 0, -
trashes
:r1-:r14
```

### :sort_callback -> class/obj/null

### :type -> class/array/type

### :vcreate -> class/array/create

### :velement -> class/num/create

### :vtable -> class/array/vtable
