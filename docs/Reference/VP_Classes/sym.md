# sym

## str

### :get_static_sym -> class/sym/get_static_sym

```code
inputs
:r1 = static sym num (uint)
outputs
:r1 = sym object (ptr)
trashes
:r1, :r3
```

### :intern -> class/sym/intern

```code
inputs
:r0 = sym object (ptr)
outputs
:r0 = interned sym object (ptr)
trashes
:r0-:r14
info
input sym IS derefed
vtable MUST be a sym
```

### :intern_cstr -> class/sym/intern_cstr

```code
inputs
:r0 = c string pointer (pubyte)
outputs
:r0 = interned sym object (ptr)
trashes
:r0-:r14
```

### :intern_str -> class/sym/intern_str

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = interned sym object (ptr)
trashes
:r0-:r14
info
input str IS NOT derefed
```

### :intern_strs -> class/sym/intern_strs

```code
inputs
:r1 = list of string objects (ptr)
outputs
:r1 = list of sym objects (ptr)
trashes
:r0-:r14
```

### :lisp_gensym -> class/sym/lisp_gensym

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

### :lisp_sym -> class/sym/lisp_sym

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

### :print -> class/sym/print

```code
inputs
:r0 = sym object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = sym object (ptr)
trashes
:r1-:r14
```

### :ref_static_sym -> class/sym/ref_static_sym

```code
inputs
:r1 = static sym num (uint)
outputs
:r1 = sym object (ptr)
trashes
:r1, :r3
```

### :statics_init -> class/sym/statics_init

```code
trashes
:r0-:r14
```

### :type -> class/sym/type

### :vtable -> class/sym/vtable

