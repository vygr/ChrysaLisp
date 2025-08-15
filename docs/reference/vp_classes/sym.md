# sym

## str

## Lisp Bindings

### (gensym)

### (sym str)

## VP methods

### :get_static_sym -> class/sym/get_static_sym

```code
inputs
:r1 = static sym num (uint)
outputs
:r1 = sym object (ptr)
trashes
:r1, :r3
```

### :intern_cstr -> class/sym/intern_cstr

```code
inputs
:r0 = c string pointer (pubyte)
outputs
:r0 = interned sym object (ptr)
trashes
:r0-:r11
```

### :intern_pstr -> class/sym/intern_pstr

```code
inputs
:r0 = start (pubyte)
:r1 = end (pubyte)
outputs
:r0 = interned sym object (ptr)
trashes
:r0-:r11
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

