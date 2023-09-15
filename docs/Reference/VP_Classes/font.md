# font

## obj

### :ascii_textures -> gui/font/ascii_textures

```code
inputs
:r0 = font object (ptr)
outputs
:r0 = font object (ptr)
:r1 = list of ascii texture objects (ptr)
trashes
:r1-:r14
```

### :create -> gui/font/create

### :deinit -> gui/font/deinit

```code
inputs
:r0 = font object (ptr)
outputs
:r0 = font object (ptr)
trashes
:r1-:r14
```

### :flush -> gui/font/flush

```code
trashes
:r0-:r14
```

### :get_metrics -> gui/font/get_metrics

```code
inputs
:r0 = font object (ptr)
outputs
:r0 = font object (ptr)
:r1 = ascent (pixels)
:r2 = descent (pixels)
:r3 = height (pixels)
trashes
:r1-:r4
```

### :glyph_bounds -> gui/font/glyph_bounds

```code
inputs
:r0 = font object (ptr)
:r1 = glyph info array object (ptr)
outputs
:r0 = font object (ptr)
:r1 = width (pixels)
:r2 = height (pixels)
trashes
:r1-:r7
```

### :glyph_data -> gui/font/glyph_data

```code
inputs
:r0 = font object (ptr)
:r1 = char code (uint)
outputs
:r0 = font object (ptr)
:r1 = 0, else glyph data pointer (ptr)
trashes
:r1-:r4
```

### :glyph_info -> gui/font/glyph_info

```code
inputs
:r0 = font object (ptr)
:r1 = utf8 encoded str object (ptr)
outputs
:r0 = font object (ptr)
:r1 = glyph info array object (ptr)
trashes
:r1-:r8
```

### :glyph_paths -> gui/font/glyph_paths

```code
inputs
:r0 = font object (ptr)
:r1 = stack array object (ptr)
:r2 = glyph info array object (ptr)
outputs
:r0 = font object (ptr)
:r1 = glyph paths list object (ptr)
:r2 = width (pixels)
:r3 = height (pixels)
trashes
:r1-:r14
```

### :glyph_ranges -> gui/font/glyph_ranges

```code
inputs
:r0 = font object (ptr)
outputs
:r0 = font object (ptr)
:r1 = glyph ranges array object (ptr)
trashes
:r1-:r7
```

### :init -> gui/font/init

```code
inputs
:r0 = font object (ptr)
:r1 = vtable (pptr)
:r2 = name c string (pubyte)
:r3 = 0, else ctf data string object (ptr)
:r4 = font size (pixels)
outputs
:r0 = font object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :lisp_create -> gui/font/lisp_create

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

### :lisp_glyph_bounds -> gui/font/lisp_glyph_bounds

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

### :lisp_glyph_paths -> gui/font/lisp_glyph_paths

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

### :lisp_glyph_ranges -> gui/font/lisp_glyph_ranges

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

### :lisp_info -> gui/font/lisp_info

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

### :lisp_texture -> gui/font/lisp_texture

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

### :open -> gui/font/open

```code
:r0 = name c string (pubyte)
:r1 = font size (pixels)
outputs
:r0 = 0 if error, else font object (ptr)
trashes
:r0-:r14
```

### :sym_texture -> gui/font/sym_texture

```code
inputs
:r0 = font object (ptr)
:r1 = utf8 encoded sym object (ptr)
outputs
:r0 = font object (ptr)
:r1 = 0, else texture object (ptr)
trashes
:r1-:r14
```

### :vtable -> gui/font/vtable

