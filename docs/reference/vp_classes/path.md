# :path

```image
docs/reference/vp_classes/path.cwb
```

## :fixeds

## Lisp Bindings

### (path-filter tol src dst) -> dst

### (path-gen-arc cx cy start end radius dst) -> dst

### (path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y dst) -> dst

### (path-gen-quadratic p1x p1y p2x p2y p3x p3y dst) -> dst

### (mat3x2-mul ma mb [out]) -> out

### (mat3x2-vec2-mul m3x2 src dst) -> dst

### (path-simplify tol src dst) -> dst

### (path-stroke-polygon path radius join) -> paths

### (path-stroke-polyline path radius join cap1 cap2) -> path

### (path-svg d) -> commands

## VP methods

### :create -> gui/path/create

```code
outputs
:r0 = 0 if error, else path object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :filter_polygon -> gui/path/filter_polygon

```code
inputs
:r0 = path object (ptr)
:r1 = source path object, can be same (ptr)
:r2 = tolerance (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r9, :d0-:d15
```

### :filter_polyline -> gui/path/filter_polyline

```code
inputs
:r0 = path object (ptr)
:r1 = source path object, can be same (ptr)
:r2 = tolerance (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r9, :d0-:d15
```

### :gen_arc -> gui/path/gen_arc

```code
inputs
:r0 = path object (ptr)
:r1 = cx (fixed)
:r2 = cy (fixed)
:r3 = start angle (fixed)
:r4 = end angle (fixed)
:r5 = radius (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r7, :d0-:d15
```

### :gen_clerp -> gui/path/gen_clerp

```code
inputs
:r0 = path object (ptr)
:r1 = cx (fixed)
:r2 = cy (fixed)
:r3 = v1x (fixed)
:r4 = v1y (fixed)
:r5 = v2x (fixed)
:r6 = v2y (fixed)
:r7 = radius (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r4, :d0-:d15
```

### :gen_cubic -> gui/path/gen_cubic

```code
inputs
:r0 = path object (ptr)
:r1 = p1x (fixed)
:r2 = p1y (fixed)
:r3 = p2x (fixed)
:r4 = p2y (fixed)
:r5 = p3x (fixed)
:r6 = p3y (fixed)
:r7 = p4x (fixed)
:r8 = p4y (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r11, :d0-:d15
```

### :gen_quadratic -> gui/path/gen_quadratic

```code
inputs
:r0 = path object (ptr)
:r1 = p1x (fixed)
:r2 = p1y (fixed)
:r3 = p2x (fixed)
:r4 = p2y (fixed)
:r5 = p3x (fixed)
:r6 = p3y (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r7, :d0-:d15
```

### :mat3x2_mul -> gui/path/mat3x2_mul

```code
inputs
:r0 = output fixeds object, can be ma or mb (ptr)
:r1 = ma fixeds object (ptr)
:r2 = mb fixeds object (ptr)
outputs
:r0 = output fixeds object (ptr)
trashes
:r1-:r13
info
the matrix that does what mb does and then what ma does, each
product shifted as (:fixeds :dot) shifts it. mb is read first and a
row of ma before that row is written, so out can be either
```

### :mat3x2_v2_mul -> gui/path/mat3x2_v2_mul

```code
inputs
:r0 = path object (ptr)
:r1 = source path object, can be same (ptr)
:r2 = m3x2 fixeds object (ptr)
outputs
:r0 = path object (ptr)
trashes
:r1-:r13
```

### :simplify -> gui/path/simplify

```code
inputs
:r0 = path object (ptr)
:r1 = source path object (ptr)
:r2 = tolerance (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r10, :d0-:d15
```

### :stroke_joints -> gui/path/stroke_joints

```code
inputs
:r0 = path object (ptr)
:r1 = in path start iter (plong)
:r2 = in path end iter (plong)
:r3 = p1x (fixed)
:r4 = p1y (fixed)
:r5 = p2x (fixed)
:r6 = p2y (fixed)
:r7 = radius (fixed)
:r8 = join style (byte)
outputs
:r0 = path object (ptr)
trashes
:r1-:r13, :d0-:d15
```

### :stroke_polygon -> gui/path/stroke_polygon

```code
inputs
:r0 = path object (ptr)
:r1 = radius (fixed)
:r2 = join style (byte)
outputs
:r0 = path object (ptr)
:r1 = output path1 object (ptr)
:r2 = output path2 object (ptr)
trashes
:r1-:r14, :d0-:d15
```

### :stroke_polyline -> gui/path/stroke_polyline

```code
inputs
:r0 = path object (ptr)
:r1 = radius (fixed)
:r2 = join style (byte)
:r3 = cap style1 (byte)
:r4 = cap style2 (byte)
outputs
:r0 = path object (ptr)
:r1 = output path object (ptr)
trashes
:r1-:r13, :d0-:d15
```

### :type -> gui/path/type

```code
inputs
:r0 = path object (ptr)
outputs
:r0 = path object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> gui/path/create

```code
outputs
:r0 = 0 if error, else path object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :vtable -> gui/path/vtable

