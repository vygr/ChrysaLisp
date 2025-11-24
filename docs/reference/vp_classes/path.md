# path

## fixeds

## Lisp Bindings

### (path-filter tol src dst) -> dst

### (path-gen-arc cx cy start end radius dst) -> dst

### (path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y dst) -> dst

### (path-gen-quadratic p1x p1y p2x p2y p3x p3y dst) -> dst

### (path-simplify tol src dst) -> dst

### (path-stroke-polygon path radius join) -> paths

### (path-stroke-polyline path radius join cap1 cap2) -> path

### (path-svg d) -> commands

### (path-transform m3x2 src dst) -> dst

## VP methods

### :create -> gui/path/create

### :filter_polygon -> gui/path/filter_polygon

```code
inputs
:r0 = path object (ptr)
:r1 = source path object, can be same (ptr)
:r2 = tolerance (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
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
:r1-:r14
```

### :gen_arc -> gui/path/gen_arc

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = cx (fixed)
:r3 = cy (fixed)
:r4 = start angle (fixed)
:r5 = end angle (fixed)
:r6 = radius (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :gen_clerp -> gui/path/gen_clerp

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = cx (fixed)
:r3 = cy (fixed)
:r4 = v1x (fixed)
:r5 = v1y (fixed)
:r6 = v2x (fixed)
:r7 = v2y (fixed)
:r8 = radius (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :gen_cubic -> gui/path/gen_cubic

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = p1x (fixed)
:r3 = p1y (fixed)
:r4 = p2x (fixed)
:r5 = p2y (fixed)
:r6 = p3x (fixed)
:r7 = p3y (fixed)
:r8 = p4x (fixed)
:r9 = p4y (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :gen_quadratic -> gui/path/gen_quadratic

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = p1x (fixed)
:r3 = p1y (fixed)
:r4 = p2x (fixed)
:r5 = p2y (fixed)
:r6 = p3x (fixed)
:r7 = p3y (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :simplify -> gui/path/simplify

```code
inputs
:r0 = path object (ptr)
:r1 = source path object (ptr)
:r2 = stack array object (ptr)
:r3 = tolerance (fixed)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :stroke_joints -> gui/path/stroke_joints

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = in path start iter (plong)
:r3 = in path end iter (plong)
:r4 = p1x (fixed)
:r5 = p1y (fixed)
:r6 = p2x (fixed)
:r7 = p2y (fixed)
:r8 = radius (fixed)
:r9 = join style (byte)
outputs
:r0 = path object (ptr)
trashes
:r1-:r14
```

### :stroke_polygon -> gui/path/stroke_polygon

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = radius (fixed)
:r3 = join style (byte)
outputs
:r0 = path object (ptr)
:r1 = output path1 object (ptr)
:r2 = output path2 object (ptr)
trashes
:r1-:r14
```

### :stroke_polyline -> gui/path/stroke_polyline

```code
inputs
:r0 = path object (ptr)
:r1 = stack array object (ptr)
:r2 = radius (fixed)
:r3 = join style (byte)
:r4 = cap style1 (byte)
:r5 = cap style2 (byte)
outputs
:r0 = path object (ptr)
:r1 = output path object (ptr)
trashes
:r1-:r14
```

### :transform -> gui/path/transform

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

### :type -> gui/path/type

### :vcreate -> gui/path/create

### :vtable -> gui/path/vtable

