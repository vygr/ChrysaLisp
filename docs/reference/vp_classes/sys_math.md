# sys_math

## VP methods

### :f_cos -> sys/math/f_cos

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = cos (fixed)
trashes
:r0-:r2, :f0-:f3
```

### :f_dist_sqd -> sys/math/f_dist_sqd

```code
inputs
:r0 = px (fixed)
:r1 = py (fixed)
:r2 = p1x (fixed)
:r3 = p1y (fixed)
:r4 = p2x (fixed)
:r5 = p2y (fixed)
outputs
:r0 = distance squared (fixed)
trashes
:r0-:r14
```

### :f_intersect -> sys/math/f_intersect

```code
inputs
:r0 = p1x (fixed)
:r1 = p1y (fixed)
:r2 = p2x (fixed)
:r3 = p2y (fixed)
:r4 = v1x (fixed)
:r5 = v1y (fixed)
:r6 = v2x (fixed)
:r7 = v2y (fixed)
outputs
:r0 = ix (fixed)
:r1 = iy (fixed)
trashes
:r0-:r14
```

### :f_sin -> sys/math/f_sin

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = sin (fixed)
trashes
:r0-:r2, :f0-:f3
```

### :f_sqrt -> sys/math/f_sqrt

```code
inputs
:r0 = number (fixed)
outputs
:r0 = sqrt (fixed)
trashes
:r0-:r1, :f0-:f1
```

### :i_rand -> sys/math/i_rand

```code
inputs
:r0 = random range (ulong)
outputs
:r0 = random number in range (ulong)
trashes
:r0-:r3
```

### :r_sin -> sys/math/r_sin

```code
inputs
:f1 = real (real)
outputs
:f0 = real (real)
trashes
:r0-:r1, :f0-:f3
```

