# sys_math

### :f_cos -> sys/math/f_cos

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = cosine (fixed)
trashes
:r0-:r4
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
:r0 = sine (fixed)
trashes
:r0-:r4
```

### :f_sqrt -> sys/math/f_sqrt

```code
inputs
:r0 = number (fixed)
outputs
:r0 = sqrt (fixed)
trashes
:r0-:r3
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

### :i_sqrt -> sys/math/i_sqrt

```code
inputs
:r0 = number (ulong)
outputs
:r0 = sqrt (ulong)
trashes
:r0-:r3
```

### :r_add -> sys/math/r_add

```code
inputs
:r13 = real (32:r32)
:r14 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r11-:r14
```

### :r_cos -> sys/math/r_cos

```code
inputs
:r13 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
all
```

### :r_div -> sys/math/r_div

```code
inputs
:r13 = real (32:r32)
:r14 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r11-:r14
```

### :r_f2r -> sys/math/r_f2r

```code
inputs
:r14 = num (fixed)
outputs
:r13 = real (32:r32)
trashes
:r12-:r14
```

### :r_floor -> sys/math/r_floor

```code
inputs
:r13 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r12-:r14
```

### :r_frac -> sys/math/r_frac

```code
inputs
:r13 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r10-:r14
```

### :r_i2r -> sys/math/r_i2r

```code
inputs
:r14 = num (long)
outputs
:r13 = real (32:r32)
trashes
:r12-:r14
```

### :r_mod -> sys/math/r_mod

```code
inputs
:r13 = real (32:r32)
:r14 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r9-:r14
```

### :r_mul -> sys/math/r_mul

```code
inputs
:r13 = real (32:r32)
:r14 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r11-:r14
```

### :r_pack -> sys/math/r_pack

```code
inputs
:r13 = exponent (long)
:r14 = mantisa (long)
outputs
:r13 = real (32:r32)
trashes
:r12-:r14
```

### :r_r2f -> sys/math/r_r2f

```code
inputs
:r13 = real (32:r32)
outputs
:r14 = num (fixed)
trashes
:r12-:r14
```

### :r_r2i -> sys/math/r_r2i

```code
inputs
:r13 = real (32:r32)
outputs
:r14 = num (long)
trashes
:r12-:r14
```

### :r_sin -> sys/math/r_sin

```code
inputs
:r13 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
all
```

### :r_sqrt -> sys/math/r_sqrt

```code
inputs
:r13 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r6-:r14
```

### :r_sub -> sys/math/r_sub

```code
inputs
:r13 = real (32:r32)
:r14 = real (32:r32)
outputs
:r13 = real (32:r32)
trashes
:r11-:r14
```

