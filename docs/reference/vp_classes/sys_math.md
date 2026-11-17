# :sys_math

```image
docs/reference/vp_classes/sys_math.cwb
```

## VP methods

### :d_exp -> sys/math/d_exp

```code
inputs
:d0 = power (double)
outputs
:d0 = e to the power (double)
trashes
:r0-:r2, :d0-:d3
info
0 below the least power that gives a whole double, infinity above
the greatest. The power is k ln2 + r, ln2 in two parts so r is exact,
no more than half ln2 either way. e to the r by its series to the 13th
power, and 2 to the k made from its bits, in two halves so neither is
out of range. It is the method of the shader language, every back end
of which must agree
```

### :d_log -> sys/math/d_log

```code
inputs
:d0 = double (double)
outputs
:d0 = natural log of the double (double)
trashes
:r0-:r2, :d0-:d3
info
minus infinity of 0, not a number below 0, infinity and not a number
as they are. The double is 2 to the e times m, m from the root of a half
to the root of 2, and the log of m is 2 atanh of f, f (m - 1) / (m + 1),
by its series in f squared to the 10th power. It is the method of the
shader language, every back end of which must agree
```

### :d_pow -> sys/math/d_pow

```code
inputs
:d0 = double (double)
:d1 = power (double)
outputs
:d0 = double to the power (double)
trashes
:r0-:r2, :d0-:d7
info
0 if the double is not greater than 0. The whole part of the power is
by squaring, the rest by repeated roots, to 31 bits of it. It is the
method of the shader language, every back end of which must agree
```

### :d_sin -> sys/math/d_sin

```code
inputs
:d0 = double (double)
outputs
:d0 = double (double)
trashes
:r0-:r1, :d0-:d3
```

### :i_rand -> sys/math/i_rand

```code
inputs
:r0 = random range (long)
outputs
:r0 = random number in range (long)
trashes
:r0-:r3, :d0-:d15
```

### :q_cos -> sys/math/q_cos

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = cos (fixed)
trashes
:r0-:r2, :d0-:d3
```

### :q_dist_sqd -> sys/math/q_dist_sqd

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
:r0-:r3, :r6-:r10
```

### :q_exp -> sys/math/q_exp

```code
inputs
:r0 = power (fixed)
outputs
:r0 = e to the power (fixed)
trashes
:r0-:r2, :d0-:d3
info
of the double, :sys_math :d_exp, so not to be had above 33
```

### :q_intersect -> sys/math/q_intersect

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
:r0-:r2, :r8-:r13
```

### :q_log -> sys/math/q_log

```code
inputs
:r0 = number (fixed)
outputs
:r0 = natural log of the number (fixed)
trashes
:r0-:r2, :d0-:d3
info
of the double, :sys_math :d_log, so not to be had of 0 or less
```

### :q_sin -> sys/math/q_sin

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = sin (fixed)
trashes
:r0-:r2, :d0-:d3
```

### :q_sqrt -> sys/math/q_sqrt

```code
inputs
:r0 = number (fixed)
outputs
:r0 = sqrt (fixed)
trashes
:r0-:r1, :d0-:d1
```

