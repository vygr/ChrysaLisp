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
:r0-:r2, :d0-:d3
info
0 if the double is not greater than 0. The whole part of the power is
by squaring, the rest by repeated roots, to 31 bits of it. It is the
method of the shader language, every back end of which must agree.
The double waits in :d3 and the whole part in :r2, and the 1 of the
roots is kept in :d3 while they are taken, so none of it is memory
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

### :d_tanh -> sys/math/d_tanh

```code
inputs
:d0 = double (double)
outputs
:d0 = tanh of the double (double)
trashes
:r0-:r2, :d0-:d3
info
of the double without its sign, the sign put back after. 1 from 22 on,
where it is 1 to the last bit. Below 0.55, e to the twice of it less 1
by its series to the 19th power, t, nothing taken from what is near
it, and t / (t + 2). Between, 1 - 2 / (e to the twice of it + 1), by
d_exp, from 0.55 on the 1 is never much more than what it takes. Of
858 doubles from -25 to 25, 2 units of the last place from the true
tanh at the most, 680 of them the nearest double
```

### :f_cos -> sys/math/f_cos

```code
inputs
:f0 = single (single)
outputs
:f0 = cos of the single (single)
trashes
:r0-:r1, :d0-:d3
info
the sine of a quarter turn on, the quarter turn added in doubles, so
near where it is 0 it has all the bits a single can
```

### :f_exp -> sys/math/f_exp

```code
inputs
:f0 = power (single)
outputs
:f0 = e to the power (single)
trashes
:r0-:r2, :d0-:d3
info
d_exp of singles: 0 below the least power that gives a whole single,
infinity above the greatest. The power is k ln2 + r, ln2 in two parts,
the first of few bits so k times it is exact, r no more than half ln2
either way. e to the r by its series to the 7th power, all a single
holds, and 2 to the k made from its bits, in two halves so neither is
out of range
```

### :f_log -> sys/math/f_log

```code
inputs
:f0 = single (single)
outputs
:f0 = natural log of the single (single)
trashes
:r0-:r2, :d0-:d3
info
d_log of singles: minus infinity of 0, not a number below 0, infinity
and not a number as they are. The single is 2 to the e times m, m from
the root of a half to the root of 2, and the log of m is 2 atanh of f,
f (m - 1) / (m + 1), by its series in f squared to the 5th power
```

### :f_sin -> sys/math/f_sin

```code
inputs
:f0 = single (single)
outputs
:f0 = sin of the single (single)
trashes
:r0-:r1, :d0-:d3
info
the single made a double and brought to the turn, and the series, in
doubles, as d_sin does, to the nearest single
```

### :f_tanh -> sys/math/f_tanh

```code
inputs
:f0 = single (single)
outputs
:f0 = tanh of the single (single)
trashes
:r0-:r2, :d0-:d3
info
d_tanh of singles: 1 from 10 on, below 0.55 the series of e to the
twice of it less 1 to the 11th power, t / (t + 2), between 1 - 2 / (e
to the twice of it + 1), by f_exp. 2 units of the last place from the
true tanh at the most, as d_tanh
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
info
by f_cos: a single holds an angle of a drawing, to 256 radians, to all
16 bits of its fraction, and its answer to more than a fixed has
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

### :q_sin -> sys/math/q_sin

```code
inputs
:r0 = angle in radians (fixed)
outputs
:r0 = sin (fixed)
trashes
:r0-:r2, :d0-:d3
info
by f_sin: a single holds an angle of a drawing, to 256 radians, to all
16 bits of its fraction, and its answer to more than a fixed has
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

