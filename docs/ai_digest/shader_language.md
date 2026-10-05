# The Shader Language

ChrysaLisp has its own shader language. A shader is written as s-expressions,
read by the Lisp reader, type checked in Lisp, and handed to a back end. One
back end gives the text of a GLSL fragment shader for a GPU. Another gives a
Lisp lambda that shades pixels with no GPU at all, which can be farmed over the
nodes of a network. The same source, the same inputs, the same picture.

This is the first step towards GPU support. There is no host GPU interface
yet, and no `@Gpu` service, see "What Is Not Here Yet". What is here is the
part that had no decisions waiting on it, the language, the two back ends, the
way the values of an app's controls reach a shader each frame, and a demo that
runs the lot.

## Why Our Own Language

A GPU is programmed in a shading language, and each graphics interface has its
own, GLSL, HLSL, MSL, WGSL, SPIR-V. A ChrysaLisp app should not have to care
which host it is on, and a system with no GPU should still give the picture.

With a language of our own the source of a shader is data, a list. Lisp code
can read it, check it, build it, and turn it into whatever the host needs. A
new host shading language is a new back end of a few hundred lines, not a
rewrite of every shader.

## The Files

* `lib/gpu/shader.inc`, the reader, the type checker, and the inputs block.
* `lib/gpu/glsl.inc`, the GLSL text back end.
* `lib/gpu/cpu.inc`, the CPU back end.
* `lib/gpu/shaders/raymarch.shader`, the surface raymarch demo, a port of
  https://vygr.github.io/JS-Raymarch.
* `apps/demos/surface/`, an app that runs that shader with no GPU.
* `tests/gpu/test_shader.lisp`, the tests.

## A Shader

A shader file is a list of declarations. This one fades from black to a colour
across the frame.

```lisp
(input resolution :vec2)
(input level :float 0.5 0.0 1.0)

(const tint (vec3 1.0 0.5 0.25))

(defun main :vec4 ((frag :vec2))
	(defq uv (/ frag resolution))
	(return (vec4 (* tint (:x uv) level) 1.0)))
```

Every shader has a `main`. It takes the coordinate of the pixel, with the
centre of the bottom left pixel at 0.5 0.5 and y going up, as `gl_FragCoord`
does, and it returns the colour of the pixel as red, green, blue and alpha.

## Types

`:float`, `:int`, `:bool`, `:vec2`, `:vec3` and `:vec4`. A vector is of floats.

A number with a decimal point is a float, `2.0`. A number without is an int,
`2`. They do not mix, `(+ 1 2.0)` is an error, as it is in GLSL. `(float i)`
and `(int f)` convert.

The Lisp reader gives a float literal as a 16.16 fixed point number, which is
good to 4 decimal places, so a float literal is rounded to 4 places, `0.001`
is 0.0010 and not the 0.00099 the fixed holds. A number that needs more digits
is written as a string, `(const pi "3.1415926535898")`. Both back ends are
given the same decimal text.

`:t` and `:nil` are the bool literals.

## Declarations

* `(input name type [default min max])`. A value the app gives for each
  frame. The type is `:float`, `:int` or a vector. A float or int can have a
  default, and a range, which is there for the app to build a control from. In
  GLSL an input is a uniform.
* `(const name expr)`. A constant, from literals and other constants.
* `(global name expr)`. A value worked out once a frame from the inputs,
  before any pixel is shaded. It can not be set by a function.
* `(defun name type ((param type) ...) body ...)`. A function. It must be
  declared before it is called, so there is no recursion. Every path through
  it must end at a `(return expr)`.

## Statements

* `(defq name expr ...)`. New locals, the type is that of the expr.
* `(setq name expr ...)`. Set a local or a parameter. A parameter is a copy,
  the caller's value does not change. `(setq (:w color) 1.0)` and
  `(setq (:xz p) (vec2 0.0 1.0))` set components of a vector.
* `(if test then [else])`, `(when test body ...)` and `(progn body ...)`.
* `(for (i start end) body ...)`. `i` is an int that counts from `start` up
  to, but not including, `end`. The bounds are int literals or constants, a
  shader loop must have a known limit. `i` can not be set.
* `(break)`. Leave the loop.
* `(return expr)`. Can be anywhere, inside a loop too.

A name can only mean one thing. A local can not have the name of an input, a
constant, a global, a function, a parameter, or another local that is in
scope. Two blocks that are side by side can each have a local of the same
name.

## Expressions

* `+ - * /` take any number of args. Vectors work by component, and a float
  with a vector applies to each component. `(- x)` is negate.
* `< <= > >=` compare two floats or two ints. `=` and `/=` take bools as
  well. `and`, `or` and `not` work on bools.
* `(vec2 ...)`, `(vec3 ...)` and `(vec4 ...)` build a vector from floats, ints
  and smaller vectors, or from a single number for every component.
* `(:x v)`, `(:xy v)`, `(:zyx v)` pick components, `xyzw` or `rgba`.
* `sin cos sqrt floor fract abs`, of a float, or of each component.
* `min max mod`, of two the same, or of a vector and a float.
* `(pow x y)`, `(clamp x lo hi)`, `(mix a b t)`.
* `(dot a b)`, `(cross a b)`, `(length v)`, `(normalize v)`, `(reflect i n)`.

They mean what they mean in GLSL. `fract` of -0.25 is 0.75, `mod` of -0.25 and
2.0 is 1.75.

## Using It

```lisp
(import "lib/gpu/glsl.inc")
(import "lib/gpu/cpu.inc")

(defq program (shader-load "lib/gpu/shaders/raymarch.shader"))
```

`(shader-load file)` reads and checks a file. `(shader-compile forms)` does
the same for a list of forms, and `(shader-read stream)` gives the forms from
a stream. A fault in the source is thrown as an error with the form at fault.

The program is a list, `(inputs consts globals funcs)`, in which every
expression carries its type. That tree is all a back end is given. The layout
of it is in the comments at the head of `lib/gpu/shader.inc`.

## The GLSL Back End

`(shader-glsl program)` gives the text of a fragment shader. Each input is a
uniform of the same name, each function is a function, `main` becomes
`shader_main`, and the `main` of the text sets the globals from the uniforms
and calls it with `gl_FragCoord.xy`. The text is written to compile as both
GLSL ES 1.00, which is WebGL, and desktop GLSL 1.20.

The text for the raymarch shader was compiled and run on the GPU of an Apple
M4 Max, in an offscreen context, and the pixels read back as floats. That is
how the CPU back end was checked, and six pixels of that render are in the
test module as its reference.

## The CPU Back End

`(shader-cpu program)` gives a Lisp lambda that shades a tile.

```lisp
(defq shade (shader-cpu program)
	args (shader-cpu-args program '((time 2.0) (resolution (64.0 48.0)))))
(defq pixels (apply shade (cat (list 0 0 64 48) args)))
```

The lambda takes the tile, `x y x1 y1`, then a value for each input, in the
order they are declared. It returns a list of the pixels, row by row, each a
`reals` of 4. `(shader-cpu-args program [vals])` gives the input values from a
list of `(name value)` pairs, with the default for any input not named.

A float is a `real`, a vector a `reals`, an int a `num`. The types are known,
so the code that is generated calls the right primitive directly, `nums-add`
for two vectors, `nums-scale` for a vector and a float, `+` for two floats.
There is no test of type at run time.

Three things make the generated code quick for interpreted Lisp.

* Every function symbol in it is prebound, the list holds the function, not
  its name. A call of one shader function from another holds the lambda of
  the callee itself, so that is the prebound lambda path of `:lisp
  :repl_eval`, with no lookup.
* Constants are folded when the lambda is built. `(vec3 0.0)`, a constant
  times a constant, a float literal made a `real`, are all values in the code.
* No vector is ever changed in place, so a constant vector can be shared by
  everything that uses it.

A `(break)` or an early `(return)` has no direct match in Lisp. The back end
turns the rest of a block, after an `if` that may leave, into the other side
of that `if`, and a loop into a `while` with a flag.

`pow` is not a primitive of `real`. It is done by squaring for the whole part
of the power, and by repeated square roots for the rest.

## Do They Agree

The raymarch shader was rendered by both back ends at 48 by 36 over five
settings of its controls, and every pixel compared, CPU back end in doubles
against the GPU in 32 bit floats.

| Settings | Largest difference | Pixels over 0.01 |
|---|---|---|
| defaults, time 2.0, 64 by 48 | 0.0021 | 0 of 3072 |
| depth 2, reflection 0.5, occlusion 0.6 | 0.0107 | 1 of 1728 |
| depth 0, displace 0.02, shadow 128 | 0.0001 | 0 of 1728 |
| anti alias on, march 0.5 | 0.0036 | 0 of 1728 |
| adaptive anti alias, debug on, limit 8 | 0.0051 | 0 of 1728 |
| bump 0.005 | 0.1227 | 22 of 1728 |

The last is not a fault. The bump map is built on `fract(sin(n) * 43758.5453)`,
a hash that depends on the last bits of a float, so 32 bit and 64 bit maths
give different noise. Any two GPUs can differ there as well.

## The Inputs Block

The values of the inputs for a frame travel as one block of bytes, laid out by
the std140 rules of a GPU uniform block. A float is 32 bits, an int is 32
bits, a vector is 2, 3 or 4 floats, with a `vec2` on an 8 byte boundary and a
`vec3` or `vec4` on a 16 byte boundary.

* `(shader-layout program)` gives `(size (name type offset) ...)`.
* `(shader-pack program [vals])` gives the block, a string, from `(name
  value)` pairs, with the default for any input not named.
* `(shader-unpack program block)` gives the `(name value)` pairs back.

This is how the controls of an app reach a shader. Each frame the app reads
its sliders, packs one block, and sends it with the request to draw. The
block for the raymarch shader is 64 bytes. A GPU service can hand the bytes
to the GPU as they are. The CPU back end unpacks them, so it shades from the
same 32 bit values the GPU would be given.

## The Demo

`apps/demos/surface` is the raymarch shader on screen, with no GPU.

The app reads the shader, and makes a slider for each input that has a range,
with its name and value. It knows nothing else of the shader, a new input in
the shader file is a new slider. It gives the two inputs that have no range,
`time` and `resolution`, itself.

Each frame it packs the inputs block and cuts the frame into tiles of 4 rows.
A farm of child tasks, one for each node, shades them. A child compiles the
shader with the CPU back end when it starts, and for each tile unpacks the
block, shades, and sends back the pixels. When the last tile is in the frame
is shown and the next one starts, with whatever the sliders say by then.

On an Apple M4 Max, 16 nodes, a 320 by 240 frame at the default settings takes
about 0.55 seconds. One node shades from 7,000 to 10,000 pixels a second.
Reading and checking the shader, 240 lines, takes about a millisecond.

The app is not in the launcher's list yet. To try it add `"surface"` to the
Demos list in `apps/system/launcher/app.lisp`, or to your own launcher config.

## What The CPU Back End Does Not Do

* A divide by zero is an error on an error checked build, where a GPU gives
  an infinity. The raymarch shader at time 0.0 has the camera at the point it
  looks at, and so fails there, on a GPU it gives a black frame.
* It is interpreted. The next step is a back end that gives VP code, the real
  registers and instructions are there, see `apps/demos/raymarch/lisp.vp`,
  and the VP maths is 64 bit where this is too.

## What Is Not Here Yet

* The host GPU interface. The choice of host library is open, OpenGL as the
  GLSL back end stands, the SDL3 GPU interface, or WebGPU. The last two do not
  take GLSL text, they would each need a back end of their own, SPIR-V or MSL
  for SDL3, WGSL for WebGPU.
* The `@Gpu` service. The plan is one service for each GPU, as there can be
  many `@Net` services, that takes a program, an inputs block and a target,
  and gives a texture id, or the pixels for a card that is not the display's.
* Vertex shaders, meshes, textures as inputs, and compute.
