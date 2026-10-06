# The Shader Language

ChrysaLisp has its own shader language. A shader is written as s-expressions,
read by the Lisp reader, type checked in Lisp, and handed to a back end. One
back end gives the text of a GLSL fragment shader for a GPU. One gives VP code,
a native function that shades pixels with no GPU at all, on any CPU ChrysaLisp
runs on, which can be farmed over the nodes of a network. One gives a Lisp
lambda that does the same, interpreted, the reference the others are checked
against. The same source, the same inputs, the same picture.

This is the first step towards GPU support. There is no host GPU interface
yet, and no `@Gpu` service, see "What Is Not Here Yet". What is here is the
part that had no decisions waiting on it, the language, the three back ends,
the way the values of an app's controls reach a shader each frame, and a demo
that runs the lot.

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
* `lib/gpu/msl.inc`, the Metal Shading Language text back end.
* `lib/gpu/spirv.inc`, the SPIR-V binary back end, for Vulkan.
* `lib/gpu/cpu.inc`, the CPU back end, interpreted Lisp.
* `lib/gpu/vp.inc`, the VP back end, native code.
* `lib/gpu/gui.inc`, a shader on the GPU of the GUI.
* `src/host/gui_sdl3.cpp`, the SDL3 GUI driver, which can draw one.
* `lib/gpu/shaders/raymarch.shader`, the surface raymarch demo, a port of
  https://vygr.github.io/JS-Raymarch.
* `apps/demos/surface/`, an app that runs that shader, on the GPU or with none.
* `cmd/shader.lisp`, the `shader` command, a shader compiled from the command
  line.
* `tests/gpu/test_shader.lisp`, the tests.

## A Shader

A shader file is a list of declarations. This one fades from black to a colour
across the frame.

```lisp
(definput resolution :vec2)
(definput level :float 0.5 0.0 1.0)

(defconst tint (vec3 1.0 0.5 0.25))

(defun main :vec4 ((frag :vec2))
	(defq uv (/ frag resolution))
	(vec4 (* tint (:x uv) level) 1.0))
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
is written as a string, `(defconst pi "3.1415926535898")`. Both back ends are
given the same decimal text.

`:t` and `:nil` are the bool literals.

## Declarations

Each begins with `def`, as `defun` and `defq` do in ChrysaLisp.

* `(definput name type [default min max])`. A value the app gives for each
  frame. The type is `:float`, `:int` or a vector. A float or int can have a
  default, and a range, which is there for the app to build a control from. In
  GLSL an input is a uniform.
* `(defconst name expr)`. A constant, from literals and other constants.
* `(defglobal name expr)`. A value worked out once a frame from the inputs,
  before any pixel is shaded. It can not be set by a function.
* `(defun name type ((param type) ...) body ...)`. A function. It must be
  declared before it is called, so there is no recursion. Its value is its
  last form, as in Lisp, see below. Every path through it must end at a
  value.

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
* `(return expr)`. Leave the function early, with this value. Can be
  anywhere, inside a loop too.

The value of a function is its last form. It need not say `return`.

```lisp
(defun hash :float ((n :float))
	(fract (* (sin n) 43758.5453)))

(defun bigger :float ((a :float) (b :float))
	(if (> a b) a b))
```

If the last form is an `if`, the last form of each of its arms is the value,
and so on down, through a `progn` as well. An `if` with one arm, a `when` and
a `for` are not values, a function that ends with one has not said what it
returns, and is refused. `return` is for leaving early. Of the 19 functions
of the raymarch shader one has a `return` in it.

Nothing else in a function is an expression on its own, a form that is not
last has to be a statement. And the back ends still see a `return`, GLSL,
MSL and SPIR-V are statement languages, the checker puts it there.

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
(import "lib/gpu/vp.inc")

(defq program (shader-load "lib/gpu/shaders/raymarch.shader"))
```

`(shader-load file)` reads and checks a file. `(shader-compile forms)` does
the same for a list of forms, and `(shader-read stream)` gives the forms from
a stream. A fault in the source is thrown as an error with the form at fault.

The program is a list, `(inputs consts globals funcs)`, in which every
expression carries its type. That tree is all a back end is given. The layout
of it is in the comments at the head of `lib/gpu/shader.inc`.

## The shader Command

`shader` compiles a shader file and shows what it is compiled to, or writes
it to a file. It is how to see what each back end makes of a shader, and it
makes the language a tool for work that has nothing to do with ChrysaLisp,
a shader written once here and handed to a GLSL, a Metal or a Vulkan
program.

```code
shader lib/gpu/shaders/raymarch.shader
shader -t msl lib/gpu/shaders/raymarch.shader
shader -t spirv lib/gpu/shaders/raymarch.shader
shader -t spirv -o raymarch.spv lib/gpu/shaders/raymarch.shader
```

The targets, `-t`, are `glsl`, the default, `msl`, `spirv`, `vp`, the VP
assembler source of the native code back end, `cpu`, the Lisp the CPU back
end runs, and `tree`, the checked and typed tree every back end is given.
`-v` gives the vertex shader that goes with every fragment shader, for `msl`
and `spirv`. `-o file` writes to a file.

SPIR-V is a binary. With no `-o` it is shown as a listing, an instruction to
a line, its name and its words. With `-o` the module itself is written, as a
driver takes it, and `spirv-dis` will name the ids of it.

A shader used outside has to be given what ChrysaLisp gives it here. Each
back end's section says what that is, the uniforms of the GLSL, the buffer
of the MSL and the set and binding of the SPIR-V, all laid out as the inputs
block, and the frag coord with y going up.

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

## The MSL Back End

`(shader-msl program)` gives the text of a fragment shader in the Metal
Shading Language, which is what the SDL3 GPU interface takes on a Mac. The
entry point is `fragment_main`.

MSL has no globals that can be set, and a function can not see the uniforms
of the entry point. So the shader is a struct. The inputs, constants and
globals are its members, the functions are its methods, and the entry point
copies the inputs in, sets the globals, and calls `shader_main`.

The inputs are one uniform buffer, at `[[buffer(0)]]`, a struct with packed
vectors and pad words that put each input at its offset in the inputs block.
So the host hands the block from `(shader-pack)` to the GPU as it is.

Every name of the shader is given a trailing `_`, so that it can not be a
word of MSL. `half` is a type there.

`(shader-msl-vertex)` gives the vertex shader that goes with every fragment
shader, one triangle that covers the target, entry point `vertex_main`. Its
uniform is the size of the target, and it gives each pixel its frag coord
with y going up, so a shader does not know that Metal counts y down.

This was run through the SDL3 GPU interface, SDL 3.4.16 on the Metal driver
of an Apple M4 Max, to a float texture and read back. Over the six settings
in the table below it agrees with OpenGL and with the CPU back end as closely
as they agree with each other. A 1024 by 768 frame, with the read back of
every pixel, takes 1.1 to 1.3ms. The first build of the shader by Metal takes
about 470ms, after that Metal has it cached and it takes 1ms.

The same run on a 2018 MacBook Pro, x86_64, with SDL 3.4.16 built from source,
gives the same picture, and takes 11.4ms for the 1024 by 768 frame and read
back. With the bump map on the two machines give quite different noise, 1,164
of 1,728 pixels differ by more than 0.01, the hash in the shader hangs on
how each GPU works out `sin`.

## The SPIR-V Back End

`(shader-spirv program)` gives a SPIR-V module, a fragment shader for Vulkan,
as SDL's GPU interface wants it, entry point `fragment_main`. It is the
binary, not text. No compiler is called on, there is no `glslc` to install,
the words of the module are made in Lisp, about 300 lines of it.

Every input, constant, global, parameter and local is a variable, read with
a load and set with a store, and the driver's own compiler makes registers of
them. The inputs are one uniform block, set 3 binding 0, with the offsets of
the inputs block, and are copied out of it at the start. Control flow is
structured, as SPIR-V has it, an `if` has a merge block, a `for` a merge block
and a continue block, and a `break` branches to the merge block of its loop.
The maths is the `GLSL.std.450` set, `mod` is `OpFMod`. Both sides of an `and`
and an `or` are worked out, where GLSL and MSL stop at the first that
settles it, and nothing in the language can tell the two apart but a function
that sets a global.

`(shader-spirv-vertex)` gives the vertex shader that goes with it, entry
point `vertex_main`, its uniform at set 1 binding 0. SDL's Vulkan driver has
y going up as its Metal driver does, so the two vertex shaders are the same
sums.

This was run on a Raspberry Pi 4, 2GB, Raspberry Pi OS on Debian 13, Mesa
26.2, SDL 3.4.16 built from source, on the Pi's own GPU, the V3D, through its
Vulkan driver, to a float texture and read back. `spirv-val` passes the
modules. The 64 by 48 raymarch frame is within 0.0025 of the CPU back end's
on every pixel, 3,072 of them. Mesa's software Vulkan driver, llvmpipe, gives
the same frame.

| | V3D | llvmpipe |
| :--- | ---: | ---: |
| First build of the shader | 17.9s | 0.9s |
| Build after that, from Mesa's cache | 1.4ms | 0.9s |
| 64 by 48 frame and read back | 6.0ms | 19.9ms |
| 640 by 480 frame and read back | 394ms | |

The first build is long. The V3D compiler takes 18 seconds over this shader,
once, then Mesa keeps the result on disk. It is not the form of the module.
The driver says what it is doing, `V3D_DEBUG=perf`, and it is this, it
compiles the shader, can not fit it in the registers the GPU has, and
compiles it again another way, three times in all, about six seconds each,
ending with 36 values spilled to memory. Every function of a SPIR-V module
is inlined, and this shader calls 42 times. The same module put through
`spirv-opt -O` first, which is what a GLSL compiler would hand over, takes 26
seconds. So nothing was changed in the back end. The driver builds the
shader on a thread of its own instead, see below, and the GUI carries on.

Every pixel the test suite checks was then run the same way, 42 of them, the
small shaders that test each operator, the loops, the function calls and the
inputs, as well as the six of the raymarch frame. On the Pi's GPU through
Vulkan, on its software Vulkan driver, and, as MSL, on an Apple M4 Max through
Metal, all 42 are within 0.00005 of what the CPU back end gives. No fault was
found in either back end.

Two things had to be found out to get SDL on to the V3D at all. The SDL3 of
Debian 13 is 3.2.10, and the GPU renderer the driver uses came with 3.4. And
a device SDL makes for itself asks for depth clamping, which the V3D does not
have, so SDL picks llvmpipe and says nothing. The sdl3 driver now makes its
own device, without the features it does not use.

With a TV on the Pi the GUI was then run on the sdl3 driver, and the shader
drawn into a canvas and read back, `(. canvas :swap +swap_read)`. The ten
pixels looked at are the same bytes the M4 gives. The surface demo in GPU
mode ran at 2 frames a second, 640 by 480, which fits the 394ms above,
and that was with nothing else able to draw, see the next section.

## On The GPU, In The GUI

Graphics belongs to the GUI. A GUI app runs on the node that has the GUI, so
that no pixmap has to cross a link, and a shader is drawn there too, by the
host GUI driver, into the texture of a canvas. There is no service and no
message. The texture is then composited like any other.

```lisp
(import "lib/gpu/gui.inc")

(defq shader (shader-gui program))
(when shader
	(. canvas :shade shader (shader-pack program vals)))
```

`(shader-gui program)` asks the host GUI driver which shading language it
takes, gives it the shader from that back end, MSL text or a SPIR-V module,
and returns the shader, or `:nil` if this driver can not draw one.
`(. canvas :shade shader block)` draws it over the whole canvas with that
inputs block. The pixmap of the canvas is not used and not changed. A later
`(. canvas :swap +swap_write)` puts the pixmap back on show, so an app can go from one to
the other frame by frame.

A shader is built by the driver in its own time, on a thread, so
`(shader-gui program)` returns at once, with a shader that may not be ready.
`:shade` returns the canvas if it drew, `:nil` if it drew nothing and the app
should try again on its next tick, and `:error` if the driver could not build
the shader. It draws nothing while the shader is still being built. On the
Raspberry Pi 4 that is the 18 seconds, the first time, and the surface demo's
status line says so while the desktop carries on, where the whole GUI used to
stop.

One shader draw is on the go at a time as well, so `:nil` is also the GPU not
having finished the last one. On a fast GPU it
never matters. On a slow one it is what keeps the desktop alive. The GUI is
drawn by the same GPU, a GPU can not be stopped part way through a draw, and
a Raspberry Pi 4 takes 400ms over a frame of the raymarch shader, so with a
frame asked for on every tick the mouse pointer moved twice a second.

So a part of the canvas can be drawn, `(. canvas :shade shader block x y x1
y1)`, in pixels of the texture, and the rest is left as it was. The surface
demo draws its GPU frame as strips, one on each tick the GPU is free, and
sizes the strip by how many ticks the last one took, to take the GPU more
than one tick and less than two. On the Pi that settles at 15 to 40 lines.
With the pointer moving the screen is then drawn 50 times a second, every
20ms, the rate of the TV, and the raymarch runs at about 1.5 frames a second
where it ran at 2.4 with the desktop frozen. On a Mac the strip is the whole
frame and nothing has changed.

The strips are not drawn on show, a frame filling in from the top is a poor
thing to watch. The demo has a second canvas that is never added to the
window, draws the strips into that, and when the frame is whole
`(. canvas :exchange that)` has the two canvases exchange their textures.

To get at the pixels the GPU drew, swap the other way, `(. canvas :swap
+swap_read)`. A swap with a negative number reads the texture back into the
pixmap. The raymarch shader drawn this way and read back is within 1 in 255
of the native code for it, on every pixel of a 64 by 48 frame.

Under that are three functions, `(canvas-shader-format)`,
`(canvas-shader-create vertex fragment)` and `(canvas-shader-destroy shader)`,
and six calls at the end of the host GUI table, `shader_format`,
`shader_create`, `shader_destroy`, `shader_texture`, `shader_draw` and
`read_texture`, see `docs/ai_digest/host_interface.md`. Every
driver has them, the SDL2, raw and frame buffer drivers answer that they can
not, format 0.

The driver that can is `src/host/gui_sdl3.cpp`, built with `make gui GUI=sdl3`.
It is the GUI on SDL3, with SDL's GPU renderer for the 2D drawing, and a
shader is drawn with SDL's GPU interface on the same device, into a texture
the renderer then blits. `make gui` builds the SDL2 driver again.

SDL2 and SDL3 share the names of their calls, so one program can not link
both, and the sdl3 GUI driver comes with an AUDIO driver of its own,
`src/host/audio_sdl3.cpp`. SDL3 gives it the device and reads a wav file, the
mixing is done by `src/host/mixer.h`, 32 voices, each with its pan, so there is no
mixer library to depend on.

The surface demo has a CPU and a GPU button, and comes up on the GPU if the
driver can draw a shader. On a Mac that is 60 frames a second, the rate of
its timer. If the driver has not built the shader within half a second the
CPU starts drawing frames, and the GPU takes over when the shader is built,
which on a Raspberry Pi 4 the first time is 18 seconds on. On a driver that
can not, the SDL2 one say, it comes up on the CPU, and the GPU button says
the driver can not.

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

## The VP Back End

`(shader-vp program)` gives a native function for the program.

```lisp
(defq native (shader-vp program)
	frame (shader-vp-frame program native '((time 2.0) (resolution (640.0 480.0))))
	pixels (shader-vp-argb native frame 0 0 640 480 480))
```

`(shader-vp-frame program native [vals])` gives the block of memory the native
function works in, with the values of the inputs written at the start of it,
from the same `(name value)` pairs the other back ends take.

`(shader-vp-argb native frame x y x1 y1 [height])` shades a tile and gives the
pixels as a string of 32 bit argb, clamped, which is what `(. canvas :tile)`
takes. If the height of the frame is given then row 0 is the top row, as a
canvas has it, and the shader still sees y going up.
`(shader-vp-pixels native frame x y x1 y1)` gives the pixels as a list of
`reals`, as the CPU back end does.

The back end writes VP source, the same assembler source the rest of the
system is written in, and the assembler turns it into code for the CPU of the
node, ARM64, x86_64, RISC-V, or the VP64 of the emulator. The source and the
function are kept under `obj/`, in `lib/gpu/jit/`, named by a hash of the
program. The first task to ask for a program assembles it, under a lock, and
every task on that machine after that just binds to it. The raymarch shader is
2,460 lines of VP, assembles in 14ms, and is 10KB of ARM64 code.

How the code is laid out.

* There is no recursion in the language, so every parameter and local of
  every function is given a slot of its own in one frame, 8 bytes for each
  float or int. The frame is the string the caller gives, so none of it is on
  the task's stack. The frame for the raymarch shader is 2,056 bytes.
* A function is a label, and a call is a `vp-call`. The args are stored to the
  parameter slots of the callee, and the result is read back from its result
  slot.
* A value being worked on is held in registers, a float in each of `:f0` to
  `:f15`, an int or bool in `:r2` to `:r10`. A `vec3` is three registers. Any
  that are live over a call are saved on the stack around it.
* `:r13` holds the frame and `:r12` the constants. A float literal is a
  constant in the function's table.
* A `(break)` is a jump, a `(return)` is a store and a jump, a test is a
  branch on the float or int compare. `and` and `or` stop at the first arg
  that settles them.
* `sin` and `cos` call `:sys_math :r_sin`. `pow` is a subroutine in the
  function, with the same method as the CPU back end, so the two agree.
  `floor`, `fract` and `mod` are built from a convert to int and back.

It has one limit the others do not. An expression that holds more than 16
floats at once, counting each component, has no register for the next, and is
refused with an error. Split it with a `(defq)`. The raymarch shader fits
as it is written.

Its maths is IEEE doubles, done by the CPU, so a divide by zero gives an
infinity as it does on a GPU.

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

The VP back end and the CPU back end both work in doubles, with the same
method for each built in. Over the 3,072 pixels of the first row of the table
they give the same values to all 6 decimal places that were compared. Every
test of a pixel in the test module is run through both.

The test module, 157 tests, passes on ARM64 macOS, ARM64 Linux on a Pi 4,
x86_64 macOS, RISC-V 64 and LoongArch 64 Linux under QEMU, and the VP64
emulator, so the VP back end has made working code through all five
translators.

## How Fast

The raymarch shader, default settings, time 2.0, one core, the whole frame in
one call.

| Machine | CPU back end, 64 by 48 | VP back end, 64 by 48 | VP back end, 320 by 240 |
|---|---|---|---|
| Apple M4 Max | 322ms | 8.7ms | 219ms |
| MacBook Pro 2018, i9-8950HK | 530ms | 14.4ms | 377ms |
| Raspberry Pi 4 | 3,103ms | 36.9ms | 913ms |

Native code is 37 times as fast as the interpreter on the two Macs, and 84
times on the Pi. On the M4 one core shades about 350,000 pixels a second.

At 640 by 480 on one core of the M4, by what the controls ask for.

| Settings | Frame |
|---|---|
| depth 0, no occlusion | 413ms |
| defaults, depth 1 | 860ms |
| bump 0.005 | 1,053ms |
| depth 2 | 1,264ms |
| displace 0.02 | 2,299ms |
| anti alias, 4 samples a pixel | 3,392ms |

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

`apps/demos/surface` is the raymarch shader on screen, on the GPU, or with no
GPU. This is how it draws with none, the CPU button.

The app reads the shader, and makes a slider for each input that has a range,
with its name and value. It knows nothing else of the shader, a new input in
the shader file is a new slider. It gives the two inputs that have no range,
`time` and `resolution`, itself.

Each frame it packs the inputs block and cuts the frame into tiles of 8 rows.
A farm of child tasks, one for each node, shades them. A child gets the native
function for the shader from the VP back end when it starts, and for each tile
unpacks the block into a frame, shades, and sends back the pixels. When the
last tile is in the frame is shown and the next one starts, with whatever the
sliders say by then.

On an Apple M4 Max, 16 nodes, a 640 by 480 frame at the default settings takes
from 76 to 96ms, 10 to 13 frames a second, with no GPU. The same demo on the
interpreted CPU back end took about 550ms for a 320 by 240 frame. Reading and
checking the shader, 205 lines, takes about a millisecond.

The app is in the Demos list of the launcher, as surface.

## Limits

* On the CPU back end a divide by zero is an error on an error checked
  build, where a GPU, and the VP back end, give an infinity. The raymarch
  shader at time 0.0 has the camera at the point it looks at. The CPU back end
  fails there, the other two give a black frame.
* The VP back end has no register spill, an expression that needs more than
  16 float registers is refused.
* The native functions are never removed from `obj/`, one is left for each
  version of each shader that has been run.
* Tasks that ask for the same native function at the same moment are held
  apart by the lock service, as `(jit)` is. The login app, the TUI and the
  test suite start it. With no `@Lock` service running, 16 children starting
  together from a cold cache read each other's half written files.
* A tile is shaded in one call with no task switch, so keep tiles small.
* The VP code is plain scalar code. Nothing is kept in a register between
  statements, no common terms are shared, and there is no SIMD.

## What Is Not Here Yet

* The sdl3 GUI driver has been run on Macs and on a Raspberry Pi 4 with no
  desktop, SDL on the bare display. It has not been run on a Linux desktop,
  X11 or Wayland. On Windows, where SDL gives it Vulkan, it has been run by
  Martyn Blyss, and the surface demo draws on the GPU there.
* Raylib is the fall back if SDL3 will not do for a host.
* The GLSL back end does not guard names against the reserved words of GLSL.
* Compute, and rendering as a service for a node with no GPU, are deferred.
* Vertex shaders, meshes, textures as inputs, and compute.
