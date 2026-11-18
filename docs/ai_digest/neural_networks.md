# Neural Networks

`lib/nn/` is a neural network made from a description of its layers, trained
by Lisp on a farm of nodes or by kernels of the shader language on the GPU.
The Digits app, `apps/science/digits/`, learns MNIST's handwritten digits with it,
and shows the network, the loss and the confusion as it does.

The network was written in Lisp first, a row of weights a vector at a time,
with the end in view: each piece of it is what a GPU kernel does later, a row
a thread. Then each place the time went was measured, and a VP vector
operation or a kernel made for it. This is about those operations and those
kernels, what each does to the sums and why it was the one chosen.

## The files

* `lib/nn/model.inc`, a model from a description, its forward and backward.
* `lib/nn/lisp.vp`, `nn-scale-bytes`, a picture's bytes into doubles.
* `lib/nn/gpu.inc`, a model trained on the GPU.
* `lib/nn/shaders/`, its five kernels, `gather`, `forward`, `loss`, `back`
  and `update`.
* `lib/nn/view.inc`, the picture of a network, on `lib/motion`.
* `apps/science/digits/`, the app, `child.lisp` a child of its farm and
  `gpu.lisp` the task that drives the GPU.
* `tests/math/test_nn.lisp` and `tests/gpu/test_nn_gpu.lisp`: the
  gradients against the slope of the loss, and the GPU against the Lisp.

## A model from a description

```vdu
(import "lib/nn/model.inc")

(defq model (nn-model '((:input 784) (:dense 128 :relu) (:dense 10 :softmax)))
	params (nn-init model))
```

The model says where each layer's numbers are in one vector of doubles, the
params, 101,770 of them for MNIST: a layer's weights a row for each unit, a
weight for each unit of the layer before, then its biases. One vector,
because one vector is what goes into a shared buffer, a file or the GPU
whole. `nn-init` spreads each layer's weights by how many feed a unit, He's
uniform, so a deep layer starts neither dead nor saturated. `nn-net` gives the
rows as vectors of their own, the shape the sums want.

`nn-model` holds a description to what there is, and throws for one that is
not: a kind of layer or an activation that is none, a filter or a block
bigger than its planes, and softmax anywhere but the last layer, which must
have it. The loss is the cross entropy of chances, on the CPU and on the
GPU, so a model whose last layer gives none could be run forward and not
taught; when there is a second loss the description will say which.

## The sums

A dense layer takes its inputs x to its outputs a:

```code
z[j] = b[j] + sum over k of W[j][k] x[k]
a[j] = act(z[j])
```

with ReLU, sigmoid or tanh for act, and for the last layer softmax, the
outputs as chances that add up to 1. The biggest score is taken from all of
them before the exp, so none overflows.

Learning is the error at the outputs taken back. With softmax and the cross
entropy, the loss of a sample of label y is `-log p[y]`, and its error at the
outputs is as plain as can be:

```code
d[j] = p[j] - (1 if j is y else 0)
```

Back through a layer, the gradient of each weight is the error at its output
times its input, of each bias the error, and the error at the layer before is
its weights' columns weighed by the error, times the slope of that layer's
activation:

```code
dW[j][k] += d[j] x[k]
db[j]    += d[j]
d'[k]     = slope(a'[k]) * sum over j of d[j] W[j][k]
```

The gradients are summed over a batch, and the params moved down them, by
the rate over the batch. `test_nn` checks each gradient against the slope of
the loss, worked out by moving each weight a little.

## The vector operations

Each of these is a native method of the vector classes. They were made where
the measuring said the time went, not before.

### A dot by four running sums

```vdu
(nums-dot row x)
```

The forward pass is dots: 128 of 784 for each sample at the first layer. The
dot added each product into one register, so each add waited for the one
before it, about 3 cycles: a dot of 784 took 453ns, 0.6ns an element, though
a pass that only maps, a scale say, took 0.28ns. The work was not the
products, it was the chain.

`:doubles :dot` now keeps four running sums over blocks of four, the rest
into the first, and adds the four at the end. No add waits on the one before
it, and a dot of 784 takes 155ns. `nums-sum` is the same, 403ns to 110ns.
Only the reductions change: a map, an add or a scale of vectors, has no chain,
each element its own, and runs as fast as it loads already. The adds are in
another order, so a sum may differ in its last bit or two.

A sample forward went from 73us to 37us.

### A scaled add in one pass

```vdu
(nums-add-scaled g x e g)	;g += e x
```

Backward is, for each unit, its weight gradient `g += d[j] x` and the error
back `d' += d[j] W[j]`. Each was `(nums-add g (nums-scale x e tmp) g)`: two
passes over 784, and a vector between them for the scaled one. 
`nums-add-scaled` is the two in one pass, the first vector plus the second
times a scale, of nums, fixeds, doubles or floats, the `:add_scaled` virtual
of `:nums`.

It is not a fused multiply add. An FMA is element by element, `a[i] b[i] +
c[i]`, and rounds once; this has one scale for the whole vector, and is a
multiply then an add, rounded twice, the same result as the scale then the
add. What it saves is the memory: 411ns for 784 as two passes, 213ns as one,
and no vector between. A unit whose error is 0 is passed by, which with ReLU
is about half of them.

A sample forward and back went from 81us to 64us.

### Bytes into doubles, and vectors into a buffer whole

```vdu
(nn-scale-bytes pixels (/ (n2d 1) (n2d 255)) x)
(buf-set params_buf 0 (* +digits_params +long_size) params)
```

A picture is 784 bytes, 0 to 255, and the network wants doubles 0 to 1.
`nn-scale-bytes` does the 784 in one native loop, 0.24us where the Lisp loop
took 47us. And `buf-set` and `obj-set` take a vector of numbers whole, its
elements' bytes in one native copy, where the params had gone in a
`set-double` at a time: 5.6ms to 23us each step.

### Floats

```vdu
(nums-convert params (floats))		;doubles to floats
(nums-convert from_gpu (doubles))	;and back
```

A GPU works in 32 bit floats. `:float` is a number of 32 bits beside
`:double`, made by `n2f`, its arithmetic and compares in 32 bits as a GPU's
are, its 32 bits kept in the low half of its value with the upper half clear,
so the same float is always the same value on every CPU. `:floats` is a
vector of them beside `:doubles`, packed 4 bytes an element, as a GPU buffer
has them, half the memory of doubles, and `gpu-write` and `gpu-read` take one
as they are. `nums-convert` makes a vector of any kind of number from any
other, the `:convert` virtual of `:nums`, each class filling its own kind
from whichever kind the source is.

The params had gone up to the GPU a float at a time, the bits of each worked
out in Lisp, about 50ms each way. By `nums-convert` and a `:floats` they go up
in 6ms and come back in 2ms.

## On the CPU, a farm

The app's farm is 16 children on the nodes of the machine. The data set, 55MB
of pictures and labels, the params and the gradients are each a `:buf`, a
block of memory shared by the nodes, so none of them goes by mail. Each step
the app writes the params into their buf, each child works its part of the
batch through the model and sums its gradients into its own slice of the
gradient buf, and the app adds the slices up and moves the params down them.
An epoch of 55,000 took 4.1s at first, 1.46s once the vectors went into the
bufs whole, 1.15s with the dot of four sums and 0.90s with the scaled add.

## On the GPU, five kernels

`lib/nn/gpu.inc` trains a model on the GPU with five kernels of the shader
language. The params stay there, in floats; the data set is put there once,
as the bytes it is, a quarter of what it would be as floats. A step is the
five, for each layer as it needs, given to the GPU's one queue with no wait
between them: a GPU call has a wait of about 0.75ms however small the work,
so the chain is waited for once, at its end, by a read of what it worked
out, `shader-gpu-dispatch` and `gpu-take` beneath it.

Each kernel is one thread for each number it works out, and no more than
that: no tiles, no memory shared between threads. That is the simplest form
that is right, the same in GLSL, MSL, SPIR-V and the VP back end, and it is
already far the fastest way to do these sums.

### gather

```vdu
(defun main :void ((i :int))
	(defq s (/ i ins) k (- i (* s ins)) n (elem-get idx s) at (+ inputs (* n ins) k))
	(elem-set x i (/ (float (logand (>> (elem-get data (>> at 2)) (* (logand at 3) 8)) 255)) 255.0))
	...
```

The batch picked out of the data set: a thread for each input of each sample,
its byte found by the sample's index and made a float 0 to 1, and the label
of each sample. All that goes up each step is the indices of the batch, 2KB,
not the 1.6MB of its pictures as floats.

### forward

```vdu
(defun main :void ((i :int))
	(defq s (/ i outs) j (- i (* s outs)) xr (* s ins) wr (* j ins) acc (elem-get b j))
	(for (k 0 max_ins)
		(when (>= k ins) (break))
		(setq acc (+ acc (* (elem-get w (+ wr k)) (elem-get x (+ xr k))))))
	(cond
		((= act 1) (when (< acc 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (/ 1.0 (+ 1.0 (exp (- acc))))))
		((= act 3) (setq acc (- (/ 2.0 (+ 1.0 (exp (* -2.0 acc)))) 1.0))))
	(elem-set a i acc))
```

A dense layer: a thread for each output of each sample, its dot of the row
and the inputs and its activation, 65,536 threads for the first layer at a
batch of 512. The loop is to a constant limit, as a shader's must be, and
leaves at the layer's real width. The first layer forward, a sample at a
time, was 65us in Lisp, 98us as the same dense kernel on the VP back end,
and 2.6us on the GPU at a batch of 512, as `evidence_not_faith.md` has it.

### loss

A thread for each sample: its outputs as chances, the biggest score taken
from all first, its loss, its error at the outputs, `p - 1` at its label and
`p` elsewhere, and the output it takes the sample for. Its outputs are few,
10, so one thread does them all.

### back

```vdu
(defun main :void ((i :int))
	(defq s (/ i ins) k (- i (* s ins)) zr (* s outs) acc 0.0 v (elem-get a i))
	(for (j 0 max_outs)
		(when (>= j outs) (break))
		(setq acc (+ acc (* (elem-get dz (+ zr j)) (elem-get w (+ (* j ins) k))))))
	(cond
		((= act 1) (when (<= v 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (* acc v (- 1.0 v))))
		((= act 3) (setq acc (* acc (- 1.0 (* v v))))))
	(elem-set dx i acc))
```

The error taken back through a layer: a thread for each input of each sample,
a column of the weights weighed by the error, times the slope of the layer
before's activation, worked out from its outputs. The first layer needs no
error back, its inputs are the picture.

### update

```vdu
(defun main :void ((i :int))
	(defq nw (* outs ins) g 0.0)
	(if (< i nw)
		(progn
			(defq j (/ i ins) k (- i (* j ins)))
			(for (s 0 max_batch)
				(when (>= s batch) (break))
				(setq g (+ g (* (elem-get dz (+ (* s outs) j)) (elem-get x (+ (* s ins) k))))))
			(elem-set w1 i (- (elem-get w0 i) (* step g))))
		...
```

A thread for each weight and each bias: its gradient summed over the batch,
and the params moved down it, in one. There is no buffer of gradients, each
is worked out and used where it is. And as a kernel writes no buffer it
reads, each layer's weights and biases are two buffers, the step's read and
the next's written, swapped each step.

### The GPU's own task

The kernels are driven by `gpu.lisp`, a task on another node, so the frames
the window draws hold none of it up. The app gives it its steps 8 ahead, so
the GPU never waits on the window; the params come back every 2 seconds for
the picture, and at each epoch's check to be kept if they are the best. An
epoch on the GPU takes about 300ms, the farm of 16 nodes 0.90s, and a run to
its early stop 15 epochs, about 8 seconds, 97.0% of the 10,000 test
digits right.

## What is not done

* Tiles and memory shared between the threads of a kernel, the next step for
  big layers, where each weight is read by every sample's thread.
* The VP back end of the shader language keeping a kernel's loop in
  registers, what makes it 98us where Lisp is 65us.
* Half floats, 16 bits, for the weights and data on the GPU, where it has
  them, half the memory read again.
* Saving a trained model, and reading one in a format others write.
* The farm across machines, the data set and params by mail between them.
