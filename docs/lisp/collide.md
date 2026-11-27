# Collision and Space

The collision library is `lib/physics/collide.inc`. It keeps the bodies of a
world in a grid of space, and answers what is in a region, what a way meets
first, and which bodies touch, which have begun touching and which have
stopped.

It is one of the physics utilities of `lib/physics/`, each of use on its
own: this finds what touches, and `lib/physics/move.inc`,
`docs/lisp/move.md`, answers it, moving each thing. An engine made of them
may come one day; none of them needs it.

It holds only what that takes. A body is an id, a centre, a bound and a turn,
and a mask of what it may meet. What a body is, how it moves and what a touch
does are the app's. The library does the hard and regular work, the same in
every app, and the app scripts what happens.

## Bodies

```lisp
(import "lib/physics/collide.inc")
(defq world (Collide +double_4)
	rock (. world :add (doubles +double_0 +double_0 +double_0) (list :sphere +double_1))
	crate (. world :add (doubles +double_3 +double_0 +double_0) (list :box +double_1 +double_1 +double_1)))
(. world :move crate (doubles +double_2 +double_0 +double_0))
(length (. world :pairs))
```

*	`(Collide cell [wrap])`, a world whose grid has cells so big, and, for a
	world that goes on round, how far round it is each way, a vec3, 0 a way
	it does not wrap.

*	`(. world :add centre bound [mask sensor turn])`, a body, its id. A bound
	is a ball, `(:sphere r)`, or a box, `(:box hx hy hz)` its half sizes. Two
	bodies may meet if their masks share a bit, 1 if not said.

*	`(. world :move id centre)`, `(. world :turn id turn)`, a box turned by a
	quaternion, `lib/math/quaternion.inc`, or square to the world, `:nil`,
	and `(. world :reshape id bound)`.

*	`(. world :remove id)`, its id used again for the next body.

The library keeps each body's box square to the world, made again only as
it moves, turns or changes. `(. world :aabb id)` gives its least and most
corners, for the app's own use: what is on screen, a label, a grid of its
own.

## Owners and Sensors

A thing of the app's can have many bodies. `(. world :attach owner centre
offset bound [mask sensor turn])` adds one to an owner, at an offset from
its centre. `(. world :move_owner owner centre [turn])` moves and turns all
of them together, each offset turned with the owner. Bodies of one owner
never meet each other. `(. world :aabb_owner owner)` is the box all of them
are in, and `(. world :detach owner)` removes them all.

So a ship has a tight body to collide with and a wide one for what is near
it, and a long thing is a few boxes along it.

What the world gives back, from `:near`, `:within`, `:cast` and `:pairs`, is
bodies, by their ids, not owners: an owner's id is not its body's, as soon
as any owner has two. `(elem-get (get :owners world) id)` is a body's owner.

A sensor is a body that says what is in it and never pushes. Two sensors do
not meet, and a ray goes through one. A sensor's touches that begin and end
are things come into it and gone out of it: a buoy's zone, a trigger, the
range of a sound.

## Asking

*	`(. world :within region centre [mask sensors turn])`, the bodies a
	region meets, a bound at a place, each `(id depth way all)`: how deep,
	the way from the place to it, and is it all in the region. Sensors only
	if asked. What a buoy has near it, what a blast reaches.

*	`(. world :near centre r [mask])`, `:within` a ball.

*	`(. world :cast from way far [r mask])`, what a ray, or a ball of r swept
	along it, meets first, `(id t)`, or `:nil`. The way is of length 1. The
	sonar's ping, a line of sight, whether a way is clear to steer.

*	`(. world :pairs)`, every pair that touches, once, `(a b depth way)`,
	the way from a to b.

*	`(. world :touches)`, the pairs as `:pairs`, as `(began held ended)`:
	since it was last asked, those that began touching, those that still
	do, and those that stopped, each `(a b)`. A thud the once as a thing
	hits, a buoy turning colour as a thing comes near.

## How It Is Fast

A body small for a cell, reaching no more than half of one, is in the cell
of its centre, and meets only those in the cells round it. A bigger one is
on a list of its own and is met with all. The grid is a map of cells by
their place, so it has no edge, and only cells with bodies in them exist.

The pairs are found by a native, `(collide-box-near centres sizes wrap
body)`: each body due, the boxes square to the world of all the bodies that
meet its, one after another, round the world where it wraps. In VP that is
quicker than looking in the cells of a grid, for a world of hundreds of
bodies; the library keeps each body's half sizes flat for it, and takes the
centres as they are each call, so a body moved in place is seen at once. A
region and a ray still look in the grid, so after moving bodies in place an
app calls `(. world :refile)`.

A pair whose boxes meet is tested by its bounds: balls by their distance, a
ball and a box in the box's own frame, boxes by the fifteen ways that could
part them.

## Exact Shapes

A body's bound holds all of its shape. Where the bound is not near enough,
the app gives the body a narrow test, a plugin, `(. world :narrow id fn
[shape])`, `(fn world a b) -> :t | :nil`, and what it keeps for it, read
back by `(. world :shape id)`. Only pairs whose bounds meet are asked, and
the test says only yes or no: the depth and way are the bounds'. How exact
and how fast is the app's choice for its own shapes.

## Tasks and Speed

The library knows nothing of tasks, and its bodies are data. The work that
is heavy, the pairs of the cells, the regions, the rays, is in a few places
here, so it can be made VP natives later without an app changing.
