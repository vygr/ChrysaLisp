# Moving Things

The library is `lib/physics/move.inc`. It is the one way things move: rocks,
buoys, a ship, missiles, fish, craft. A body has a place and a velocity, a
kick and a shove still to take up and, if it tumbles, a spin and a turn,
each with its own numbers, as a Nav has its own. Kicks, shoves and hits
change it, and a step moves every body.

It is not a physics engine: no forces are summed and there is no solver.
What steers a thing is the app's or a Nav's; how it then moves, takes a knock
and is kept out of what it meets is the library's.

It is one of the physics utilities of `lib/physics/`, each of use on its
own: `lib/physics/collide.inc`, `docs/lisp/collide.md`, finds what touches,
and this answers it. An engine made of them may come one day; none of them
needs it.

## Bodies

```lisp
(import "lib/physics/move.inc")
(defq mover (Mover (doubles (n2d 100) (n2d 100) (n2d 100)))
	rock (. mover :add (doubles (n2d 10) (n2d 10) (n2d 10)) (doubles (n2d 2) +double_0 +double_0)))
(. mover :set rock :mass (n2d 8) :drag +double_1/10)
(. mover :impulse rock (doubles +double_0 (n2d 8) +double_0))
(. mover :step (/ +double_1 (n2d 60)))
(. mover :velocity rock)
```

*	`(Mover [wrap])`, bodies in a box that goes on every way, `wrap` its size
	each way, or in all of space.

*	`(. mover :add place [vel spin shove])`, a body, its place, velocity,
	spin and shove the app's own doubles, or new, so what reads them reads
	them moved. A spin is `(wx wy wz qw qx qy qz)`, how fast it turns about
	which way and how it is turned. Its id is given back, a removed one's
	used again.

*	`(. mover :remove id)`, a body gone; none follows it after.

*	`(. mover :place id)` `(. mover :velocity id)` `(. mover :mass id)`.

## Its Numbers

`(. mover :set id key val ...)` sets a body's own:

| Key | What it is | Not said |
|---|---|---|
| `:mass` | how heavy; a kick moves it by its mass | 1 |
| `:drag` | its damping, a share a second of how much faster it goes than what it is in | 0 |
| `:flow` | the velocity of what it is in, a river's, a current's; else the mover's `:flow_fn` of its place, else still | `:nil` |
| `:accel` | a steady acceleration, gravity, a thrust held | `:nil` |
| `:top` | its most speed, 0 none | 0 |
| `:take` | how fast it takes up a kick, a share a second, 0 at once | 0 |
| `:rate` | how fast a shove eases in, a share a second | 10 |
| `:bounce` `:grind` `:grip` | how it meets another: bounce, the slide it loses, how hard that slide spins it | 0.1 0 0 |
| `:spin_top` | its most spin, 0 none | 0 |
| `:align` | how fast its velocity turns to the way it faces, `:face`, its speed kept | 0 |
| `:follows` `:follow_rate` | a body whose velocity it has, and how fast it comes to it, 0 at once | -1 0 |
| `:kinematic` | moved by the app alone: kicks and hits do not move it, but it moves what it meets as if too heavy to move | `:nil` |

So drag with a flow is a river that carries what is in it along, a strong
drag swept with it, a weak one drifting slowly into it; and a body's own
flow is the speed it would go, eased to as fast as its drag, as a sub's
thrust is in Water.

`(def mover :flow_fn (lambda (place) ...))` gives the velocity of what is
there, or `:nil`, a current that changes from place to place.

## Changing How It Goes

*	`(. mover :impulse id dv)`, a kick, `dv` a vector, its way and how hard,
	the velocity changed by it over the body's mass: a missile, a blast.

*	`(. mover :push id dv [k])`, its velocity changed by `k` of `dv`, whatever
	its mass: a pull, gravity between two, a blast worked out by the app.
	A body with `:take` takes a kick or a push up over its steps.

*	`(. mover :shove id d [as_now])`, its place to change by `d`, eased in at
	its `:rate`, never a jump. With `as_now`, `d` is all it is still to
	change, a correction measured afresh each step, so it does not add up
	while it is easing in.

*	`(. mover :spin id dw)`, its spin made faster about a way, no faster than
	its `:spin_top`.

*	`(. mover :face id way)`, the way it faces, its velocity turned to it as
	fast as its `:align`.

*	`(. mover :hit a b way depth ra rb [ease])`, two bodies meeting, `way` of
	length 1 from a to b, `depth` how far one is in the other, `ra` and `rb`
	how far each reaches to where they meet. If coming together they are
	bounced apart, the lighter the more, the slide across taken down by
	their grind and spinning each by their grip; and shoved apart, the
	lighter the more, `ease` of it if not all. How fast they were closing,
	under 0, or 0. A collide world's touches, `lib/physics`, go straight to
	it, a body's owner its body here.

*	`(. mover :step dt)`, every body on: its kick taken up, its velocity come
	to its leader's, turned to where it faces, its acceleration, damped
	toward what it is in, no faster than its top; its place on by it and by
	its shove, kept in the box; turned as it tumbles.

## Uses

| A thing | How |
|---|---|
| a rock | `:mass` its size cubed, `:spin_top` by its size, `:hit` from the world's touches, `:push` for pulls and blasts |
| a buoy | `:follows` its cluster's big rock, `:shove` out of a rock it is in |
| a ship | `:drag`, `:accel` while thrusting, `:align` to where it faces, `:hit` with what it meets |
| a sub | its own `:flow`, the speed it would go, `:drag` how fast it comes to it, `:align`, `:shove` off the rock |
| a missile | added as fired, removed as it goes off |
| a steered thing, a visitor, a fish, a craft | its velocity set each step to where its steering would have it, `:hit` or `:shove` off what it meets |
| the player moved by the app | `:kinematic` |

## Speed

A step is one loop over all the bodies, their places, velocities and
shoves in lists side by side and their numbers in arrays, work skipped for
numbers left as they were. Space moves its 450 rocks, its buoys, ship,
missiles and visitor in under a millisecond a frame. It is laid out for a
native step in VP to take it over.
