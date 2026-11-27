# Navigation, Routes and Following

The navigation library is in `lib/nav/`. It is for anything that goes
somewhere: a player, a fish, a craft, a tour guide, a submarine in a tunnel.
It is told where a thing is, which way it faces, where it is to go and by
where, and it finds a route and keeps to it, turning as a thing of weight
does.

It has three parts, each usable alone:

*	`lib/nav/graph.inc`, a graph of places and links, and the route of least
	cost between two of them.

*	`lib/nav/path.inc`, paths, lists of places, made even and smooth, and how
	far along one a thing is.

*	`lib/nav/nav.inc`, the `Nav` class, which routes and follows, its parts
	that differ from one use to another given to it as functions, plugins.

## Graphs

A graph is plain data, `(places links)`: places a list of vec3 doubles, a
node its index in that list, and links a list of a `nums` for each node, the
nodes it links to. It goes in a message as it is.

```lisp
(import "lib/nav/graph.inc")
(defq g (graph)
	a (graph-node g (doubles +double_0 +double_0 +double_0))
	b (graph-node g (doubles +double_10 +double_0 +double_0)))
(graph-link g a b)
(graph-route g a b)
```

`(graph-route g from to [cost_fn])` finds the route of least cost by A*, its
nodes from first to last, or `:nil` if there is none. The cost of a link is
asked of `(cost_fn g a b)` as the search goes, so a link can cost more now,
or be shut by answering `:nil`. Without one the cost is the distance. The
search guesses the cost on to the end by the straight distance, so a cost
never less than the distance finds the least route.

`(graph-nearest g at [ok_fn])` gives the node nearest a place, of those
`(ok_fn g node)` allows.

## Paths

A path is a list of places, vec3 doubles, one after another.

*	`(nav-path-resample pts step)`, places along it no further apart than step.

*	`(nav-path-smooth pts [times])`, each place the mean of itself twice and its
	neighbours, the ends kept.

*	`(nav-path-on pts at cursor look)`, how far along it a thing at a place is, a
	cursor that never goes back: of the next `look` places, the one it is
	past.

*	`(nav-path-past pts cursor at)` and `(nav-path-ahead pts cursor far)`, how far
	past its cursor place it is, and the place so far on from there. Together
	they give an aim that slides on as the thing goes, not one that jumps a
	place at a time.

## The Nav Class

```file
lib/nav/nav.inc
```

`(Nav at way [up])` makes one at a place facing a way. Its state is its
place `:at`, its turn `:turn`, a quaternion, with `:way` and `:up` from it,
its turn rate `:spin`, and `:speed`.

*	`(. nav :go to [via])`, off to a place, through the places of via, its
	route asked for now.

*	`(. nav :routed path [more])`, the path it is to follow, given when it
	comes, or with more added to the end of the one it has.

*	`(. nav :step dt)`, a step of dt seconds.

*	`(. nav :matrix)`, its turn as a matrix for a model.

*	`(. nav :state [state])`, its state as data, to keep or to send, or set
	again from it.

### A Step

Each step it asks its `:aim_fn` what to steer at, and its `:avoid_fn` to bend
that round what is in the way. It turns toward that, with the up its `:up_fn`
wants, as its `:steer` says:

*	`:spring`, how fast it turns sprung toward the turn still to go, stiffness
	`:k`, damping `:zeta`: 1 just enough never to go past, more slower to
	get there, less swinging past.

*	`:lerp`, a share of the way each step, `:rate` a second.

Either no faster than `:turn_most` radians a second. Its speed is sprung to
what its `:speed_fn` wants, and it moves on, its `:move_fn` having the last
say on where it goes. Then it checks if it is there, and if it is stuck.

### Following Without Swinging

A thing that turns as a thing of weight does takes a while for its turn to
catch up with its aim. Aiming at a place on its path a little ahead, as it
is, it swings across its path and back, the see-saw of a pilot. So the
follower aims from where it will be when its turn has caught up, `:predict`
times the time its turn takes, 1/`:k` or 1/`:rate`, and steers along the
way from there to its aim. That damps the swing: from 3 units off a straight
path it settles onto it without going past. Near the end of a path it heads
for the end itself.

## Plugins

Each plugin is a property holding a function. It takes the Nav and reads
what it needs of its properties, so it works on any task that imports it.

| Property | Called | Gives | Given here |
|---|---|---|---|
| `:route_fn` | `(nav from to via)` | a path, or `:nil` for later | `nav-route-line`, `nav-route-graph` |
| `:refine_fn` | `(nav path)` | the path made fit, or `:nil` for later | |
| `:next_fn` | `(nav)` | where to go on arriving, or `:nil` | `nav-next-tour`, `nav-next-wander` |
| `:aim_fn` | `(nav dt)` | what to steer at, or `:nil` | `nav-aim-follow`, `nav-aim-seek`, `nav-aim-pointer` |
| `:avoid_fn` | `(nav aim)` | the aim bent round what is in the way | `nav-avoid-field`, `nav-avoid-cone` |
| `:speed_fn` | `(nav dt)` | how fast it would go | `nav-speed-cruise` |
| `:up_fn` | `(nav way dt)` | which way it would have up | `nav-up-level`, `nav-up-lean`, `nav-up-free` |
| `:move_fn` | `(nav to dt)` | where it goes | `nav-move-field` |
| `:arrive_fn` | `(nav)` | told it is there | |
| `:stuck_fn` | `(nav)` | told it is stuck | |

The defaults are a straight route, following it, its cruise speed, and level
of the world's up, but keeping its own up near straight up or down, where the
world's says nothing of roll.

### Avoiding

`nav-avoid-field` takes `:field_fn`, of a place how clear of what is in the
way it is, a distance field. A few places on the way to the aim not `:clear`
are moved up the field's slope till they are, and the first aimed at, so it
steers round or over. `nav-move-field` keeps the thing itself `:body` clear.

`nav-avoid-cone` takes `:clear_fn`, of the Nav and a way how clear going that
way is. If the way to the aim is not `:clear`, it tries ways round it, as far
as `:cone`, in `:cone_rings` rings of `:cone_count`, and takes the one that
clears, turning the least and keeping to the side it went round last,
`:cone_keep`, so it does not flip from side to side.

## Uses

| A thing | Its plugins |
|---|---|
| a player | `nav-aim-pointer`, `nav-up-lean`, its own `:move_fn` for collisions |
| a fish, a craft | `nav-aim-seek` or a flocking aim of its own, `nav-avoid-field`, `nav-next-wander` |
| map routing | `nav-route-graph` with a `:cost_fn`, `nav-aim-follow` |
| a tunnel | `nav-route-graph`, a `:refine_fn` to the middle of the tunnel, heavy damping |
| a tour guide | `nav-next-tour` and a route between its stops |

## Tasks

The library knows nothing of tasks. A route can come later: a `:route_fn` or
`:refine_fn` answers `:nil`, the app sends the work to a child, and gives the
answer with `(. nav :routed path)`. Meanwhile the Nav heads for its goal. A
Nav's state is data, `(:state)`, and a graph is data, so either goes in a
message; functions do not, so a child imports the same plugins.
