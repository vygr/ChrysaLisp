# Motion

`lib/motion/` is a 2D scene graph of vector figures that look after themselves.
It is for motion graphics, vector games and interactive lessons. The Motion
demo, `apps/demos/motion/`, is a story and a playground made with it, and
`tests/math/test_motion.lisp` runs them with no screen.

There is no timeline and no script of the whole. Each figure has its own small
rules, and what happens next comes of a rule ending or of two figures meeting.
A presentation is that with a story in it, a game is that with a player in it.

## The files

* `lib/motion/ease.inc`, how a change goes over its time.
* `lib/motion/morph.inc`, outlines from an SVG path or words, and one into another.
* `lib/motion/figure.inc`, a node of the tree, `Figure`.
* `lib/motion/comps.inc`, the components that change a figure.
* `lib/motion/rostrum.inc`, the top of the tree and the camera, `Rostrum`: the
  tick, what meets what, and the drawing.

## A figure

```vdu
(import "lib/motion/rostrum.inc")

(defq stage (Rostrum 640 480) ball (Figure "ball"))
(. ball :set_outline (list (list :t (path-gen-ellipse 0.0 0.0 20.0 20.0 (path)))))
(def ball :fill 0xffe0a040 :x 320.0 :y 240.0)
(. ball :add (Tween :x 500.0 1.5 (const ease-back)) (Rate :rz 2.0))
(. stage :add_node ball)
```

and for each frame, by the seconds since the last:

```vdu
(. stage :tick dt)
(. canvas :fill +argb_black)
(. stage :draw canvas view)
(. canvas :swap +swap_write)
```

`view` is the 3x2 matrix from the picture to the canvas, `:nil` if they are the
same size.

A figure's attributes are properties. Anything can read them with `(get)`, and
set them with `(def)`, with `:set_outline`, `:set_width` and `:set_trim` for
those a kept flattening depends on.

| Attribute | What it is |
|---|---|
| `:x :y :z` | Where its pivot is, in the space of its parent |
| `:rx :ry :rz` | Turned about each axis, in radians |
| `:sx :sy` | Its size |
| `:px :py` | Its pivot, in its own space |
| `:alpha` | How much it is seen, this and all under it |
| `:fill :stroke` | Colours, 0 for none |
| `:width :join :cap1 :cap2` | Of the stroke |
| `:trim` | How much of each line of the stroke is drawn, 0.0 to 1.0 |
| `:rule` | How it is filled, `+winding_odd_even` unless it is said |
| `:outline` | A list of `(closed path)`, as `(path-gen-paths)` gives |
| `:kinds :meets` | Bit masks, what it is and what it looks out for |
| `:on_enter :on_stay :on_leave` | `(fnc this other)`, see below |
| `:on_death` | `(fnc this)`, when it is taken out |

A group is a figure with figures under it, `(. parent :add_node child)`. The
tree has the method names of `Scene-node`, `lib/math/scene.inc`. A figure can
be moved to another parent at any moment, in its own turn too, to a group made
that moment or one to come. `(. figure :kill)` takes it out at the end of the
tick, and all under it.

## Components

Each has its turn each tick, a parent before its children. Any can fire a
routine when it is over, `(then figure comp)`, which can add components or
figures, or kill the figure.

| Component | What it does |
|---|---|
| `(Tween key to secs [ease then wait])` | An attribute to a value. A number by its value, `:fill` and `:stroke` by each channel, `:outline` by a morph |
| `(Wait secs [then])` | Nothing, then it is over |
| `(Rate key rate [accel secs then])` | So much a second, and that by so much a second |
| `(Wave key size period [phase secs then])` | Swings about what it was |
| `(Follow line secs [ease then wait])` | `:x :y` along a path |
| `(Tie other keys [offset scale])` | Attributes taken from another figure, while it is there |
| `(Cues comp ...)` | Each in turn, the cue list of the one figure |
| `(Custom fnc [then])` | `(fnc figure dt)` says `:t` to go on |

The eases are `ease-linear`, `ease-in` slow in and fast out, `ease-out` fast in
and slow out, `ease-in-out`, `ease-in3`, `ease-out3`, `ease-back`,
`ease-anticipate`, `ease-bounce` and `ease-elastic`. An ease is a function of
0.0 to 1.0, so any other is a function too.

## Meeting

A figure is met if it has `:kinds` or `:meets`, and looks out for those whose
kinds are in its meets. Each tick the boxes round them are swept along x, then
the boxes themselves are asked, then their polygons, by `(poly-hit?)`. A figure
with children has all of theirs as well as its own.

`:on_enter` is fired when two come to touch, `:on_stay` each tick they go on
touching, and `:on_leave` when they part, each for a figure that looks out for
the other. It is asked in the picture, as they look, not in 3D.

A figure whose colour has no alpha, `0x00ffffff`, draws nothing and is met, a
wall or a zone that fires when something enters it. A pointer is a small figure
the app keeps moving, so a touch is a meeting like any other.

## Pseudo 3D

Each figure is flat, with a place in space. A `Rostrum` with a focus has a
camera looking down z at the middle of the picture, and things further off are
smaller. `:by_depth` draws the furthest first, else it is the order of the tree,
and a figure added late is on top. Empty groups added first are layers: what
arrives late goes in the layer of its sort and stays under the layers above.

With no focus a tilted figure is only narrower. Whichever it is, a figure that
faces the camera, or any with no focus, is one matrix in native code,
`(path-transform)`. A figure tilted with a focus is a divide for each point in
Lisp, measured at about 17 times the cost for a line of big words.

## Morphing

`(Morph from to)` pairs the lines of two outlines, the nearest first and a hole
with a hole if there is one. Each pair is made the same number of points evenly
along its length and turned to start where the two lie closest. A line with no
partner grows out of a point or shrinks into one. That is done once, it is a few
ms for a line of words, and each frame after it is a sum over arrays in native
code. `(Tween :outline to ...)` does all of it.

Outlines come from `(outline-svg d)`, `(outline-text text font size)` and
`(outline-centred outline)`.

## How fast

On one node of an M4, against 16.6 ms for a frame at 60 a second. Drawing of small
figures is the least of it, a tick is all Lisp.

| | Tick | Draw |
|---|---|---|
| 200 small figures that collide | 1.8 ms | 0.3 ms |
| 500 | 6.5 ms | 0.6 ms |
| 1000 | 20 ms | 1.3 ms |
| 200 see through circles of radius 60, at 1280x960 | | 16 ms |

There is no farm of children. The Whiteboard draws in stripes for its thousands
of shapes, `lib/cwb/stripes.inc`, and if a scene here ever needs it, what a figure
draws comes only from its attributes.

## A window that keeps its shape

```vdu
(def *window* :aspect_view *picture* :aspect (list 640 480))
```

As the window is dragged the view stays that shape, `(window-aspect)` in
`gui/window/lisp.inc`. The app remakes its canvas on `+event_layout`.

## With the Whiteboard

Not joined up yet. A figure's outline is what `(cwb-outline)` gives of a shape,
and a component is data, so a `.cwb` document could name its components in its
reserved `:script` key and run on a rostrum. That is what `docs/ai_digest/whiteboard.md`
keeps the key for.

## What is not done

* A children's farm, see above.
* A native divide for tilted figures with a focus, if a scene needs it.
* A figure's collision polygons are those it draws, there is no separate hull.
