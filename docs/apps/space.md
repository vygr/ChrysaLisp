# Space

`Space`, in the demos, is a flight through a field of rocks in deep space,
drawn by the GPU through shaders written in ChrysaLisp's own shader language,
on the framework of Flight. Behind it all is the Pillars of Creation, as the
Hubble Space Telescope saw them, and it plays the user's Jukebox favourites
behind it.

## What is in it

* The sky: the Pillars of Creation in one direction, laid on the sky as a
  window of it is seen through, their nebula carried out past the picture's
  edges into the dark so it has no frame; the galaxy's band round all of it
  and a nebula of their colours on the far side; the stars, with Hubble's
  spikes on the bright ones; and the sun, a disc and the glow round it. All
  of it fixed in the sky as the eye turns.

* The rocks: 450 of them of eight shapes, each a field made into a mesh by a
  farm of children, lumps and ridges of noise, flat faces broken off and
  craters bitten out, a few hundred triangles each. What makes them rough is
  their shader: the moon's surface on them from three ways, finer as they
  come close, and a bump of a grain the size of the rock's own, there close
  to and fading out further off. Matt, lit by the sun, by the Pillars'
  nebula a quarter as bright and of its colour, and by the far nebula, and
  fading into the dark far off.

* The field: the rocks in clusters, a big one with smaller ones orbiting it,
  all drifting and tumbling, pulled to one another by their weight. Where
  two meet they crunch, not bounce: little comes back, the smaller is thrown
  the more, their faces grip as they grind past and spin each other, and
  grains of rock spray from the place they hit. A crunch is heard by how far off it is and from the side it is.
  A cluster out of sight is now and then sent off again, so the field stays
  alive.

* The buoys: markers in the field, most keeping station beside a cluster
  and a few in the open between. A buoy reads the rocks round it and
  flashes green, yellow or red, and the faster the greater the hazard, red
  when a rock passes close; one in the open is ice blue when calm; its flash lights the rocks round it in its
  colour; and its glow round its lamp. Polished metal that mirrors the sky
  round it as it is drawn, the nebula, the Pillars and the stars, and the
  sun. A buoy turns, slowly, when it must, to keep its lamp toward
  the ship's side of it. A cluster in sight with no buoy has one flown in
  to it from out of sight.

* The field keeps itself in hand: a buoy in a heap too crowded, 18 rocks
  or more round it look after look, and in view, arms, sounds a klaxon,
  flashes white fast for three seconds, and goes nuclear with a boom. Each small rock near it is shattered, grains
  thrown out, and sent off out of sight to drift back in alone, no more of
  its cluster; and its blast, far bigger than a missile's, throws the big
  ones out from it. So a heap that would slow the frames is broken up.

* The ship: a short sharp ping of white light every second or so, that
  lights the rocks close to.

* Missiles, to shepherd the rocks: a click fires one through where it is
  clicked, a missile with a hot nozzle and a trail, that goes off at a rock
  or at the end of its fuse. Its blast goes out as a front of pressure,
  throwing each rock it passes out from it, as hard as the pressure on its
  face, a small rock far and a big one little, and tumbling end over end
  away from it; the rock it strikes spun by where it is struck, hard off
  its middle and not at all through it, hit on hit spinning it up, a big
  rock the slower, as it is the heavier. A rock struck straight on enough
  times for its size breaks into two or three of the next size down, the
  pieces half in each other as they start and parting slowly, a spray of
  grains between them; the smallest shatter at the first hit. The pieces
  are rocks borrowed from out of sight, and the rock broken is sent off out
  of sight as a small one, so the field keeps its count and its sizes, and
  its frames. The blast flashes, lights the rocks round it before any
  buoy's light, throws out grains and is heard.

* The ship goes by thrust: thrust pushes it on, and drag slows it to a stop
  without, so it can stand and look. A rock blocks it, and it slides off.

* Two ways of flying. Auto pilot is a tour guide: of the headings round the
  way it wants that are safe it takes the one through the tightest gap,
  and it seeks out the big rocks ahead, one after another, not coming back
  to one it has been to for a while, and now and then turns round for the
  biggest cluster near. It burns as it needs, coasting between, turns
  before it goes, and bumped head on, turns away and goes for another.
  Mouse is steered by the pointer, straight where it points.

## Using it

* The toolbar: Auto pilot and Mouse; the song before, play or pause and the
  song after, of the user's Jukebox favourites, the ones it has kept.
* The window's max and min buttons: 1024 by 768 and 640 by 480.
* In Mouse mode the pointer is the stick: left and right of the middle to
  turn, above and below to climb and dive, gently near the middle and hard
  at the edge, off the picture to fly straight.
* A click on the picture fires a missile through where it is clicked, in
  either way of flying; three may be out at once.
* The right button held over the picture is thrust, in either way of
  flying: the ship speeds up, a roar rising with it, and when it is let go
  drag slows it, the roar dying away.

The way of flying, the size, the music and the detail are kept in the user's
`space.tre`.

## Detail

Each rock drawn is work for the app as well as the GPU, so less detail
draws the rocks less far, their fade into the dark coming in with them, and
the bump goes at the third level. A big rock is drawn the further, as it is
the more of the frame, and one 40 across or more all the way, so a big rock
in front never fades:

0. All of it.
1. The rocks drawn to 85% of the way.
2. No bump.
3. The rocks drawn to 70% of the way.
4. The rocks drawn to 55% of the way.

Space starts at the least the first time and goes up a level while it holds
58 frames a second or more, down a level while it is under 50, and keeps the
level it was at. A change of level eases the distance over a second or two,
so rocks go into the dark sooner, not at once. The status line says the
rate, how frames are drawn, and the detail if it is less than all of it.

A frame waits three times as long as the app took over the last one, at
least, so the app has a third of its node and what is beside it the rest.

## How it is made

`apps/demos/space/`:

* `app.lisp`: the window, the flight, the sounds and the frame.
* `field.inc`: the rocks and buoys and how they move, meet and are heard,
  and the way through them; no GUI, so it is tested on its own,
  `tests/math/test_space_field.lisp`.
* `rock.inc` and `child.lisp`: the fields of the rocks and the buoy, and the
  child of a `Jobs` farm that makes each a mesh.
* `sky.inc`: where the sun and the Pillars are in the sky, and their light.
* The shaders: `sky_lit`, with the library's `sky_vertex`; `rock_` and
  `grain_`, each a `_vertex` and a `_lit`; `buoy_lit` with `rock_vertex`;
  and `glow_`, the light round a buoy's lamp.
* `data/`: the pictures and sounds, and how each is made, its `README.md`.
  The pictures are each made by a bake of its own, `bake_nebula.lisp` and
  `bake_stars.lisp`; the sounds are recordings, credited there. A crunch is played as loud as its hit is
  heard. The roar
  of thrust is written to a stream of the audio service as the ship thrusts,
  a stretch of its loop each frame as loud as the thrust is.

The rocks meet in twelve parts, a part a frame, so each pair is met once in
twelve frames. A rock that has sunk into another by then is not jumped out
of it: its shove out is eased in over the frames till the pair meets again,
so a small rock resting on a big one does not judder. Touches are found through a fine grid, a sixteenth of the
field each way: a small rock looks in the cells round its own for the small
rocks it may touch, a big rock in the cells as far as its reach for the
small ones, and the big rocks, a few, each other. Pulls are through a
coarse grid, an eighth of the field each way, only of the rocks big enough
for their pull to be seen, and in one round of three, three times as much,
as a pull changes slowly. The rocks' pixel shader has one list of inputs for
all of them, so it is packed the once a frame; what is a rock's own goes in
its vertex shader's, and on to the pixel shader.

A frame is a list of draws given to a `(Render-tris)`,
`lib/gpu/render_farm.inc`, see `docs/ai_digest/render_farm.md`: the rocks
and buoys the eye sees, the sky after what hides it, and last what adds
light, the buoys' glows and the grains. A rock is drawn where it is nearest
the eye of all the places it is, the field going on every way. The auto
pilot looks at seven headings a frame, so its looking holds no frame up.

`+frame_log` in `app.lisp` logs each second its rate, its detail, the time a
frame of each part of the app's work and the GPU's, and the rocks drawn, for
tuning.
