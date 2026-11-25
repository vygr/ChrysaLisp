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
  the more, both are set spinning, and grains of rock spray from the place
  they hit. A crunch is heard by how far off it is and from the side it is.
  A cluster out of sight is now and then sent off again, so the field stays
  alive.

* The buoys: markers in the field, most keeping station beside a cluster
  and a few in the open between. A buoy reads the rocks round it and
  flashes green, yellow or red, and the faster the greater the hazard, red
  when a rock passes close; its flash lights the rocks round it in its
  colour. Painted metal that mirrors the sky and the sun.

* The ship: a short sharp ping of white light every second or so, that
  lights the rocks close to.

* Two ways of flying, sharing what keeps clear of the rocks. Auto pilot is a
  tour guide: of the headings round the way it wants that are safe it takes
  the one through the tightest gap, and it seeks out the big rocks ahead,
  one after another, not coming back to one it has been to for a while.
  Mouse is steered by the pointer, the safe heading nearest the way it
  points taken.

## Using it

* The toolbar: Auto pilot and Mouse; the song before, play or pause and the
  song after, of the user's Jukebox favourites, the ones it has kept.
* The window's max and min buttons: 1024 by 768 and 640 by 480.
* In Mouse mode the pointer is the stick: left and right of the middle to
  turn, above and below to climb and dive, off the picture to fly straight.

The way of flying, the size, the music and the detail are kept in the user's
`space.tre`.

## Detail

Space starts with no bump on the rocks the first time, and puts it on while
the GPU holds 50 frames a second or more, takes it off while it is under 28,
and keeps what it was at. The status line says the rate, how frames are
drawn, and if the detail is less than all of it.

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
  Each is made by a bake of its own, `bake_nebula.lisp`, `bake_stars.lisp`,
  `bake_crunch.lisp` and `bake_levels.lisp`.

A frame is a list of draws given to a `(Render-tris)`,
`lib/gpu/render_farm.inc`, see `docs/ai_digest/render_farm.md`: the rocks
and buoys the eye sees, the sky after what hides it, and last what adds
light, the buoys' glows and the grains. A rock is drawn where it is nearest
the eye of all the places it is, the field going on every way. The rocks
meet in twelve parts, a part a frame, through a grid of where they are, and
the auto pilot looks at seven headings a frame, so neither holds a frame up.
