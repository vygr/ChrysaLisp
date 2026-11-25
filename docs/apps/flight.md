# Flight

`Flight`, in the demos, is a flight over a land made as it goes, drawn by
the GPU through shaders written in ChrysaLisp's own shader language, with
craft not of this world about it. It is a show of what the shader language,
the render farm and the scene graph do together, and it plays the user's
Jukebox favourites behind it.

## What is in it

* The land: rolling hills of noise, raised in a vertex shader on a grid that
  is moved under the eye by whole cells, so nothing of it is made again; made
  rough by a bump, with shadows marched toward the sun, lakes with ripples
  that mirror the sky, sand by the water, rock where it is steep and snow on
  the tops, all of it going into the haze far off.

* The sky: a shader of its own behind everything, the sun's disc, a moon
  opposite the sun with NASA's map on it, stars by night, and a sheet of
  cloud that drifts in the wind and is seen through.

* The day: eleven minutes of it, day, dusk, a night under a full moon, and
  dawn. The light that lights the land, and casts the craft's shadows, goes
  from the sun to the moon and back. A slider under the toolbar is the time
  of day, and is dragged to any time of it.

* The craft: a scout, an orb, a blob, a triangle, a jellyfish and a top,
  each flying by its own rules, coming in out of the haze, keeping ahead of
  the flight, and racing past it. Their parts are fields made into meshes by
  a farm of children, few triangles each; what makes them is their shader,
  metal that mirrors the sky and land, a film of colours, seams and lamps
  drawn on. They light the land under them in their colours and shadow it.

* The snatch: now and then a craft stops in front of the flight over the
  land, snaps on a beam, a column of white light, and takes a cow up it,
  then is away at speed. Over the water it comes away with nothing.

* Three ways of flying: a figure of eight, along the valleys low like a jet,
  and by the pointer, which keeps clear of the land. A change of one to
  another is flown, not jumped.

## Using it

* The toolbar: Eight, Valleys and Mouse; the song before, play or pause and
  the song after, of the user's Jukebox favourites, the ones it has kept.
* The slider under it: the time of day.
* The window's max and min buttons: 1024 by 768 and 640 by 480.
* In Mouse mode the pointer is the stick: left and right of the middle to
  turn, above and below to climb and dive, off the picture to fly level.

The way of flying, the size, the music, the time of day and the detail are
kept in the user's `flight.tre`.

## Detail

Flight draws as much as the GPU keeps up with. It starts at the least the
first time and goes up a level while it holds 50 frames a second or more,
down a level while it is under 28, and keeps the level it was at:

0. All of it.
1. A coarser land and fewer steps toward the sun.
2. No shadow march and no bump.
3. No clouds.
4. Plain water and land, no stars.

The status line says the rate, how frames are drawn, and the detail if it is
less than all of it.

## How it is made

`apps/demos/flight/`:

* `app.lisp`: the window, the flight, the day and the frame.
* `fleet.inc`: the craft, each a scene of `lib/math/scene.inc`, and their
  ways of flying, their lights and beams.
* `craft.inc` and `child.lisp`: the fields of the craft's parts, and the
  child of a `Jobs` farm that makes each a mesh.
* `land.shader`: the land's height and the noise, shared by the shaders and
  called from Lisp, `(shader-vp-func)`, so a craft knows where the land is.
* The shaders, a pair for each kind of thing drawn: `land_`, `sky_`,
  `moon_`, `craft_`, `beam_` and `cloud_`, each a `_vertex` and a `_lit`.

A frame is a list of draws given to a `(Render-tris)`,
`lib/gpu/render_farm.inc`, each draw with its own pair of shaders, see
`docs/ai_digest/render_farm.md`: the land, the moon, the craft, the sky
after what hides it, and last a see-through draw, the clouds, and one that
adds light, a beam. The land at full detail is 64 tiles, each a mesh, and
only those the eye sees any of are drawn, `(land-seen)`; all of them have
the one list of inputs, which the render packs the once. A frame is given
when the last comes back, if that took a tick of the timer or more, and by
the tick if not. On a machine with no GPU the frame is drawn
by the nodes, a strip each. The music is a `Music` of `apps/media/jukebox/music.inc`,
the Jukebox's own player driven with no window.
